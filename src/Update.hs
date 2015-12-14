{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Update where

import Agricola
import UI.NCurses (Event)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char (toLower)

placeBorder ::  Alignment -> Integer -> Integer  -> Agricola -> Agricola
placeBorder al n m = flip (&~) $ do
  color <- use whoseTurn
  (player color . supply . borders) -= 1
  (player color . farm . border al n m) .= Border al True


hasBorders :: Agricola -> Color -> Bool
hasBorders agri color = agri ^. (player color . supply . borders) >= 1

freeSpace :: Agricola -> Color -> Alignment -> Integer -> Integer -> Bool
freeSpace agri color al n m = not $ agri ^.
                              (player color . farm . border al n m . isThere )

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

addAnimalSupply :: Animals -> Animals -> Animals
addAnimalSupply (Animals as ap ac ah) (Animals bs bp bc bh) =
  Animals (as + bs) (ap+bp) (ac+bc) (ah+bh)

addSupply :: Supply -> Supply -> Supply
addSupply (Supply ab aw as ar aa) (Supply bb bw bs br ba) =
  Supply (ab+bb) (aw+bw) (as+bs) (ar+br) (aa `addAnimalSupply` ba)

takeResources :: Agricola -> Supply -> Agricola
takeResources agri sup = agri & player col . supply %~ addSupply sup
  where col = agri ^. whoseTurn


subtractWorker :: StateT Agricola Identity ()
subtractWorker = do
  playerColor <- use whoseTurn
  (player playerColor . workers) -= 1
  hasPlacedWorker .= True


breedAnimals :: Agricola -> Agricola
breedAnimals = flip (&~) $ do
  id %= breedAnimals' Red
  id %= breedAnimals'  Blue


countAnimalInTiles :: Animal -> [[Tile]] -> Integer
countAnimalInTiles an ts = sum (map countInRow ts)
  where countInRow r = sum (map countInTile r)
        countInTile t = case _tileanimals t of
          Nothing -> 0
          Just (b, n) -> if an == b then n else 0


countAnimal :: Agricola -> Color -> Animal -> Integer
countAnimal agri col animal = countAnimalInTiles animal ts
  where ts = agri ^. (player col . farm . tiles)


breedAnimals' :: Color -> Agricola -> Agricola
breedAnimals' col agri = agri &~ do
  id %= breedAnimal col Sheep
  id %= breedAnimal col Pig
  id %= breedAnimal col Cow
  id %= breedAnimal col Horse


breedAnimal ::  Color -> Animal -> Agricola -> Agricola
breedAnimal col an agri = agri &~ when (countAnimal agri col an >= 2)
                          (player col . supply . animals . animalLens an += 1)

takeAction :: Action -> Agricola -> Agricola
takeAction StartBuildingTroughs = takeUnitTile BuildTroughs
takeAction StartBuildingStoneWalls = takeUnitTile StoneWall
takeAction StartBuildingWoodFences = flip (&~) $ do
  col <- use whoseTurn
  id %= takeUnitTile WoodFence
  player col . supply . goodLens Wood -= 1
takeAction DoNothing = id
takeAction (SetMessage msg) = set message msg
takeAction (PlaceBorder al cx cy) = placeBorder al cx cy
takeAction (PlaceAnimal ani cx cy) = placeAnimal (cx,cy) ani
takeAction (SpendResources good n) = flip (&~) $ do
  col <- use whoseTurn
  player col . supply . goodLens good -= n

takeAction (PlaceTrough cx cy) = flip (&~) $ do
  col <- use whoseTurn
  player col . farm . tile cx cy . trough .= True
takeAction (FreeAnimal an) = flip (&~) $ do
  col <- use whoseTurn
  player col . supply . animals . animalLens an -= 1

takeAction EndTurn = flip (&~) $ do
  whoseTurn %= otherColor
  curphase <- use phase
  when (curphase == WorkPhase) $ hasPlacedWorker .= False

takeAction EndPhase = flip (&~) $ do
  curphase <- use phase
  starter <- use starting
  case curphase of
    BreedingPhase -> do
        whoseTurn .= starter
        hasPlacedWorker .= False
        red . workers .= 3
        blue . workers .= 3
        board %= takeWorkers
        board %= refillBoard
        phase .= WorkPhase
    WorkPhase -> do
      id %= breedAnimals
      phase .= BreedingPhase
      whoseTurn .= starter
      hasPlacedWorker .= True

takeAction TakeResources = flip (&~) $ do
  col <- use whoseTurn
  player col . supply . wood += 1
  player col . supply . reeds += 1
  player col . supply . stones += 1
  id %= takeUnitTile Resources


takeAction TakeMillpond = takeDuoTile Millpond (goodLens Reed) (animals . sheep)
takeAction TakePigsAndSheep = takeDuoTile PigsAndSheep (animals . pigs) (animals . sheep)
takeAction TakeCowsAndPigs = takeDuoTile CowsAndPigs (animals . cows) (animals . pigs)
takeAction TakeHorsesAndSheep = takeDuoTile HorsesAndSheep (animals .  horses) (animals . sheep)


takeAction TakeSmallForest = flip (&~) $ do
  id %= takeMonoTile SmallForest Wood
  playerColor <- use whoseTurn
  starting .= playerColor

takeAction TakeBigForest   = takeMonoTile BigForest Wood
takeAction TakeBigQuarry   = takeMonoTile BigQuarry Stone
takeAction TakeSmallQuarry = takeMonoTile SmallQuarry Stone


takeAction (TakeAnimal cx cy) = flip (&~) $ do
  playerColor <- use whoseTurn
  Just (ani,n) <- use (player playerColor . farm . tile cx cy .tileanimals)
  player playerColor . supply . animals . animalLens ani += 1
  if n == 1
    then player playerColor . farm . tile cx cy .tileanimals .= Nothing
    else player playerColor . farm . tile cx cy .tileanimals .= Just (ani,n-1)

takeAction a = const $ error $ "Cannot find action for " ++ show a

takeUnitTile gbtile agri = agri &~ do
  col <- use whoseTurn
  board . (unitTileLens gbtile) .= Just col
  subtractWorker

takeDuoTile gbtile fspath snpath agri = agri &~ do
  Right (fs,sn) <- use (board . duoTileLens gbtile )
  col <- use whoseTurn
  player col . supply . fspath += fs
  player col . supply  . snpath += sn
  board . duoTileLens gbtile .= Left col
  subtractWorker


takeMonoTile gbtile good agri = agri &~ do
  Right r <- use (board . monoTileLens gbtile)
  col <- use whoseTurn
  player col . supply . goodLens good += r
  board . monoTileLens gbtile .= Left col
  subtractWorker


tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri (MultiAction actions) =
  case tryTakeMultiAction agri actions of
  Left nagri -> return $ nagri & message .~ ""
  Right err -> return $ agri & message .~ ("Cannot do all of "
                                           ++ concatMap ((++ ", ") . show ) actions
                                           ++ "since "  ++ err)

tryTakeAction agri action =
  case isProblem agri action of
  Nothing -> let newagri = agri & message .~ "" in
    return $ takeAction action newagri
  Just err -> return $ agri & message .~ ("Cannot " ++ show action ++ ", " ++ err)


tryTakeMultiAction :: Agricola -> [Action] -> Either Agricola String
tryTakeMultiAction agri [] = Left agri
tryTakeMultiAction agri (a:as) = case isProblem agri a of
  Nothing ->  tryTakeMultiAction (takeAction a agri) as
  Just err -> Right err

update :: Agricola -> Maybe Action -> Maybe Agricola
update agri action = action >>= tryTakeAction agri

placeAnimal ::  Coord -> Animal -> Agricola ->  Agricola
placeAnimal (cx,cy) animal = flip (&~) $ do
  colr <- use whoseTurn
  player colr . supply . animals . animalLens animal -= 1
  player colr . farm . tile cx cy . tileanimals %= addAnimal animal


hasWorkers :: Agricola -> Bool
hasWorkers agri = agri ^. (player (agri ^. whoseTurn) . workers) > 0

hasAnimals :: Agricola -> Integer -> Integer -> Bool
hasAnimals agri cx cy = let col = agri ^. whoseTurn in
  isJust (agri ^. (player col . farm . tile cx cy . tileanimals))

hasAnimalsInSupply agri =
     agri ^. player col . supply . animals . animalLens Sheep > 0
  || agri ^. player col . supply . animals . animalLens Cow > 0
  || agri ^. player col . supply . animals . animalLens Horse > 0
  || agri ^. player col . supply . animals . animalLens Pig > 0
  where col = agri ^. whoseTurn

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft a = not $ isRight a

boardSpaceFree :: Agricola -> Action -> Bool
boardSpaceFree agri TakeResources      = isNothing (agri ^. board . resources)
boardSpaceFree agri TakeSmallForest    = isRight (agri ^. board . smallForest)
boardSpaceFree agri TakeBigForest      = isRight (agri ^. board . bigForest)
boardSpaceFree agri TakeSmallQuarry    = isRight (agri ^. board . smallQuarry)
boardSpaceFree agri TakeBigQuarry      = isRight (agri ^. board . bigQuarry)
boardSpaceFree agri TakeExpand         = isRight (agri ^. board . expand)
boardSpaceFree agri TakeMillpond       = isRight (agri ^. board . millpond)
boardSpaceFree agri TakePigsAndSheep   = isRight (agri ^. board . pigsAndSheep)
boardSpaceFree agri TakeCowsAndPigs    = isRight (agri ^. board . cowsAndPigs)
boardSpaceFree agri TakeHorsesAndSheep = isRight (agri ^. board . horsesAndSheep)
boardSpaceFree agri StartBuildingTroughs = isNothing (agri ^. board . buildTroughs)
boardSpaceFree agri StartBuildingStoneWalls = isNothing (agri ^. board . stoneWall)
boardSpaceFree agri StartBuildingWoodFences = isNothing (agri ^. board . woodFence)

workerActions :: [Action]
workerActions = [ TakeResources
                , TakeSmallForest
                , TakeBigForest
                , TakeSmallQuarry
                , TakeBigQuarry
                , TakeExpand
                , TakeMillpond
                , TakePigsAndSheep
                , TakeCowsAndPigs
                , TakeHorsesAndSheep
                , StartBuildingTroughs
                , StartBuildingStoneWalls
                , StartBuildingWoodFences
                ]

isProblem :: Agricola -> Action ->  Maybe String
isProblem agri (SetMessage _) = Nothing
isProblem agri EndTurn = if hasWorkers agri && not (agri ^. hasPlacedWorker)
                         then Just $ show col ++ " has to place worker"
                         else if hasAnimalsInSupply agri
                              then Just $ show col ++ " has unplaced animals, free them or place 'em"
                              else Nothing
  where col = agri ^. whoseTurn
isProblem agri EndPhase =
  case isProblem agri EndTurn of
  Just s -> Just s
  Nothing -> case isProblem (agri & whoseTurn %~ otherColor) EndTurn of
    Just s -> Just s
    Nothing -> if (agri ^. (red . workers) > 0) || (agri ^. (blue . workers) > 0)
               then Just "some players have unplaced workers"
               else Nothing
isProblem _ DoNothing = Nothing

isProblem agri (PlaceBorder al cx cy) =
  if hasBorders agri col 
     then if freeSpace agri col al cx cy
          then Nothing
          else Just "because there is already a border there"
     else Just "because you don't have enough borders"
  where col = agri ^. whoseTurn

isProblem agri (TakeAnimal cx cy) = if not (hasAnimals agri cx cy)
                                    then Just "since there is no animal on that tile."
                                    else Nothing

isProblem agri (PlaceAnimal ani cx cy) = if (agri ^. (player col . supply . animals . animalLens ani) == 0)
                                         then Just $ "because there is no " ++ map toLower (show ani) ++ " to place"
                                         else case (isSameAnimal agri col c ani) of
                                           Just an -> Just $  "because there is already a " ++ map toLower (show an) ++ " there"
                                           Nothing -> if animalSpace agri c <= 0
                                                      then Just "because there is not enough room there"
                                                      else Nothing
                                          where col = agri ^. whoseTurn
                                                c = (cx,cy)
isProblem agri (FreeAnimal an) =
  if agri ^. (player col . supply . animals . animalLens an) == 0
     then Just "you have none in your supply"
     else Nothing
  where col = agri ^. whoseTurn
isProblem agri (SpendResources good n) =
  if agri ^. (player col . supply . goodLens good) < n
     then Just ("you do not have enough " ++ map toLower (show good) ++ " in your supply")
     else Nothing
  where col = agri ^. whoseTurn
isProblem agri (PlaceTrough cx cy) = if (agri ^. player col . farm . tile cx cy. trough)
                                        then Just "because there is already a trough on that tile"
                                        else Nothing
                                      where col = agri ^. whoseTurn
isProblem agri action | action `elem` workerActions =
                        if not (hasWorkers agri)
                        then return "no workers available"
                        else if not (boardSpaceFree  agri action)
                             then return "board space occupied"
                             else if agri ^. hasPlacedWorker
                                     then return "worker already placed"
                                     else isResourceProblem action agri
isProblem agri a = error $ "did not find legal for " ++ show a


isResourceProblem :: Action -> Agricola -> Maybe String
isResourceProblem StartBuildingWoodFences = resourceProblem Wood 1
isResourceProblem _ = const Nothing

resourceProblem :: Good -> Integer -> Agricola -> Maybe String
resourceProblem good n agri =
  if agri ^. (player col . supply . goodLens good) < n
     then return $ "there is not enough " ++ show good
     else Nothing
  where col = agri ^. whoseTurn


addAnimal a Nothing      = Just (a,1)
addAnimal a (Just (b,n)) | a == b = Just (a,n+1)
addAnimal _ _            = error "Cannot add different animal"



isSameAnimal :: Agricola -> Color -> Coord -> Animal -> Maybe Animal
isSameAnimal agri colr (cx,cy) an =
  let tileAn = agri ^. player colr . farm . tile cx cy . tileanimals in
      if (isNothing tileAn || (fst $ fromJust tileAn) == an)
      then Nothing
      else Just $ (fst $ fromJust tileAn)



data Direction = N | S | E | W deriving (Eq)

allDirections :: [Direction]
allDirections = [N,S,E,W]

isOutOfBounds :: [[a]] -> Coord -> Bool
isOutOfBounds grid c@(cn,cm) = cn < 0
                               || cn >= toInteger (length grid)
                               || cm < 0
                               || cm >= toInteger (maximum $ map length grid)

enclosedWith :: Farm -> Coord -> [Coord]
enclosedWith farm coord = enclosedWith' [] [coord]
  where
    enclosedWith' visited []  =  visited
    enclosedWith' visited (v:tovisit) | isOutOfBounds (farm ^. tiles) v = []
    enclosedWith' visited (v:tovisit) =  enclosedWith' (v:visited) $
                                         filter
                                           (`notElem` (v:visited))
                                           (tovisit ++  toVisit v)
    toVisit c@(cn,cm) = [ coordInDirection c d |
                          d <- allDirections,
                          not $ hasBorder farm c d,
                          not $ hasBuilding farm c d]


isEnclosed :: Agricola -> Coord -> Bool
isEnclosed agri c = not $ null $ enclosedWith f c
  where col = agri ^. whoseTurn
        f = agri ^. (player col . farm)


-- testFarm = emptyFarm &~ do
--   border H 0 0 . isThere .= True
--   border H 1 0 . isThere .= True
--   border H 0 1 . isThere .= True
--   border H 1 1 . isThere .= True
--   border V 0 0 . isThere .= True
--   border V 0 2 . isThere .= True

-- testFarm2 = startingFarm &~ do
--   border H 1 0 . isThere .= True
--   border V 1 0 . isThere .= True
--   border V 1 1 . isThere .= True


coordInDirection :: Coord -> Direction -> Coord
coordInDirection (cn,cm) N = ( cn - 1, cm)
coordInDirection (cn,cm) S = ( cn + 1, cm)
coordInDirection (cn,cm) W = ( cn , cm -1)
coordInDirection (cn,cm) E = ( cn , cm +1)

hasBuilding :: Farm -> Coord -> Direction -> Bool
hasBuilding farm c@(cx,cy) d = not (isOutOfBounds (farm ^. tiles) n)
                               && isJust (farm ^. tile nx ny . building)
  where n@(nx,ny) = coordInDirection c d

hasBorder:: Farm -> Coord -> Direction -> Bool
hasBorder farm (cn,cm) N = farm ^. (border H cn cm . isThere)
hasBorder farm (cn,cm) S = farm ^. (border H (cn +1) cm. isThere) 
hasBorder farm (cn,cm) W = farm ^. (border V cn cm . isThere)
hasBorder farm (cn,cm) E = farm ^. (border V cn (cm + 1) . isThere)

animalCapacity :: Agricola -> Coord -> Integer
animalCapacity agri c | not (isEnclosed agri c) = hasTrough agri c
                      | isEnclosed agri c = 2 ^ (1 + troughNum agri c)
  where col = agri ^. whoseTurn

animalSpace :: Agricola -> Coord -> Integer
animalSpace agri c@(cx,cy) | isNothing tileAn = animalCapacity agri c
                           | otherwise = animalCapacity agri c - snd (fromJust tileAn)
    where
      col = agri ^. whoseTurn
      tileAn = agri ^. player col . farm . tile cx cy . tileanimals


coordToTile :: Agricola -> Coord -> Tile
coordToTile agri c@(cx,cy) = f ^. tile cx cy
  where col = agri ^. whoseTurn
        f = agri ^. (player col . farm)

troughNum :: Agricola -> Coord -> Integer
troughNum agri coord = toInteger $ length $ filter hast encw
  where col = agri ^. whoseTurn
        f = agri ^. (player col . farm)
        encw = enclosedWith f coord
        hast c@(cx,cy) = f ^. tile cx cy . trough

hasTrough :: Agricola -> Coord -> Integer
hasTrough agri (cx,cy) | agri ^. player col . farm . tile cx cy . trough = 1
                       | otherwise = 0
  where col = agri ^. whoseTurn
