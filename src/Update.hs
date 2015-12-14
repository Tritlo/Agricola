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

placeBorder :: Agricola -> Alignment -> Integer -> Integer -> Agricola
placeBorder agri al n m = agri &~ do
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
breedAnimals agri = agri &~ do
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

takeAction :: Agricola -> Action -> Agricola
takeAction agri StartBuildingTroughs = agri &~ do
  col <- use whoseTurn
  board . buildTroughs .= Just col
  subtractWorker
takeAction agri StartBuildingStoneWalls = agri &~ do
  col <- use whoseTurn
  board . stoneWall .= Just col
  subtractWorker
takeAction agri StartBuildingWoodFences = agri &~ do
  col <- use whoseTurn
  board . woodFence .= Just col
  subtractWorker
takeAction agri DoNothing = agri
takeAction agri (SetMessage msg) = agri & message .~  msg
takeAction agri (PlaceBorder al cx cy) = placeBorder agri al cx cy
takeAction agri (PlaceAnimal ani cx cy) = placeAnimal agri (cx,cy) ani
takeAction agri (SpendResources good n) = agri &~ do
  col <- use whoseTurn
  player col . supply . goodLens good -= n
takeAction agri (PlaceTrough cx cy) = agri &~ do
  col <- use whoseTurn
  player col . farm . tile cx cy . trough .= True
takeAction agri (FreeAnimal an) = agri &~ do
  col <- use whoseTurn
  player col . supply . animals . animalLens an -= 1

takeAction agri EndTurn = agri &~ do
  whoseTurn %= otherColor
  curphase <- use phase
  when (curphase == WorkPhase) $ hasPlacedWorker .= False

takeAction agri EndPhase = agri &~ do
  curphase <- use phase
  case curphase of
    BreedingPhase -> do
        whoseTurn .= (agri ^. starting)
        hasPlacedWorker .= False
        red . workers .= 3
        blue . workers .= 3
        board %= takeWorkers
        board %= refillBoard
        phase .= WorkPhase
    WorkPhase -> do
      id %= breedAnimals 
      phase .= BreedingPhase
      whoseTurn .= (agri ^. starting)
      hasPlacedWorker .= True

takeAction agri TakeResources = agri &~ do
  col <- use whoseTurn
  player col . supply . wood += 1
  player col . supply . reeds += 1
  player col . supply . stones += 1
  id %= takeUnitTile Resources


takeAction agri TakeMillpond =
  takeDuoTile Millpond (goodLens Reed) (animals . sheep) agri
takeAction agri TakePigsAndSheep =
  takeDuoTile PigsAndSheep (animals . pigs) (animals . sheep) agri
takeAction agri TakeCowsAndPigs =
  takeDuoTile CowsAndPigs (animals . cows) (animals . pigs) agri
takeAction agri TakeHorsesAndSheep =
  takeDuoTile HorsesAndSheep (animals .  horses) (animals . sheep) agri


takeAction agri TakeSmallForest = agri &~ do
  id %= takeMonoTile SmallForest Wood
  playerColor <- use whoseTurn
  starting .= playerColor

takeAction agri TakeBigForest   = takeMonoTile BigForest Wood agri
takeAction agri TakeBigQuarry   = takeMonoTile BigQuarry Stone agri
takeAction agri TakeSmallQuarry = takeMonoTile SmallQuarry Stone agri


takeAction agri (TakeAnimal cx cy) = agri &~ do
  playerColor <- use whoseTurn
  Just (ani,n) <- use (player playerColor . farm . tile cx cy .tileanimals)
  player playerColor . supply . animals . animalLens ani += 1
  if n == 1
    then player playerColor . farm . tile cx cy .tileanimals .= Nothing
    else player playerColor . farm . tile cx cy .tileanimals .= Just (ani,n-1)

takeAction _ a = error $ "Cannot find action for " ++ show a

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
    return $ takeAction newagri action
  Just err -> return $ agri & message .~ ("Cannot " ++ show action ++ ", " ++ err)


tryTakeMultiAction :: Agricola -> [Action] -> Either Agricola String
tryTakeMultiAction agri [] = Left agri
tryTakeMultiAction agri (a:as) = case isProblem agri a of
  Nothing ->  tryTakeMultiAction (takeAction agri a) as
  Just err -> Right err

update :: Agricola -> Maybe Action -> Maybe Agricola
update agri action = action >>= tryTakeAction agri

placeAnimal :: Agricola -> Coord -> Animal -> Agricola
placeAnimal agri (cx,cy) animal = agri &~ do
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
                                     else Nothing
isProblem agri a = error $ "did not find legal for " ++ show a


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
