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
import Data.List
import Control.Applicative



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
takeAction (ChooseAnimal a) = flip (&~) $ do
  col <- use whoseTurn
  (player col . supply . animals . animalLens a) += 1
  
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
takeAction (StartBuilding HalfTimberedHouse ) = flip (&~) $ do
  id %= takeSpecialTile SpecialBuilding
  col <- use whoseTurn
  player col . supply . goodLens Stone -= 2
  player col . supply . goodLens Reed -= 1
  player col . supply . goodLens Wood -= 3

takeAction (StartBuilding Storage ) = flip (&~) $ do
  id %= takeSpecialTile SpecialBuilding
  col <- use whoseTurn
  player col . supply . goodLens Wood -= 2
  player col . supply . goodLens Reed -= 1

takeAction (StartBuilding Shelter ) = flip (&~) $ do
  id %= takeSpecialTile SpecialBuilding
  col <- use whoseTurn
  player col . supply . goodLens Stone -= 1
  player col . supply . goodLens Wood -= 2

takeAction (StartBuilding OpenStable ) = flip (&~) $ do
  id %= takeSpecialTile SpecialBuilding
  
takeAction (StartBuilding Stable ) = flip (&~) $ do
  id %= takeUnitTile BuildStable

takeAction (StartBuilding Stall ) = flip (&~) $ do
  id %= takeUnitTile BuildStall
  col <- use whoseTurn
  player col . supply . goodLens Stone -= 3
  player col . supply . goodLens Reed -= 1

takeAction (SpendResources good n) = flip (&~) $ do
  col <- use whoseTurn
  player col . supply . goodLens good -= n

takeAction (PlaceTrough cx cy) = flip (&~) $ do
  col <- use whoseTurn
  player col . farm . tile cx cy . trough .= True
  global . troughs -= 1
takeAction (PlaceBuilding b cx cy) = flip (&~) $ do
  col <- use whoseTurn
  player col . farm . tile cx cy . building .= Just b
  when (((isSpecialBuilding b) || b == Stall)) $ do
      global . globalBuildingLens b -= 1
      when (b == OpenStable) $ do
        global . stalls += 1
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
        bordsleft <- use (global . globalBorders)
        agri <- use id
        hasPlacedWorker .= False
        if bordsleft == 0
          then
            do phase .= Finished
               message .= finalScore agri
               whoseTurn .= No
          else
            do whoseTurn .= starter
               red . workers .= 3
               blue . workers .= 3
               global . globalBorders -= 1
               board %= takeWorkers
               board %= refillBoard
               phase .= WorkPhase
    WorkPhase -> do
      id %= breedAnimals
      phase .= BreedingPhase
      whoseTurn .= starter
      hasPlacedWorker .= True
    Finished -> id %= id


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
takeAction (PlaceExpand isLeftSide) = flip (&~) $ do
  col <- use whoseTurn
  expssofar <- numberOfExpansions col <$> use id
  let  newts = replicate 3  emptyTile {_expansion = Just (expssofar + 1)}
  global . expansions -= 1
  player col . farm . tiles %= addTo isLeftSide newts
  player col . farm . vborders %= addTo isLeftSide newvs
  player col . farm . hborders %= addTo isLeftSide newhs
  where newvs = replicate 3 (Border V False)
        newhs = replicate 4 (Border H False)
        addTo True new old = transpose $ new : transpose old
        addTo False new old = transpose $ transpose old ++ [new]
takeAction TakeExpand = takeMonoTile Expand Fence



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

takeSpecialTile gbtile agri = agri &~ do
  (a,Nothing) <- use (board . specialTileLens gbtile)
  col <- use whoseTurn
  case a of
    Just c -> board . specialTileLens gbtile .= (Just c, Just col)
    Nothing -> board . specialTileLens gbtile .= (Just col,Nothing)
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
boardSpaceFree agri (StartBuilding Stall) = isNothing (agri ^. board . buildStall)
boardSpaceFree agri (StartBuilding Stable) = isNothing (agri ^. board . buildStable)
boardSpaceFree agri (StartBuilding b ) | isSpecialBuilding b =
  case agri ^. board . specialBuilding of
  (Just _, Just _) -> False
  _ -> True


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
                ++ [StartBuilding b
                   | b <- [Stable, OpenStable, HalfTimberedHouse , Storage , Shelter, Stall]]

isProblem :: Agricola -> Action ->  Maybe String
isProblem agri (SetMessage _) = Nothing
isProblem agri (ChooseAnimal _) = Nothing
isProblem agri (PlaceExpand _) = if (agri ^. global . expansions) <= 0
                                    then Just "there are no expansion left in the global supply."
                                    else Nothing
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
          else Just "there is already a border there"
     else Just "you don't have enough borders"
  where col = agri ^. whoseTurn

isProblem agri (TakeAnimal cx cy) = if not (hasAnimals agri cx cy)
                                    then Just "there is no animal on that tile."
                                    else Nothing

isProblem agri (PlaceAnimal ani cx cy) = if (agri ^. (player col . supply . animals . animalLens ani) == 0)
                                         then Just $ "there is no " ++ map toLower (show ani) ++ " to place"
                                         else case (isSameAnimal agri col c ani) of
                                           Just an -> Just $  "there is already a " ++ map toLower (show an) ++ " there"
                                           Nothing -> if animalSpace agri c <= 0
                                                      then Just "there is not enough room there"
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
isProblem agri (PlaceTrough cx cy) =
  if (agri ^. player col . farm . tile cx cy. trough)
  then Just "there is already a trough on that tile"
  else
    if (agri ^. global . troughs <= 0)
    then Just "there are no troughs left in the game components"
    else Nothing
  where col = agri ^. whoseTurn
isProblem agri (PlaceBuilding Stable cx cy) =
  if (agri ^. player col . farm . tile cx cy. building) /= Just Stall
  then Just "there is no stall on that tile"
  else Nothing
  where col = agri ^. whoseTurn
isProblem agri (PlaceBuilding OpenStable cx cy) =
  if (agri ^. player col . farm . tile cx cy. building) /= Just Stall
  then Just "there is no stall there"
  else Nothing
  where col = agri ^. whoseTurn
isProblem agri (PlaceBuilding HalfTimberedHouse cx cy) =
  if (agri ^. player col . farm . tile cx cy. building) /= Just Cottage
  then Just "there is no cottage there "
  else Nothing
  where col = agri ^. whoseTurn
isProblem agri (PlaceBuilding b cx cy) =
  if (isJust (agri ^. player col . farm . tile cx cy. building))
  then Just "there is already a building on that tile"
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


buildingResourceProblem :: Building -> Agricola -> Maybe String
buildingResourceProblem HalfTimberedHouse agri =
  resourceProblem [(Wood, 3), (Stone, 2), (Reed,1)] agri
buildingResourceProblem Storage agri =
  resourceProblem [(Wood, 2), (Reed,1)] agri
buildingResourceProblem Shelter agri =
  resourceProblem [(Wood, 2), (Stone,1)] agri
buildingResourceProblem OpenStable agri =
  case resourceProblem [(Stone,3)] agri of
  Nothing -> Nothing
  Just a -> resourceProblem [(Wood,3)] agri
buildingResourceProblem Stall agri =
  resourceProblem [(Stone,3),(Reed,1)]  agri
buildingResourceProblem Stable agri =
  case resourceProblem [(Stone, 5)] agri of
  Nothing -> Nothing
  Just a -> resourceProblem [(Wood,5)] agri


isSpecialBuilding :: Building -> Bool
isSpecialBuilding HalfTimberedHouse = True
isSpecialBuilding Storage = True
isSpecialBuilding Shelter = True
isSpecialBuilding OpenStable = True
isSpecialBuilding _ = False

isInSupply :: Building -> Agricola -> Maybe String
isInSupply Stable agri = Nothing
isInSupply b agri =
  if (agri ^. global . globalBuildingLens b ) <= 0
  then Just $ "all available " ++ show b ++ " have been built already"
  else Nothing

isResourceProblem :: Action -> Agricola -> Maybe String
isResourceProblem StartBuildingWoodFences = resourceProblem [(Wood,1)]
isResourceProblem (StartBuilding b) = \agri ->
  isInSupply b agri <|> buildingResourceProblem b agri
isResourceProblem _ = const Nothing

resourceProblem :: [(Good,Integer)] -> Agricola -> Maybe String
resourceProblem [] agri = Nothing
resourceProblem ((good,n):goods) agri =
  if agri ^. (player col . supply . goodLens good) < n
     then return $ "there is not enough " ++ show good
     else resourceProblem goods agri
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

buildingCapacity :: Building -> Integer
buildingCapacity Cottage = 1
buildingCapacity Stall = 3
buildingCapacity Stable = 5
buildingCapacity OpenStable = 5
buildingCapacity HalfTimberedHouse = 2
buildingCapacity Storage = 2
buildingCapacity Shelter = 1

animalCapacity :: Agricola -> Coord -> Integer
animalCapacity agri c@(cx,cy) =
  case (agri ^. player col . farm . tile cx cy . building) of
  Just b -> (buildingCapacity b)*(2^t)
  Nothing -> if (isEnclosed agri c)
             then  2 ^ (1 + troughNum agri c)
             else t
  where col = agri ^. whoseTurn
        t   = hasTrough agri c

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


bonusScore ::  Color -> Agricola -> Rational
bonusScore col agri = buildingscore + expansionscore
  where pts = agri ^. player col . farm . tiles
        builds = mapMaybe _building $ concat pts
        buildingscore = sum $ map (scoreBuilding agri col) builds
        expansionscore = 0


scoreBuilding ::  Agricola -> Color -> Building -> Rational
scoreBuilding _ _ Stall = 1
scoreBuilding _ _ Stable = 4
scoreBuilding _ _ Cottage = 0
scoreBuilding _ _ HalfTimberedHouse = 5
scoreBuilding agri col Storage = (fromInteger goodcount) / 2
  where sup = agri ^. player col . supply
        goodcount = sum (map (\g -> sup ^. goodLens g) [Wood, Stone, Reed])
scoreBuilding _ _ Shelter = 0
scoreBuilding _ _ OpenStable = 2

scoreAnimal :: Animal -> Integer -> Integer
scoreAnimal Sheep n | n >= 13 =  n + (n - 13) + 3
scoreAnimal Sheep n | n >= 11 =  n + 2
scoreAnimal Sheep n | n >= 8 =  n + 1

scoreAnimal Pig n   | n >= 11  = n + (n - 11) + 3
scoreAnimal Pig n   | n >= 9  = n + 2
scoreAnimal Pig n   | n >= 7  = n + 1

scoreAnimal Cow n   | n >= 10 = n + (n - 10)  + 3
scoreAnimal Cow n   | n >= 8  = n + 2
scoreAnimal Cow n   | n >= 6  = n + 1

scoreAnimal Horse n   | n >= 9 = n + (n - 9)  + 3
scoreAnimal Horse n   | n >= 7  = n + 2
scoreAnimal Horse n   | n >= 5  = n + 1

scoreAnimal a n | n <= 3 = -3
scoreAnimal a n = n

animalScore :: Color -> Agricola -> Rational
animalScore col agri = toRational $ sum [ scoreAnimal Sheep sh
                                        , scoreAnimal Pig  pi
                                        , scoreAnimal Cow co
                                        , scoreAnimal Horse ho
                            ]
  where pts = agri ^. player col . farm . tiles
        ans = mapMaybe _tileanimals $ concat pts
        sh = sum $ map snd $ filter (\x -> fst x == Sheep) ans
        pi = sum $ map snd $ filter (\x -> fst x == Pig) ans
        co = sum $ map snd $ filter (\x -> fst x == Cow) ans
        ho = sum $ map snd $ filter (\x -> fst x == Horse) ans

finalPlayerScore :: Color -> Agricola -> Rational
finalPlayerScore col agri = animalScore col agri + bonusScore col agri

finalScore :: Agricola -> String
finalScore agri = final
  where reds = finalPlayerScore Red agri
        blues = finalPlayerScore Blue agri
        winner = if reds > blues then Red else Blue
        loser = otherColor winner
        winners = maximum [reds, blues]
        losers = minimum [reds, blues]
        final = unwords [ show winner
                        , "wins with a score of"
                        , show winners
                        , "while"
                        , show loser
                        , "only got a score of"
                        , show losers
                        ]

startingState :: Agricola
startingState = emptyAgricola &~ do
  player Blue %=  initPlayer
  player Red  %=  initPlayer
  board .= emptyBoard
  global .= startingGlobalSupply
  phase .= BreedingPhase
  id %= takeAction EndPhase
