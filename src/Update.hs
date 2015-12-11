{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Update where

import Agricola
import UI.NCurses (Event)
import Input
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe

placeBorder :: Agricola -> Alignment -> Integer -> Integer -> Agricola
placeBorder agri al n m = agri &~ do
  color <- use whoseTurn
  (player color . supply . borders) -= 1
  (player color . farm . border al n m) .= Border al True

canPlaceBorder :: Agricola -> Alignment -> Integer -> Integer -> Color -> Bool
canPlaceBorder agri al n m  color = hasBorders agri color
                                    && freeSpace agri color al n m

hasBorders :: Agricola -> Color -> Bool
hasBorders agri color = agri ^. (player color . supply . borders) >= 1

freeSpace :: Agricola -> Color -> Alignment -> Integer -> Integer -> Bool
freeSpace agri color al n m = not $ agri ^.
                              (player color . farm . border al n m . isThere )

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

tryPlaceBorder :: Agricola -> Alignment -> Integer -> Integer ->  Maybe Agricola
tryPlaceBorder agri al n m =
  if
    canPlaceBorder agri al n m (agri ^. whoseTurn)
  then return $ placeBorder agri al n m
  else return agri

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
takeAction agri DoNothing = agri
takeAction agri (PlaceBorder al cx cy) = placeBorder agri al cy cx
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
  id %= flip takeResources (Supply 0 1 1 1 emptyAnimals)
  board . resources .= Nothing
  subtractWorker

takeAction agri TakeMillpond = agri &~ do
  Just (rs,sh) <- use (board . millpond )
  id %= flip takeResources (emptySupply { _animals = emptyAnimals { _sheep = sh}, _reeds = rs} )
  board . millpond .= Nothing
  subtractWorker

takeAction agri TakePigsAndSheep = agri &~ do
  Just (ps,sh) <- use (board . pigsAndSheep )
  id %= flip takeResources (emptySupply { _animals = emptyAnimals { _sheep = sh, _pigs = ps}})
  board . pigsAndSheep .= Nothing
  subtractWorker

takeAction agri TakeCowsAndPigs = agri &~ do
  Just (cs,ps) <- use (board . cowsAndPigs )
  id %= flip takeResources (emptySupply { _animals = emptyAnimals { _cows = cs, _pigs = ps}})
  board . cowsAndPigs .= Nothing
  subtractWorker

takeAction agri TakeHorsesAndSheep = agri &~ do
  Just (hs,sh) <- use (board . horsesAndSheep )
  id %= flip takeResources (emptySupply { _animals = emptyAnimals { _sheep = sh, _horses = hs}})
  board . horsesAndSheep .= Nothing
  subtractWorker

takeAction agri TakeSmallForest = agri &~ do
  Just w <- use (board . smallForest)
  playerColor <- use whoseTurn
  player playerColor . supply . wood += w
  starting .= playerColor
  board . smallForest .= Nothing
  subtractWorker

takeAction agri TakeBigForest = agri &~ do
  Just w <- use (board . bigForest)
  playerColor <- use whoseTurn
  player playerColor . supply . wood += w
  board . bigForest .= Nothing
  subtractWorker

takeAction agri TakeSmallQuarry = agri &~ do
  Just s <- use (board . smallQuarry)
  playerColor <- use whoseTurn
  player playerColor . supply . stones += s
  starting .= playerColor
  board . smallQuarry .= Nothing
  subtractWorker

takeAction agri TakeBigQuarry = agri &~ do
  Just s <- use (board . bigQuarry)
  playerColor <- use whoseTurn
  player playerColor . supply . stones += s
  board . bigQuarry .= Nothing
  subtractWorker

takeAction agri TakeExpand = agri &~ do
  Just b <- use (board . expand)
  playerColor <- use whoseTurn
  player playerColor . supply . borders += b
  board . expand .= Nothing
  subtractWorker

tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri action =
  case isProblem agri action of
  Nothing -> let newagri =  takeAction agri action in
    return $ newagri & message .~ ""
  Just err -> return $ agri & message .~ ("Cannot " ++ show action ++ ", " ++ err)




update :: Agricola -> Maybe Action -> Maybe Agricola
update agri action = do
  act <- action
  agri <- tryTakeAction agri act
  return agri
  -- p <-  return $ agri ^. whoseTurn
  -- agri <- return $ agri &~ do (player p . workers) -= 1
  --                             whoseTurn %= otherColor
  -- let p = agri ^. whoseTurn
  -- if agri ^. (player p . workers) > 0
  --   then return agri
  --   else return $ agri &~ do board %= refillBoard;
  --                            (player p . workers) .= 3
  --                            (player (otherColor p ) . workers) .= 3
tryPlaceAnimal :: Agricola -> Coord -> Animal -> Maybe Agricola
tryPlaceAnimal agri c anml =
  if (isLegalAnimalPlacement agri c anml)
  then (Just $ placeAnimal agri c anml)
  else (Just agri)

animalLens :: Functor f => Animal -> (Integer -> f Integer) -> Animals -> f Animals
animalLens Sheep = sheep
animalLens Pig = pigs
animalLens Cow = cows
animalLens Horse = horses

placeAnimal :: Agricola -> Coord -> Animal -> Agricola
placeAnimal agri (cx,cy) animal = agri &~ do
  colr <- use whoseTurn
  player colr . supply . animals . animalLens animal -= 1
  player colr . farm . tile cx cy . tileanimals %= addAnimal animal



hasWorkers :: Agricola -> Bool
hasWorkers agri = agri ^. (player (agri ^. whoseTurn) . workers) > 0

boardSpaceFree :: Agricola -> Action -> Bool
boardSpaceFree agri TakeResources      = isJust (agri ^. board . resources)
boardSpaceFree agri TakeSmallForest    = isJust (agri ^. board . smallForest)
boardSpaceFree agri TakeBigForest      = isJust (agri ^. board . bigForest)
boardSpaceFree agri TakeSmallQuarry    = isJust (agri ^. board . smallQuarry)
boardSpaceFree agri TakeBigQuarry      = isJust (agri ^. board . bigQuarry)
boardSpaceFree agri TakeExpand         = isJust (agri ^. board . expand)
boardSpaceFree agri TakeMillpond       = isJust (agri ^. board . millpond)
boardSpaceFree agri TakePigsAndSheep   = isJust (agri ^. board . pigsAndSheep)
boardSpaceFree agri TakeCowsAndPigs    = isJust (agri ^. board . cowsAndPigs)
boardSpaceFree agri TakeHorsesAndSheep = isJust (agri ^. board . horsesAndSheep)


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
                ]

isProblem :: Agricola -> Action ->  Maybe String
isProblem agri EndTurn = if hasWorkers agri && not (agri ^. hasPlacedWorker)
                         then Just $ show col ++ " has to place worker"
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
  if  canPlaceBorder agri al cy cx (agri ^. whoseTurn)
      then Nothing
      else Just ""

isProblem agri (FreeAnimal an) =
  if agri ^. (player col . supply . animals . animalLens an) == 0
     then Just "you have none in your supply"
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


isLegalAnimalPlacement :: Agricola -> Coord -> Animal -> Bool
isLegalAnimalPlacement = undefined

isSameAnimal :: Agricola -> Coord -> Animal -> Bool
isSameAnimal agri (cx,cy) an =
  let colr = agri ^. whoseTurn in
      let tileAn = agri ^. player colr . farm . tile cx cy . tileanimals in
      isNothing tileAn || (fst $ fromJust tileAn) == an

isEnclosed :: Agricola -> Coord -> Bool
isEnclosed agri c = let col = agri ^. whoseTurn in
  and [isEnclosed' agri col c d | d <- [N,S,E,W]]

data Direction = N | S | E | W deriving (Eq)

-- TODO : update East to allow extensions
isEnclosed' :: Agricola -> Color -> Coord -> Direction -> Bool
isEnclosed' agri col (x,0) N = hasBorder agri col (x,0) N
isEnclosed' agri col (x,y) N = hasBorder agri col (x,y) N || isEnclosed' agri col (x,y-1) N
isEnclosed' agri col (x,2) S = hasBorder agri col (x,2) S
isEnclosed' agri col (x,y) S = hasBorder agri col (x,y) S || isEnclosed' agri col (x,y+1) S
isEnclosed' agri col (0,y) W = hasBorder agri col (0,y) W
isEnclosed' agri col (x,y) W = hasBorder agri col (x,y) W || isEnclosed' agri col (x-1,y) W
isEnclosed' agri col (1,y) E = hasBorder agri col (1,y) E
isEnclosed' agri col (x,y) E = hasBorder agri col (x,y) E || isEnclosed' agri col (x+1,y) E

hasBorder :: Agricola -> Color -> Coord -> Direction -> Bool
hasBorder agri col (cx,cy) N = agri ^. player col . farm . border H cy cx . isThere
hasBorder agri col (cx,cy) S = agri ^. player col . farm . border H (cy+1) cx . isThere
hasBorder agri col (cx,cy) W = agri ^. player col . farm . border V cy cx . isThere
hasBorder agri col (cx,cy) E = agri ^. player col . farm . border V cy (cx+1) . isThere
--(player color . farm . border al n m . isThere )
-- enclosed?
-- free space?
-- correct animal type?
