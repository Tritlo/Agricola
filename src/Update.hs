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

takeAction :: Agricola -> Action -> Agricola
takeAction agri DoNothing = agri
takeAction agri (PlaceBorder al cx cy) = placeBorder agri al cy cx
takeAction agri EndTurn = agri & whoseTurn %~ otherColor
takeAction agri TakeResources = agri &~ do
  id %= flip takeResources (Supply 0 1 1 1 emptyAnimals)
  playerColor <- use whoseTurn
  (player playerColor . workers) -= 1
  board . resources .= Nothing

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
  subtractWorker

takeAction agri TakeBigForest = agri &~ do
  Just w <- use (board . bigForest)
  playerColor <- use whoseTurn
  player playerColor . supply . wood += w
  subtractWorker

takeAction agri TakeSmallQuarry = agri &~ do
  Just s <- use (board . smallQuarry)
  playerColor <- use whoseTurn
  player playerColor . supply . stones += s
  starting .= playerColor
  subtractWorker

takeAction agri TakeBigQuarry = agri &~ do
  Just s <- use (board . bigQuarry)
  playerColor <- use whoseTurn
  player playerColor . supply . stones += s
  subtractWorker

takeAction agri TakeExpand = agri &~ do
  Just b <- use (board . expand)
  playerColor <- use whoseTurn
  player playerColor . supply . borders += b
  subtractWorker

tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri action =
  if isLegal agri action
  then return $ takeAction agri action
  else return agri


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
  if isLegalAnimal agri c anml then
    do let newAgri = placeAnimal agri c anml
       return newAgri
  else return agri

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

addAnimal :: Animal -> Maybe (Animal,Integer) -> Maybe (Animal,Integer)
addAnimal a Nothing = Just (a,1)
addAnimal _ (Just (a,n)) = Just (a,n+1)


isLegalAnimal = undefined


hasWorkers :: Agricola -> Bool
hasWorkers agri = agri ^. (player (agri ^. whoseTurn) . workers) > 0

isLegal :: Agricola -> Action ->  Bool
isLegal _ EndTurn = True
isLegal _ DoNothing = True
isLegal agri (PlaceBorder al cx cy) = canPlaceBorder agri al cy cx (agri ^. whoseTurn)
isLegal agri TakeResources      = hasWorkers agri && isJust (agri ^. board . resources)
isLegal agri TakeSmallForest    = hasWorkers agri && isJust (agri ^. board . smallForest)
isLegal agri TakeBigForest      = hasWorkers agri && isJust (agri ^. board . bigForest)
isLegal agri TakeSmallQuarry    = hasWorkers agri && isJust (agri ^. board . smallQuarry)
isLegal agri TakeBigQuarry      = hasWorkers agri && isJust (agri ^. board . bigQuarry)
isLegal agri TakeExpand         = hasWorkers agri && isJust (agri ^. board . expand)
isLegal agri TakeMillpond       = hasWorkers agri && isJust (agri ^. board . millpond)
isLegal agri TakePigsAndSheep   = hasWorkers agri && isJust (agri ^. board . pigsAndSheep)
isLegal agri TakeCowsAndPigs    = hasWorkers agri && isJust (agri ^. board . cowsAndPigs)
isLegal agri TakeHorsesAndSheep = hasWorkers agri && isJust (agri ^. board . horsesAndSheep)
isLegal agri a = error $ "did not find legal for " ++ show a


-- enclosed?
-- free space?
-- correct animal type?
