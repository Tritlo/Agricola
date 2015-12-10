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
  if (canPlaceBorder agri al n m (agri ^. whoseTurn))
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

tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri (PlaceBorder alignment cx cy) = tryPlaceBorder agri alignment  cy  cx
tryTakeAction agri (TakeResources sup) = Just (takeResources agri sup)
tryTakeAction agri DoNothing = Just agri

update :: Agricola -> Maybe Action -> Maybe Agricola
update agri action = do
  act <- action
  agri <- tryTakeAction agri act
  p <-  return $ agri ^. whoseTurn
  agri <- return $ agri &~ do (player p . workers) -= 1
                              whoseTurn %= otherColor
  p <-  return $ agri ^. whoseTurn
  if agri ^. (player p . workers) > 0
    then return agri
    else return $ agri &~ do board %= refillBoard;
                             (player p . workers) .= 3
                             (player (otherColor p ) . workers) .= 3
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

addAnimal :: Animal -> Maybe (Animal,Integer) -> Maybe (Animal,Integer)
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
