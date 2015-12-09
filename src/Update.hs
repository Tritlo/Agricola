{-# LANGUAGE RankNTypes #-}
module Update where

import Agricola
import UI.NCurses (Event)
import Input
import Control.Lens
import Control.Monad
import Control.Monad.State

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
  if isLegal agri c anml then
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

isLegal :: Agricola -> Coord -> Animal -> Bool
isLegal = undefined

-- enclosed?
-- free space?
-- correct animal type?
