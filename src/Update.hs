module Update where

import Agricola
import UI.NCurses (Event)
import Input
import Control.Lens
import Control.Monad

placeBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Agricola
placeBorder agri al n m color = agri &~ do
  (player color . supply . borders) -= 1
  (player color . farm . border al n m) .= Border al True

canPlaceBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Bool
canPlaceBorder agri al n m  color = hasBorders agri color
                                    && freeSpace agri color al n m

hasBorders :: Agricola -> Color -> Bool
hasBorders agri color = agri ^. (player color . supply . borders) >= 1

freeSpace :: Agricola -> Color -> Alignment -> Int -> Int -> Bool
freeSpace agri color al n m = not $ agri ^.
                              (player color . farm . border al n m . isThere )

otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red

tryPlaceBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Maybe Agricola
tryPlaceBorder agri al n m color =
  if canPlaceBorder agri al n m color then
    do let newAgri = placeBorder agri al n m color
       return newAgri
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
tryTakeAction agri (PlaceBorder alignment cx cy) =
  tryPlaceBorder agri alignment  cy  cx (agri ^. whoseTurn)
tryTakeAction agri (TakeResources sup) = Just (takeResources agri sup)
tryTakeAction agri DoNothing = Just agri

update :: Agricola -> Event -> Maybe Agricola
update agri event = do
  action <- getAction agri event
  agri <- tryTakeAction agri action
  p <-  return $ agri ^. whoseTurn
  agri <- return $ agri &~ do (player p . workers) -= 1
                              whoseTurn %= otherColor
  p <-  return $ agri ^. whoseTurn
  if agri ^. (player p . workers) <= 0 
    then return agri
    else return $ agri &~ do board %= refillBoard;
                             (player p . workers) .= 3
                             (player (otherColor p ) . workers) .= 3
    
    
    
  
