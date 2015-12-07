module Update where

import Agricola
import UI.NCurses (Event)
import Input
import Control.Lens





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
freeSpace agri color al n m = not $
                              agri ^.
                              (player color . farm . border al n m . isThere )


otherColor :: Color -> Color
otherColor Red = Blue
otherColor Blue = Red


tryPlaceBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Maybe Agricola
tryPlaceBorder agri al n m color = do
  
  if canPlaceBorder agri al n m color then do
    let newAgri = placeBorder agri al n m color
    return (newAgri & whoseTurn %~ otherColor)
  else Nothing


tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri (PlaceBorder alignment cx cy) =
  tryPlaceBorder agri alignment  cy  cx (agri ^. whoseTurn)
tryTakeAction agri TakeResources = undefined
tryTakeAction agri DoNothing = Just agri



update :: Agricola -> Event -> Maybe Agricola
update agri event = do
  action <- getAction agri event
  tryTakeAction agri action
