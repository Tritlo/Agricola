module Input where

import Agricola
import UI.NCurses (Event(..), mouseCoordinates)
import Data.Maybe
import Data.Either
import Control.Lens


data Action = DoNothing |
              PlaceBorder Alignment Int Int |
              TakeResources deriving (Eq, Show)

clickedFarm :: Agricola -> Coord -> Maybe Color
clickedFarm agri  coord
  | inBox coord (farmOffset Blue) (farmVolume agri Blue) = Just Blue
  | inBox coord (farmOffset Red) (farmVolume agri Red) = Just Red
  | otherwise = Nothing
  where inBox (x,y) (bx,by) (xv,yv) = bx <= x && x < bx + xv && by <= y && y < by + yv

getClicked :: Agricola -> Coord -> Maybe (Color, Either Tile Border, Coord)
getClicked agri coords = do
  color <-  clickedFarm agri coords
  let offsetcoords@(ox,oy) = coords .-. farmOffset color
        where (.-.) :: Coord -> Coord -> Coord
              (x1,y1) .-. (x2,y2) = (x1 - x2, y1 - y2)
  let farmMp = farmMap (agri ^. (player color . farm))
  let fmLs = farmMapLengths farmMp
  let beforeInLineLengths = takeWhile (<= ox) $ subSeqSum (fmLs !! fromIntegral oy)
  let ix = length beforeInLineLengths
  let beforeInLine = take ix (farmMp !! fromIntegral oy)
  itm <- (farmMp !! fromIntegral oy) !! ix
  let nx = case itm of
        (Left _) -> length $ lefts $ catMaybes beforeInLine
        (Right _) -> length $ rights $ catMaybes beforeInLine
  return (color,itm,  (fromIntegral nx , fromIntegral oy `div` 2  ))

getClickAction :: Agricola -> Coord -> Maybe Action
getClickAction agri (mx,my) = case getClicked agri (mx,my) of
  Nothing -> return DoNothing
  Just (col, item, (cx,cy)) -> case item of
    Left _ -> return DoNothing
    (Right (Border a _)) -> return $ PlaceBorder a (fromIntegral cx) (fromIntegral cy)


getAction :: Agricola -> Event -> Maybe Action
getAction agri (EventCharacter 'q')  =  Nothing
getAction agri (EventCharacter 'Q')  =  Nothing
getAction agri (EventCharacter char) = undefined
getAction agri (EventSpecialKey key) = undefined

getAction agri (EventMouse int mouseState) =  getClickAction agri (mx,my)
  where (mx,my,mz) = mouseCoordinates mouseState

getAction agri EventResized = Just DoNothing
getAction agri (EventUnknown ev) = Just DoNothing
