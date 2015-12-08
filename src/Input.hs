module Input where

import Agricola
import UI.NCurses (Event(..), mouseCoordinates, Curses)
import Data.Maybe
import Data.Either
import Control.Lens



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


getAction :: Agricola -> Event -> Curses (Maybe Action)
getAction agri (EventCharacter 'q')  = return Nothing
getAction agri (EventCharacter 'Q')  = return Nothing
-- getAction agri (EventCharacter 'f')  =  Nothing
-- getAction agri (EventCharacter 'F')  =  Nothing
-- getAction agri (EventCharacter 's')  =  Nothing
-- getAction agri (EventCharacter 'S')  =  Nothing
-- getAction agri (EventCharacter 'e')  =  Nothing
-- getAction agri (EventCharacter 'm')  =  Nothing
-- getAction agri (EventCharacter 'p')  =  Nothing
-- getAction agri (EventCharacter 'c')  =  Nothing
-- getAction agri (EventCharacter 'h')  =  Nothing
getAction agri (EventCharacter char) = undefined
getAction agri (EventSpecialKey key) = undefined

getAction agri (EventMouse int mouseState) = return $ getClickAction agri (mx,my)
  where (mx,my,mz) = mouseCoordinates mouseState

getAction agri EventResized = return $ Just DoNothing
getAction agri (EventUnknown ev) = return $ Just DoNothing
