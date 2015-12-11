module Input where

import Agricola
import UI.NCurses hiding (Color)

import Data.Maybe
import Data.Either
import Control.Lens
import Render



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
getAction agri (EventCharacter ' ') =   return $ Just  EndTurn
getAction agri (EventCharacter '\n')  =  return $ Just EndPhase
getAction agri (EventCharacter 'f')  =  return $ Just TakeSmallForest
getAction agri (EventCharacter 'F')  =  return $ Just TakeBigForest
getAction agri (EventCharacter 's')  =  return $ Just TakeSmallQuarry
getAction agri (EventCharacter 'S')  =  return $ Just TakeBigQuarry
getAction agri (EventCharacter 'e')  =  return $ Just TakeExpand
getAction agri (EventCharacter 'm')  =  return $ Just TakeMillpond
getAction agri (EventCharacter 'p')  =  return $ Just TakePigsAndSheep
getAction agri (EventCharacter 'c')  =  return $ Just TakeCowsAndPigs
getAction agri (EventCharacter 'h')  =  return $ Just TakeHorsesAndSheep
getAction agri (EventCharacter 'r')  =  return $ Just TakeResources
getAction agri (EventCharacter 'R') = do
  (w, _,_,_) <- settings
  (mx,my) <- getCursor w
  updateWindow w $ do
    moveCursor 0 0
    drawString "Choose animal to free (s) sheep, (p) pig, (c) cow or (h) horse"
    moveCursor my mx
  render
  return Nothing
  ev <- waitFor w
  case ev of
    EventCharacter 'q' -> return Nothing
    EventCharacter 'Q' -> return Nothing
    EventCharacter 's' -> return $ Just $ FreeAnimal Sheep
    EventCharacter 'p' -> return $ Just $ FreeAnimal Pig
    EventCharacter 'c' -> return $ Just $ FreeAnimal Cow
    EventCharacter 'h' -> return $ Just $ FreeAnimal Horse
    _ -> return $ Just DoNothing
getAction agri (EventCharacter 'b') = do
  (w, _,_,_) <- settings
  (mx,my) <- getCursor w
  updateWindow w $ do
    moveCursor 0 0
    drawString "Choose border to place"
    moveCursor my mx
  render
  return Nothing
  ev <- waitFor w
  case ev of
    m@(EventMouse _ mouseState) -> return $ getClickAction agri (mx,my)
      where (mx,my,mz) = mouseCoordinates mouseState
    EventCharacter 'q' -> return Nothing
    EventCharacter 'Q' -> return Nothing
    _ -> getAction agri (EventCharacter 'b')
getAction agri (EventCharacter char) = return $ Just DoNothing
getAction agri (EventSpecialKey key) = undefined
getAction agri (EventMouse int mouseState) = return $ Just DoNothing
getAction agri EventResized = return $ Just DoNothing
getAction agri (EventUnknown ev) = return $ Just DoNothing

waitFor :: Window -> Curses Event
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev -> return ev
