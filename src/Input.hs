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


-- If these pattern matches fail, the result is nothing (which is what we want)
clickedBorder :: Agricola -> Coord -> Maybe (Alignment, Integer, Integer)
clickedBorder agri (mx,my) = do
  (col, Right (Border a _), (fx,fy)) <- getClicked agri (mx,my)
  return (a, fromIntegral fy, fromIntegral fx)

clickedTile :: Agricola -> Coord -> Maybe (Integer, Integer)
clickedTile agri (mx,my) = do
  (col, Left _, (cx,cy)) <- getClicked agri (mx,my) 
  return (fromIntegral cy, fromIntegral cx)


getAnimalTypeFromEvent (EventCharacter 's') = Just Sheep
getAnimalTypeFromEvent (EventCharacter 'p') = Just Pig
getAnimalTypeFromEvent (EventCharacter 'c') = Just Cow
getAnimalTypeFromEvent (EventCharacter 'h') = Just Horse
getAnimalTypeFromEvent _ = Nothing




dispMsgAtTopAndWaitForInput msg = do
  (w, _,_,_) <- settings
  (mx,my) <- getCursor w
  updateWindow w $ do
    moveCursor 0 0
    drawString msg 
    moveCursor my mx
  render
  ev <- waitFor w
  return ev

getAction :: Agricola -> Event -> Curses (Maybe Action)
getAction agri (EventCharacter 'q')  = return Nothing
getAction agri (EventCharacter 'Q')  = return Nothing
getAction agri (EventCharacter ' ')  =   return $ Just  EndTurn
getAction agri (EventCharacter '\n') =  return $ Just EndPhase
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
  ev <- dispMsgAtTopAndWaitForInput $ unwords ["Choose animal to free"
                                              , "(s) sheep,"
                                              , "(p) pig,"
                                              , "(c) cow"
                                              , "or"
                                              , "(h) horse"]
  case getAnimalTypeFromEvent ev of
    Just a -> return $ Just $ FreeAnimal a
    Nothing -> return $ Just DoNothing
getAction agri (EventCharacter 'a') = do
  ev <- dispMsgAtTopAndWaitForInput $ unwords [ "Choose animal to place"
                                              , "(s) sheep,"
                                              , "(p) pig,"
                                              , "(c) cow"
                                              , "or"
                                              , "(h) horse"]
  let an = getAnimalTypeFromEvent ev
  case an of
    Nothing -> return $ Just DoNothing
    Just a -> do
      ev <- dispMsgAtTopAndWaitForInput "Choose tile to place on"
      case ev of
        m@(EventMouse _ mouseState) -> case clickedTile agri (mx,my) of
          Nothing -> return $ Just DoNothing
          Just (x,y) -> return $ Just $ PlaceAnimal a x y
          where (mx,my,mz) = mouseCoordinates mouseState
        _ -> return $ Just DoNothing
getAction agri (EventCharacter 'A') = do
  ev <- dispMsgAtTopAndWaitForInput "Choose tile to take animal from"
  case ev of
    m@(EventMouse _ mouseState) -> case clickedTile agri (mx,my) of
      Nothing -> return $ Just DoNothing
      Just (x,y) -> return $ Just $ TakeAnimal x y
      where (mx,my,mz) = mouseCoordinates mouseState
    _ -> return $ Just DoNothing
getAction agri (EventCharacter 'b') = do
  ev <- dispMsgAtTopAndWaitForInput "Choose border to place"
  case ev of
    m@(EventMouse _ mouseState) -> case clickedBorder agri (mx,my) of
      Nothing -> return $ Just DoNothing
      Just (a, x,y) -> return $ Just $ PlaceBorder a x y
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
