module Input where

import Agricola
import UI.NCurses hiding (Color)

import Data.Maybe
import Data.Either
import Control.Lens
import Render
import Debug.Trace


clickedFarm :: Agricola -> Coord -> Maybe Color
clickedFarm agri  coord | inBox coord (farmOffset Blue) (farmVolume agri Blue) = Just Blue
                        | inBox coord (farmOffset Red) (farmVolume agri Red) = Just Red
                        | otherwise = Nothing

(.-.) :: Coord -> Coord -> Coord
(x1,y1) .-. (x2,y2) = (x1 - x2, y1 - y2)

inBox :: Coord -> Coord -> Measurements -> Bool
inBox (x,y) (bx,by) (xv,yv) = bx <= x && x < bx + xv && by <= y && y < by + yv

getClicked :: Agricola -> Coord -> Maybe (Color, Either Tile Border, Coord)
getClicked agri coords = do
  color <-  clickedFarm agri coords
  let offsetcoords@(ox,oy) = coords .-. farmOffset color
        where
  let farmMp = farmMap (agri ^. (player color . farm))
  let fmLs = farmMapLengths farmMp
  let fmHs = farmMapHeights farmMp
  let iy = fromIntegral $ getY oy fmHs
  let ix = fromIntegral $ getX ox fmLs
  let beforeInLine = take ix (farmMp !!  iy)
  let beforeInRow = take iy (map (!!  ix) farmMp)
  itm <- (farmMp !!  iy) !! ix
  let (nx,ny) = case itm of
        (Left _)  -> both %~ (toInteger . length . lefts  . catMaybes) $  (beforeInLine,beforeInRow)
        (Right _) -> both %~ (toInteger . length . rights . catMaybes) $  (beforeInLine,beforeInRow)
  return (color, itm, (nx, ny))
  where getX ox ls = toInteger $ length $ takeWhile (<= ox) $ subSeqSum ls
        getY oy hs = toInteger $ length $ takeWhile (<= oy) $ subSeqSum hs



clickedBorder :: Agricola -> Coord -> Maybe (Alignment, Integer, Integer)
clickedBorder agri (mx,my) = do
  (col, Right (Border a _), (fx,fy)) <- getClicked agri (mx,my)
  return (a, fromIntegral fy, fromIntegral fx)

clickedTile :: Agricola -> Coord -> Maybe (Integer, Integer)
clickedTile agri (mx,my) = do
  (col, Left _, (cx,cy)) <- getClicked agri (mx,my)
  return (fromIntegral cy, fromIntegral cx)


clickedControls  :: Coord -> Maybe Button
clickedControls c@(cx,cy) =
  if inBox c controlsOffset v && tm < length defaultControls && tn < (length . head) defaultControls
    then Just b
    else Nothing
  where v@(vx,vy) = (toInteger $ (1+longestb)*numbs, 5)
        numbs = maximum $ map length defaultControls
        longestb =  maximum $ map (maximum . map (length . show)) defaultControls
        oc@(ox,oy) = c .-. controlsOffset
        -- t@(tn,tm) = (fromInteger $ ox `div` (vx  `div` (toInteger $ length defaultControls)), 0)
        t@(tn,tm) = (
          fromInteger $ ox `div` ((vx `div` (toInteger $ (length . head) defaultControls))),
          fromInteger $ oy `div` ((vy `div` (toInteger $ length defaultControls))))
        b = (defaultControls !! tm) !! tn

clickedBoard :: Agricola -> Coord -> Maybe GameBoardTile
clickedBoard agri c@(cx,cy) =
  if inBox c boardOffset (vx-1,vy-1) && tm < length gameBoardLayout && tn < (length . head) gameBoardLayout
  then return $  (gameBoardLayout !! tm) !! tn
  else Nothing
  where oc@(ox,oy) = c .-. boardOffset
        vol@(vx,vy) = volume (agri ^. board)
        t@(tn,tm) = (
          fromInteger $ ox `div` ((vx - 1) `div` (toInteger $ (length . head) gameBoardLayout)),
          fromInteger $ oy `div` ((vy - 1) `div` (toInteger $ length gameBoardLayout)))

getAnimalTypeFromEvent (EventCharacter 's') = Just Sheep
getAnimalTypeFromEvent (EventCharacter 'p') = Just Pig
getAnimalTypeFromEvent (EventCharacter 'c') = Just Cow
getAnimalTypeFromEvent (EventCharacter 'h') = Just Horse
getAnimalTypeFromEvent (EventMouse int mouseState) =
    case clickedControls (mx,my) of
      Just (AnimalB a)  ->  Just a
      _ -> Nothing
  where (mx,my,_) = mouseCoordinates mouseState
getAnimalTypeFromEvent _ = Nothing

getNextEvent :: Window -> Curses Event
getNextEvent w = do
  event <- waitFor w
  case event of
    EventMouse _ mouseState -> do
      let (mx,my,mz) =  mouseCoordinates mouseState
      updateWindow w $ moveCursor my mx
    _ -> return  ()
  return event

dispMsgAtTopAndWaitForInput :: String -> Curses Event
dispMsgAtTopAndWaitForInput msg = do
  (w, _,_,_) <- settings
  updateWindow w $ do
    moveCursor 0 0
    drawString (replicate 80 ' ')
    moveCursor 0 0
    drawString msg
  render
  getNextEvent w




getAction :: Agricola -> Event -> Curses (Maybe Action)
getAction agri (EventCharacter 'q')  = return Nothing
getAction agri (EventCharacter 'Q')  = return Nothing
getAction agri (EventCharacter ' ')  = return $ Just  EndTurn
getAction agri (EventCharacter '\n') = return $ Just EndPhase
getAction agri (EventCharacter 'f')  = return $ Just TakeSmallForest
getAction agri (EventCharacter 'F')  = return $ Just TakeBigForest
getAction agri (EventCharacter 's')  = return $ Just TakeSmallQuarry
getAction agri (EventCharacter 'S')  = return $ Just TakeBigQuarry
getAction agri (EventCharacter 'e')  = return $ Just TakeExpand
getAction agri (EventCharacter 'm')  = return $ Just TakeMillpond
getAction agri (EventCharacter 'p')  = return $ Just TakePigsAndSheep
getAction agri (EventCharacter 'c')  = return $ Just TakeCowsAndPigs
getAction agri (EventCharacter 'h')  = return $ Just TakeHorsesAndSheep
getAction agri (EventCharacter 't')  = do
  ev <- dispMsgAtTopAndWaitForInput "Choose tile to place trough on"
  case ev of
        m@(EventMouse _ mouseState) -> case clickedTile agri (mx,my) of
          Nothing -> return $ Just DoNothing
          Just (x,y) -> return $ Just $ PlaceTrough x y
          where (mx,my,mz) = mouseCoordinates mouseState
        _ -> return $ Just DoNothing
getAction agri (EventCharacter 'r')  =  return $ Just TakeResources
getAction agri (EventCharacter 'R') = do
  ev <- dispMsgAtTopAndWaitForInput $ unwords ["Choose animal to free"]
  case getAnimalTypeFromEvent ev of
    Just a -> return $ Just $ FreeAnimal a
    Nothing -> return $ Just DoNothing
getAction agri (EventCharacter 'a') = do
  ev <- dispMsgAtTopAndWaitForInput $ unwords ["Choose animal to place"]
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
getAction agri (EventMouse int mouseState) = do
  case clickedBoard agri (mx,my) of
    Just SmallForest -> return $ Just TakeSmallForest
    Just BigForest -> return $ Just TakeBigForest
    Just SmallQuarry -> return $ Just TakeSmallQuarry
    Just BigQuarry -> return $ Just TakeBigQuarry
    Just Resources -> return $ Just TakeResources
    Just Expand -> return $ Just TakeExpand
    Just Millpond -> return $ Just TakeMillpond
    Just PigsAndSheep -> return $ Just TakePigsAndSheep
    Just CowsAndPigs -> return $ Just TakeCowsAndPigs
    Just HorsesAndSheep -> return $ Just TakeHorsesAndSheep
    Just a -> return $ Just (SetMessage (show a ++ " not implemented"))
    Nothing -> case clickedControls (mx,my) of
      Just StopButton -> return $ Just DoNothing
      Just EndTurnButton -> return $ Just EndTurn
      Just EndPhaseButton -> return $ Just EndPhase
      Just PlaceAnimalButton -> getAction agri (EventCharacter 'a')
      Just TakeAnimalButton -> getAction agri (EventCharacter 'A')
      Just FreeAnimalButton -> getAction agri (EventCharacter 'R')
      Just QuitButton -> return Nothing
      _ -> return $ Just DoNothing
  where (mx,my,_) = mouseCoordinates mouseState
getAction agri EventResized = return $ Just DoNothing
getAction agri (EventUnknown ev) = return $ Just DoNothing

waitFor :: Window -> Curses Event
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev -> return ev
