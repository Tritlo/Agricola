module Input where

import Agricola
import UI.NCurses hiding (Color)

import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad
import Render
import Update


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



safeMoveCursor :: Window -> Coord -> Curses ()
safeMoveCursor w (dy,dx) = do
  (sy,sx) <- screenSize
  (mx,my) <- getCursorCoord
  let (ny,nx) = (my+dy, mx+dx)
  unless ( ny < 0 || sy <= ny || nx < 0 || sx <= nx )
    $ updateWindow w $ (moveCursor ny nx)


getNextEvent :: Window -> Curses Event
getNextEvent w = do
  event <- waitFor w
  case event of
    EventMouse _ mouseState -> do
      let (mx,my,_) =  mouseCoordinates mouseState
      updateWindow w $ moveCursor my mx
    EventSpecialKey KeyUpArrow    ->  safeMoveCursor w (-1,0)
    EventSpecialKey KeyDownArrow  ->  safeMoveCursor w (1,0)
    EventSpecialKey KeyLeftArrow  ->  safeMoveCursor w (0,-1)
    EventSpecialKey KeyRightArrow ->  safeMoveCursor w (0,1)
    _ -> return  ()
  return event

dispMsgAtTopAndWaitForInput :: String -> Curses Event
dispMsgAtTopAndWaitForInput msg = do
  (w, _,_,_) <- settings
  (mx,my) <- getCursorCoord
  updateWindow w $ do
    moveCursor 0 0
    drawString (replicate 80 ' ')
    moveCursor 1 0
    drawString (replicate 80 ' ')
    drawLines 0 2 $ lines msg
    moveCursor my mx
  render
  getNextEvent w


getCursorCoord :: Curses (Coord)
getCursorCoord = do
  (w, _,_,_) <- settings
  (my,mx) <- getCursor w
  return (mx,my)



interaction :: String -> (Agricola -> Coord -> Curses (Maybe Action))
            -> Agricola -> Curses (Maybe Action)
interaction msg click agri = do
  ev <- dispMsgAtTopAndWaitForInput msg
  case ev of
    EventCharacter 'q' -> return Nothing
    EventCharacter 'Q' -> return Nothing
    EventCharacter ' ' -> getCursorCoord >>= (click agri)
    m@(EventMouse _ mouseState) -> click agri (mx,my)
      where (mx,my,mz) = mouseCoordinates mouseState
    _ -> return $ Just DoNothing


placeTroughInteraction msg = interaction msg click
  where click agri (mx,my) = case clickedTile agri (mx,my) of
          Nothing -> case clickedControls (mx,my) of
            Just StopButton -> return $ Just DoNothing
            Just CancelButton -> return Nothing
            Just QuitButton -> return Nothing
            _ -> placeTroughInteraction msg agri
          Just (x,y) -> return $ Just $ PlaceTrough x y
                                  

placeBorderInteraction = interaction "Choose border to place" click
  where click agri (mx,my) = case clickedBorder agri (mx,my) of
          Nothing -> case clickedControls (mx,my) of
            Just StopButton -> return $ Just DoNothing
            Just CancelButton -> return Nothing
            Just QuitButton -> return Nothing
            _ -> return $ Just DoNothing
          Just (a, x,y) -> return $ Just $ PlaceBorder a x y


takeAnimalInteraction = interaction "Choose tile to take animal from:" click
  where click agri (mx,my) = case clickedTile agri (mx,my) of
          Nothing -> return $ Just DoNothing
          Just (x,y) -> return $ Just $ TakeAnimal x y



placeAnimalInteraction :: Agricola -> Curses (Maybe Action)
placeAnimalInteraction agri = do
  ev <- dispMsgAtTopAndWaitForInput "Choose animal to place."
  let an = getAnimalTypeFromEvent ev
  case an of
    Nothing -> return $ Just DoNothing
    Just a -> interaction "Choose tile to place on" click agri
      where click agri (mx,my) =  case clickedTile agri (mx,my) of
              Nothing -> return $ Just DoNothing
              Just (x,y) -> return $ Just $ PlaceAnimal a x y



freeAnimalInteraction :: Curses (Maybe Action)
freeAnimalInteraction = do
  ev <- dispMsgAtTopAndWaitForInput $ unwords ["Choose animal to free"]
  case getAnimalTypeFromEvent ev of
    Just a -> return $ Just $ FreeAnimal a
    Nothing -> return $ Just DoNothing


buildTroughInteraction :: Agricola -> Curses (Maybe Action)
buildTroughInteraction agri = buildTroughInteraction' agri [] firstmsg
  where
    firstmsg = "Click tile to place trough on tile, or stop to cancel."
    latermsg = "Click tile to place trough on for 3 wood,"
               ++" stop to finish or cancel to cancel."
    buildTroughInteraction' agri [] _ = do
      case isProblem agri StartBuildingTroughs of
        Nothing -> do
          action <- placeTroughInteraction firstmsg agri
          case action of
            Nothing -> return $ Just DoNothing
            Just DoNothing -> return $ Just DoNothing
            Just pt@(PlaceTrough _ _) -> do
              let newitems = [StartBuildingTroughs, pt]
              case tryTakeMultiAction agri newitems of
                Left na -> buildTroughInteraction' na newitems latermsg
                Right err ->
                  return $ Just (SetMessage $
                                 "Cannot build troughs, since " ++ err)
        Just err ->
          return $ Just (SetMessage $ "Cannot build troughs, since " ++ err)
    buildTroughInteraction' agri sofar msg = do
      renderGame agri
      action <- placeTroughInteraction msg agri
      case action of
        Nothing -> return $ Just DoNothing
        Just DoNothing -> return $ Just (MultiAction sofar)
        Just pt@(PlaceTrough x y) -> do
          let newitems = [SpendResources Wood 3, pt]
          case tryTakeMultiAction agri newitems of
            Left na -> buildTroughInteraction' na (sofar ++ newitems) latermsg
            Right err ->
              buildTroughInteraction' agri sofar $
              unlines [latermsg, "Cannot "
                                 ++ unwords (map show newitems)
                                 ++ " since " ++ err ++ ", try again. " ]
stoneWallInteraction :: Agricola -> Curses (Maybe Action)
stoneWallInteraction agri = stoneWallInteraction' agri [] firstmsg
  where
    firstmsg = "Click on border to place, or click stop to cancel."
    latermsg = "Click on border to place for 2 stones, stop to finish or cancel to cancel."
    stoneWallInteraction' agri [] _ = do
      case isProblem agri StartBuildingStoneWalls of
        Nothing -> do
          action <- placeBorderInteraction agri
          case action of
            Nothing -> return $ Just DoNothing
            Just DoNothing -> return $ Just DoNothing
            Just pb@(PlaceBorder _ _ _) -> do
              let newitems = [StartBuildingStoneWalls, pb]
              case tryTakeMultiAction agri newitems of
                Left na -> stoneWallInteraction' na newitems latermsg
                Right err -> return $ Just (SetMessage $ "Cannot build borders, since " ++ err)
        Just err -> return $ Just (SetMessage $ "Cannot build borders, since " ++ err)
    stoneWallInteraction' agri sofar msg = do
      renderGame agri
      action <- placeBorderInteraction agri
      case action of
        Nothing -> return $ Just DoNothing
        Just DoNothing -> return $ Just (MultiAction sofar)
        Just pb@(PlaceBorder al x y) -> do
          let newitems = [SpendResources Stone 2, pb]
          case tryTakeMultiAction agri newitems of
            Left na -> stoneWallInteraction' na (sofar ++ newitems) latermsg
            Right err ->
              stoneWallInteraction' agri sofar $ unlines [latermsg,
                                                            "Cannot "
                                                            ++ unwords (map show newitems)
                                                            ++ " since " ++ err ++ ", try again. " ]

mouseClick :: Coord -> Agricola -> Curses (Maybe Action)
mouseClick (mx,my) agri = do
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
    Just BuildTroughs -> buildTroughInteraction agri
    Just StoneWall -> stoneWallInteraction agri
    Just a -> return $ Just (SetMessage (show a ++ " not implemented"))
    Nothing -> case clickedControls (mx,my) of
      Just StopButton -> return $ Just DoNothing
      Just EndTurnButton -> return $ Just EndTurn
      Just EndPhaseButton -> return $ Just EndPhase
      Just PlaceAnimalButton -> placeAnimalInteraction agri
      Just TakeAnimalButton -> takeAnimalInteraction agri
      Just FreeAnimalButton -> freeAnimalInteraction
      Just QuitButton -> return Nothing
      _ -> return $ Just DoNothing

resized :: Curses (Maybe Action)
resized = do
  (sx,sy) <- screenSize
  return $ Just (SetMessage ("Screen resized to" ++ show (sx,sy)))




getAction :: Event -> Agricola -> Curses (Maybe Action)
getAction (EventCharacter 'q')      = const $ return Nothing
getAction (EventCharacter 'Q')      = const $ return Nothing
getAction (EventCharacter ' ')      = \ag -> getCursorCoord >>= (flip mouseClick ag)
getAction (EventCharacter '\n')     = const $ return $ Just EndPhase
getAction (EventCharacter 'f')      = const $ return $ Just TakeSmallForest
getAction (EventCharacter 'F')      = const $ return $ Just TakeBigForest
getAction (EventCharacter 's')      = const $ return $ Just TakeSmallQuarry
getAction (EventCharacter 'S')      = const $ return $ Just TakeBigQuarry
getAction (EventCharacter 'e')      = const $ return $ Just TakeExpand
getAction (EventCharacter 'm')      = const $ return $ Just TakeMillpond
getAction (EventCharacter 'p')      = const $ return $ Just TakePigsAndSheep
getAction (EventCharacter 'c')      = const $ return $ Just TakeCowsAndPigs
getAction (EventCharacter 'h')      = const $ return $ Just TakeHorsesAndSheep
getAction (EventCharacter 'r')      = const $ return $ Just TakeResources
getAction (EventCharacter 'R')      = const freeAnimalInteraction
getAction (EventCharacter 'a')      = placeAnimalInteraction
getAction (EventCharacter 'A')      = takeAnimalInteraction
getAction (EventCharacter 'b')      = placeBorderInteraction
getAction (EventCharacter char)     = const $ return $ Just DoNothing
getAction (EventSpecialKey key)     = const $ return $ Just DoNothing
getAction EventResized              = const resized
getAction (EventUnknown ev)         = const $ return $ Just DoNothing
getAction (EventMouse _ mouseState) = mouseClick (mx,my)
  where (mx,my,_) = mouseCoordinates mouseState

waitFor :: Window -> Curses Event
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev -> return ev
