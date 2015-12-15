{-# LANGUAGE FlexibleContexts #-}
module Input where

import Agricola
import UI.NCurses hiding (Color)

import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad
import Render
import Update
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe

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

getAnimalTypeFromEvent (EventMouse int mouseState) =
    case clickedControls (mx,my) of
      Just (ChoiceB a _ _)  ->  Just a
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



interaction :: String -> (Agricola -> Coord -> Curses (Maybe Action)) -> Agricola -> Curses (Maybe Action)
interaction msg click agri = do
  ev <- dispMsgAtTopAndWaitForInput msg
  case ev of
    EventCharacter 'q' -> return Nothing
    EventCharacter 'Q' -> return Nothing
    EventCharacter ' ' -> getCursorCoord >>= checkControlsAndClick
    m@(EventMouse _ mouseState) -> checkControlsAndClick (mx,my)
      where (mx,my,mz) = mouseCoordinates mouseState
    _ -> return $ Just DoNothing
    where checkControlsAndClick (mx,my) = case clickedControls (mx,my) of
            Just StopButton -> return $ Just DoNothing
            Just EndTurnButton -> return $ Just EndTurn
            Just CancelButton -> return Nothing
            Just QuitButton -> return Nothing
            _ -> click agri (mx,my)


placeTroughInteraction :: String -> Agricola -> Curses (Maybe Action)
placeTroughInteraction msg = interaction msg click
  where click agri (mx,my) = case clickedTile agri (mx,my) of
          Nothing ->  placeTroughInteraction msg agri
          Just (x,y) -> return $ Just $ PlaceTrough x y

placeBorderInteraction :: String -> Agricola -> Curses (Maybe Action)
placeBorderInteraction msg = interaction msg click
  where click agri (mx,my) = case clickedBorder agri (mx,my) of
          Nothing -> placeBorderInteraction msg agri
          Just (a,x,y) -> return $ Just $ PlaceBorder a x y

placeBuildingInteraction :: Building -> String -> Agricola -> Curses (Maybe Action)
placeBuildingInteraction b msg = interaction msg click
  where click agri (mx,my) = case clickedTile agri (mx,my) of
          Nothing -> placeBuildingInteraction b msg agri
          Just (x,y) -> return $ Just $ PlaceBuilding b x y

-- return Just DoNothing on nothing
takeAnimalInteraction agri =  (<|> Just DoNothing) <$>
                              interaction "Choose tile to take animal from:" click agri
  where click agri (mx,my) = case clickedTile agri (mx,my) of
          Nothing -> return $ Just DoNothing
          Just (x,y) -> return $ Just $ TakeAnimal x y

chooseResourceInteraction :: Integer -> [Good] ->  String -> Agricola -> Curses (Maybe Action)
chooseResourceInteraction n choices msg = interaction msg click
  where click agri (mx,my) = case clickedControls (mx,my) of
          Just (ChoiceB _ r _) | r `elem` choices -> return $ Just $ (SpendResources r n)
          _ -> chooseResourceInteraction n choices msg agri


chooseAnimalInteraction :: [Animal] ->  String -> Agricola -> Curses (Maybe Action)
chooseAnimalInteraction choices msg = interaction msg click
  where click agri (mx,my) = case clickedControls (mx,my) of
          Just (ChoiceB a _ _) | a `elem` choices -> return $ Just $ (ChooseAnimal a)
          _ -> chooseAnimalInteraction choices msg agri

chooseBuildingInteraction :: String -> Agricola -> Curses (Maybe Action)
chooseBuildingInteraction msg = interaction msg click
  where click agri (mx,my) = case clickedControls (mx,my) of
          Just (ChoiceB _ _ b) -> return $ Just $ StartBuilding b
          _ -> chooseBuildingInteraction msg agri

placeAnimalInteraction :: Agricola -> Curses (Maybe Action)
placeAnimalInteraction agri = do
  ev <- dispMsgAtTopAndWaitForInput "Choose animal to place:"
  let an = getAnimalTypeFromEvent ev
  case an of
    Nothing -> return $ Just DoNothing
    Just a -> (<|> Just DoNothing) <$>
              interaction "Choose tile to place on:" click agri
      where click agri (mx,my) =  case clickedTile agri (mx,my) of
              Nothing -> return $ Just DoNothing
              Just (x,y) -> return $ Just $ PlaceAnimal a x y




freeAnimalInteraction :: Curses (Maybe Action)
freeAnimalInteraction = do
  ev <- dispMsgAtTopAndWaitForInput $ unwords ["Choose animal to free"]
  case getAnimalTypeFromEvent ev of
    Just a -> return $ Just $ FreeAnimal a
    Nothing -> return $ Just DoNothing


type Interaction = String -> Agricola -> Curses (Maybe Action)

multiActionInteraction :: [String]
                       -> [Action]
                       -> [Interaction]
                       -> Agricola
                       ->   Curses (Maybe Action)
multiActionInteraction msgs costs interactions agri
  = runActionM $ multiActionInteraction' agri [] interactions costs msgs ""
   where
     multiActionInteraction' _ sofar ins cs ms _ |
       null ins || null cs || null ms = return $ MultiAction sofar
     multiActionInteraction' agri sofar interactions@(inter:inters) costs@(c:cs) msgs@(m:ms) err = do
       let prob = isProblem agri c
       when (null sofar && isJust prob) $
         throwError $ concat ["Cannot ", show c,", since ", fromJust prob]
        
       if null sofar
         then lift $ lift $ renderGame $ takeAction c agri
         else lift $ lift $ renderGame agri
       let errtop = if (isJust prob)
                    then concat ["Cannot do more, since "
                                ,fromJust prob
                                ,". Press stop to finish"
                                ," or cancel to cancel."]
                    else err
       Just action <- lift $ lift $ inter (unlines [m,errtop]) agri
       case action of
         DoNothing -> return $ MultiAction sofar
         EndTurn -> return $ MultiAction (sofar ++ [EndTurn])
         a -> do
           let newitems = [c,a]
           case tryTakeMultiAction agri newitems of
             Left na -> multiActionInteraction' na (sofar ++ newitems) inters cs ms ""
             Right err -> do
               let errmsg = unwords ["Cannot " , show c , "to", show a
                                    ,"since " , err ,", try again."]
               multiActionInteraction' agri sofar interactions costs msgs errmsg




buildTroughInteraction :: Agricola -> Curses (Maybe Action)
buildTroughInteraction =
  multiActionInteraction
  (firstmsg : repeat latermsg)
  (StartBuildingTroughs: repeat cost )
  (repeat placeTroughInteraction)
  where
    cost = (SpendResources Wood 3)
    firstmsg = "Click tile to place trough on tile, or stop to cancel."
    latermsg = unwords ["Click tile to place trough on for 3 wood,"
                       , "stop to finish or cancel to cancel."
                       ]

buildStallInteraction :: Agricola -> Curses (Maybe Action)
buildStallInteraction =
  multiActionInteraction
  ["Click tile to place stall for 3 stones and 1 reed, or stop to cancel."]
  [StartBuilding Stall]
  [placeBuildingInteraction Stall]



-- We wrap ExceptT in MaybeT, so that we can fail silently on
-- pattern match errors.
-- On Nothing, we just DoNothing as the action.
-- On an error, we just set the message to the error.
-- We use except t to be able to throw errors nicely
type ActionM m = MaybeT (ExceptT String m)

runActionM :: Monad m => ActionM m Action -> m (Maybe Action)
runActionM actionProducer = do
  m <- runExceptT $  runMaybeT $ actionProducer
  case m of
    Left err -> return $ Just $ SetMessage err
    Right act -> case act of
      Just a -> return $ Just a
      Nothing -> return $ Just DoNothing


buildSpecialBuildingInteraction' :: Agricola -> ActionM Curses Action
buildSpecialBuildingInteraction' agri = do
  unless (hasWorkers agri && boardSpaceFree agri (StartBuilding Shelter)) $
    throwError $ "Cannot build special building,"
                  ++" since no worker can be placed on gameboard tile."
  Just sb@(StartBuilding b) <-
    lift $  lift $ chooseBuildingInteraction chbuild agri
  when (isJust $ isProblem agri sb ) $ do
    let err = fromJust $ isProblem agri sb
    throwError $ concat ["Cannot " , show sb, " since " , err, "."]
  case b of
    OpenStable -> do
      Just sr@(SpendResources _ _) <-
        lift $ lift $ chooseResourceInteraction 3 [Stone, Wood] chmat agri
      when (isJust $ isProblem agri sb ) $ do
        let err = fromJust $ isProblem agri sr
        throwError $ concat ["Cannot " , show sr, " since " , err, "."]
      Just pb@(PlaceBuilding _ _ _) <- getBP b
      Just an@(ChooseAnimal _) <-
        lift $  lift $ chooseAnimalInteraction [Cow,Horse] chan agri
      return $ (MultiAction [sr, sb, pb, an])
    Shelter -> do
      Just pb@(PlaceBuilding _ _ _) <- getBP b
      Just an@(ChooseAnimal _)  <-
        lift $ lift $ chooseAnimalInteraction [Cow,Horse, Sheep, Pig] chan agri
      return $ MultiAction [sb, pb, an]
    _ -> do
      Just pb@(PlaceBuilding _ _ _) <- getBP b
      return $ MultiAction [sb, pb]
  where chbuild = "Choose special building to build:"
        chmat = "Choose building material to build with:"
        chan = "Choose animal to get for free:"
        cht b = "Choose tile to place " ++ show b ++ " on:"
        getBP b = lift $  lift $ placeBuildingInteraction b (cht b) agri


-- buildSpecialBuildingInteraction :: Agricola -> Curses (Maybe Action)
-- buildSpecialBuildingInteraction agri = do
--   if (hasWorkers agri &&  boardSpaceFree agri (StartBuilding Shelter))
--     then
--       do
--         cho <- chooseBuildingInteraction "Choose a special building to build." agri
--         case cho of
--           Just sb@(StartBuilding b) -> do
--             let prob = isProblem agri sb
--             case prob of
--               Just err -> return $ Just (SetMessage $ concat ["Cannot " , show sb , ", since " , err , "."])
--               Nothing -> do
--                   pb <- placeBuildingInteraction b ("Choose tile to place " ++ show b ++ " on:") agri
--                   case pb of
--                     Nothing -> return $ Just DoNothing
--                     Just pb -> case b of
--                       OpenStable -> do
--                         mbres <- chooseResourceInteraction 3 [Stone, Wood] "Choose building material for open stable." agri
--                         case mbres of
--                           Just spendr -> do
--                             an <- chooseAnimalInteraction [Cow,Horse] ("Choose animal to get:") agri
--                             case an of
--                               Just geta -> return $ Just (MultiAction [spendr, sb,pb,geta])
--                               Nothing -> return $ Just DoNothing
--                           Nothing -> return $ Just DoNothing
--                       Shelter -> do
--                         an <- chooseAnimalInteraction [Cow,Horse,Sheep,Pig] ("Choose animal to get:") agri
--                         case an of
--                           Just geta -> return $ Just $ MultiAction [sb,pb,geta]
--                           Nothing -> return $ Just DoNothing
--                       _ -> return $ Just  (MultiAction [sb, pb])
--           _ -> return $ Just DoNothing
--     else return $ Just (SetMessage "Cannot build a special building, since you cannot put a worker on the gameboard tile.")


buildStableInteraction :: Agricola -> Curses (Maybe Action)
buildStableInteraction =
  multiActionInteraction
  (cycle ["Choose whether to build from stone or wood. stop to stop or cancel to cancel."
         , "Click tile to place stable for 5 of the chosen good, or stop to cancel."])
  (StartBuilding Stable : repeat DoNothing)
  (cycle  [chooseResourceInteraction 5 [Stone,Wood], placeBuildingInteraction Stable])

stoneWallInteraction :: Agricola -> Curses (Maybe Action)
stoneWallInteraction =
  multiActionInteraction
  (firstmsg : (secondmsg : repeat latermsg))
  (StartBuildingStoneWalls : (DoNothing : repeat cost))
  (repeat placeBorderInteraction)
  where
    cost = (SpendResources Stone 2)
    firstmsg = "Click on border to place, or click stop to cancel."
    secondmsg = "Click on border to place, stop to finish or cancel to cancel." 
    latermsg = "Click on border to place for 2 stones, stop to finish or cancel to cancel."

woodFenceInteraction :: Agricola -> Curses (Maybe Action)
woodFenceInteraction =
  multiActionInteraction
  (firstmsg : repeat latermsg)
  (StartBuildingWoodFences : repeat cost)
  (repeat placeBorderInteraction)
  where
    cost = (SpendResources Wood 1)
    firstmsg = "Click on border to place for 1 wood, or click stop to cancel"
    latermsg = "Click on border to place for 1 wood, stop to finish or cancel to cancel."

mouseClick :: Coord -> Agricola -> Curses (Maybe Action)
mouseClick (mx,my) agri =
  case clickedControls (mx,my) of
    Just QuitButton -> return Nothing
    Just a | isSomeonesTurn -> case a of
      StopButton -> return $ Just DoNothing
      EndTurnButton -> return $ Just EndTurn
      EndPhaseButton -> return $ Just EndPhase
      PlaceAnimalButton -> placeAnimalInteraction agri
      TakeAnimalButton -> takeAnimalInteraction agri
      FreeAnimalButton -> freeAnimalInteraction
      _ -> return $ Just DoNothing
    Just a -> return $ Just DoNothing
    Nothing  | isSomeonesTurn -> case clickedBoard agri (mx,my) of
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
      Just WoodFence -> woodFenceInteraction agri
      Just BuildStall -> buildStallInteraction agri
      Just BuildStable -> buildStableInteraction agri
      Just SpecialBuilding -> runActionM $ buildSpecialBuildingInteraction' agri
      Nothing -> return $ Just DoNothing
    _ -> return $ Just DoNothing
    where isSomeonesTurn = agri ^. whoseTurn /= No

  
resized :: Curses (Maybe Action)
resized = do
  (sx,sy) <- screenSize
  return $ Just (SetMessage ("Screen resized to" ++ show (sx,sy)))




getAction :: Event -> Agricola -> Curses (Maybe Action)
getAction (EventCharacter 'q')      = const $ return Nothing
getAction (EventCharacter 'Q')      = const $ return Nothing
getAction (EventCharacter ' ')      = \ag -> getCursorCoord >>= (flip mouseClick ag)
getAction (EventCharacter '\n')     = const $ return $ Just EndTurn
getAction (EventCharacter 's')      = const $ return $ Just (SpendResources Stone $ -1)
getAction (EventCharacter 'w')      = const $ return $ Just (SpendResources Wood $ -1)
getAction (EventCharacter 'r')      = const $ return $ Just (SpendResources Reed $ -1)
getAction (EventCharacter 'b')      = placeBorderInteraction "Choose border to place"
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
