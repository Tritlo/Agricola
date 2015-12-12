module Render where

import Agricola hiding (Color)
import qualified Agricola as Ag (Color)
import UI.NCurses
import qualified UI.NCurses
import Control.Lens


drawLines :: Integral a => a -> a -> [String] -> Update a
drawLines n _ [] = return n
drawLines n m (l:ls) = do
    moveCursor n' m'
    drawString l
    drawLines (n+1) m ls
    where (n',m') =   (toInteger n, toInteger m)

drawFarm :: Agricola -> Ag.Color -> Update Integer
drawFarm agri col = drawLines (snd $ farmOffset col) (fst $ farmOffset col) $
                    lines $ showFarm (agri ^. (player col . farm))


drawSupply :: Agricola -> Ag.Color -> Integer -> Update Integer
drawSupply agri color start =
  drawLines (start + 2) (fst $ farmOffset color) $
  lines $ show $ agri ^. (player color . supply)


drawBoard :: Agricola -> Integer -> Integer ->  Update Integer
drawBoard agri x y= drawLines x y $ lines $ show $ agri ^. board

instructions :: String
instructions = "Press (b) to place border." ++ "\n"
               ++ "Press (R) to free an animal.\n"
               ++ "Press (t) to place trough.\n"
               ++ "Press (a) to place an animal on a tile.\n"
               ++ "Press (A) to take an animal from a tile.\n"
               ++ "Press (r) to " ++ show TakeResources ++ ".\n"
               ++ "Press (f) to " ++ show TakeSmallForest ++ ".\n"
               ++ "Press (F) to " ++ show TakeBigForest ++ ".\n"
               ++ "Press (s) to " ++ show TakeSmallQuarry ++ ".\n"
               ++ "Press (S) to " ++ show TakeBigQuarry ++ ".\n"
               ++ "Press (e) to " ++ show TakeExpand ++ ".\n"
               ++ "Press (m) to " ++ show TakeMillpond ++ ".\n"
               ++ "Press (p) to " ++ show TakePigsAndSheep ++ ".\n"
               ++ "Press (c) to " ++ show TakeCowsAndPigs ++ ".\n"
               ++ "Press (h) to " ++ show TakeHorsesAndSheep ++ ".\n"
               ++ "Press (space) to " ++ show EndTurn ++ ".\n"
               ++ "Press (enter) to " ++ show EndPhase ++ ".\n"
drawState :: Agricola -> ColorID -> ColorID -> ColorID -> Update ()
drawState agri colRed colBlue colBoard = do
     clear
     moveCursor 1 2
     drawString "Hello qt3.14!"
     moveCursor 2 2
     drawString "(press q to quit)"

     setColor colRed
     end <- drawFarm agri Red
     end <- drawSupply agri Red end
     moveCursor (end + 1) $ fst $ farmOffset Red
     drawString $ show $ agri ^. (player Red . color)
     drawString $ " has " ++ (show $ agri ^. (player Red . workers)) ++ " workers."

     setColor colBlue
     end <- drawFarm agri Blue
     end <- drawSupply agri Blue end
     moveCursor (end + 1) $ fst $ farmOffset Blue
     drawString $ show $ agri ^. (player Blue . color)
     drawString $ " has " ++ (show $ agri ^. (player Blue . workers)) ++ " workers."

     setColor colBoard
     end <- drawBoard agri 20 2
     moveCursor (end + 3) 2
     drawString $ show (agri ^. whoseTurn) ++ "'s Turn"
     end <- drawLines (end + 4) 2 $ lines (agri ^. message)
     drawLines (end + 1) 2 $ lines instructions
     return ()

settings :: Curses (Window, ColorID, ColorID, ColorID)
settings = do
  setEcho False
  w <- defaultWindow
  colRed <- newColorID  ColorRed ColorBlack 1
  colBlue <- newColorID ColorBlue ColorBlack 2
  colWhite <- newColorID ColorWhite ColorBlack 3
  return (w,colRed,colBlue,colWhite)


renderGame :: Agricola -> Curses ()
renderGame agri = do
  (w,colRed, colBlue,colWhite) <- settings
  (mx,my) <- getCursor w
  updateWindow w $ do
    drawState agri colRed colBlue colWhite
    setColor colWhite
    moveCursor mx my
  render

