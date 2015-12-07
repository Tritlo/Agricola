module Render where

import Agricola hiding (Color)
import qualified Agricola as Ag (Color)
import UI.NCurses
import qualified UI.NCurses
import Control.Lens


drawLines :: Integer -> Integer -> [String] -> Update Integer
drawLines n _ [] = return n
drawLines n m (l:ls) = do
    moveCursor n m
    drawString l
    drawLines (n+1) m ls

drawFarm :: Agricola -> Ag.Color -> Update Integer
drawFarm agri col = drawLines (snd $ farmOffset col) (fst $ farmOffset col) $
                    lines $ showFarm (agri ^. (player col . farm))


drawSupply :: Agricola -> Ag.Color -> Integer -> Update Integer
drawSupply agri color start =
  drawLines (start + 2) (fst $ farmOffset color) $
  lines $ show $ agri ^. (player color . supply)


drawState :: Agricola -> ColorID -> ColorID -> Update ()
drawState agri colRed colBlue = do
     clear
     moveCursor 1 2
     drawString "Hello qt3.14!"
     moveCursor 2 2
     drawString "(press q to quit)"
     drawString $ show $ agri ^. whoseTurn

     setColor colRed
     end <- drawFarm agri Red
     end <- drawSupply agri Red end
     moveCursor (end + 1) $ fst $ farmOffset Red
     drawString $ show $ agri ^. (player Red . color)

     setColor colBlue
     end <- drawFarm agri Blue
     end <- drawSupply agri Blue end
     moveCursor (end + 1) $ fst $ farmOffset Blue
     drawString $ show $ agri ^. (player Blue . color)


settings :: Curses (Window, ColorID, ColorID, ColorID)
settings = do
  setEcho True
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
    drawState agri colRed colBlue
    setColor colWhite
    moveCursor mx my
  render
