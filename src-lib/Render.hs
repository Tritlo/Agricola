module Render where

import Agricola hiding (Color)
import qualified Agricola as Ag (Color)
import UI.NCurses
import qualified UI.NCurses
import Control.Lens
import Update
import Data.Maybe
import Data.Function
import Data.List
import Control.Monad

-- Functions for rendering the game

drawLines :: Integral a => a -> a -> [String] -> Update a
drawLines n _ [] = return n
drawLines n m (l:ls) = do
    moveCursor n' m'
    drawString l
    drawLines (n+1) m ls
    where (n',m') =   (toInteger n, toInteger m)

drawFarm :: Agricola -> Ag.Color -> Update Integer
drawFarm agri col = drawLines oy ox $
                    lines $ show (agri ^. (player col . farm))
                    where (ox,oy) = farmOffset agri col

drawSupply :: Agricola -> Ag.Color -> Integer -> Update Integer
drawSupply agri color start =
  drawLines (start + 1) fx $
  lines $ show $ agri ^. (player color . supply)
  where (fx,_) = farmOffset agri color

drawBoard :: Agricola ->  Update Integer
drawBoard agri =
  drawLines y x $ lines $ show $ agri ^. board
  where (x,y) = boardOffset

instructions :: String
instructions = "Press (b) to place border." ++ "\n"
               ++ "Press (R) to free an animal.\n"
               ++ "Press (t) to place trough.\n"
               ++ "Press (a) to place an animal on a tile.\n"
               ++ "Press (A) to take an animal from a tile.\n"
               ++ "Press (space) to " ++ show EndTurn ++ ".\n"
               ++ "Press (enter) to " ++ show EndPhase ++ ".\n"


drawControls :: [[Button]] ->  Update Integer
drawControls bs = drawLines y x [
  "+" ++ intercalate "+" (map (\(_,y) -> replicate y '-') line1pairs) ++ "+"
  , "|" ++ concatMap (\(x,y) -> ((++ "|") . center y . show) x ) line1pairs
  , "+" ++ intercalate "+" (map (\(_,y) -> replicate y '-') line1pairs) ++ "+"
  , "|" ++ concatMap (\(x,y) -> ((++ "|") . center y . show) x ) line2pairs
  , "+" ++ intercalate "+" (map (\(_,y) -> replicate y '-') line1pairs) ++ "+"
  ]
  where (x,y) = controlsOffset
        line1 = head bs
        line2 = last bs
        collengths = zipWith (max `on` shownl ) line1 line2
        line1pairs = zip line1 collengths
        line2pairs = zip line2 collengths
        shownl = (+2) . length . show


drawPlayer agri col = do
     end <- drawFarm agri col
     end <- drawSupply agri col end
     when (agri ^. starting == col) $ do
       moveCursor end $ fst $ farmOffset agri col
       drawString $ show $ agri ^. (player col . color)
       drawString " is the starting player."
     moveCursor (end + 1) $ fst $ farmOffset agri col
     drawString $ show $ agri ^. (player col . color)
     drawString $ " has " ++ show  (agri ^. (player col . workers)) ++ " workers avabilable."
     return end


clearFirstLine = do
  moveCursor 0 0
  drawString $ replicate 92 ' '

drawState agri colRed colBlue colBoard = do
     clear
     setColor colRed
     end <- drawPlayer agri Red

     setColor colBlue
     end <- drawPlayer agri Blue

     setColor colBoard
     if not ( null (agri ^. message))
       then do drawLines 0 2 $ lines (agri ^. message)
               return ()
       else do moveCursor 0 2
               drawString "Welcome to Agricola, all creatures big and small!"
     end <- drawControls defaultControls

     end <- drawBoard agri
     moveCursor (snd boardOffset - 1) (fst boardOffset)
     drawString $ show (agri ^. global . globalBorders) ++ " turns left. "
     drawString $ show (agri ^. phase) ++ ", "
     drawString $ show (agri ^. whoseTurn) ++ "'s turn. "
     if agri ^. hasPlacedWorker
       then if (isNothing $ isProblem agri EndTurn)
            then drawString "Turn can be ended on next action."
            else drawString "Turn can be ended after placing animals."
       else drawString $ show (agri ^. whoseTurn) ++ " has yet to place a worker."
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
  (my,mx) <- getCursor w
  scz@(sy,sx) <- screenSize
  updateWindow w $ do
    if (sy < (fst minScreenSize)) || (sx < (snd minScreenSize))
      then do clear
              drawString $ "The current sceen size (" ++ show scz ++ ") is to small!"
              moveCursor 1 0
              drawString $ "Please make sure that the screen is at least "
                ++ show minScreenSize ++" by fullscreening, or shrinking font."
      else drawState agri colRed colBlue colWhite
    setColor colWhite
    moveCursor my mx
  render
