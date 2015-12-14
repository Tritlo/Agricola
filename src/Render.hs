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
drawFarm agri col = drawLines oy ox $
                    lines $ show (agri ^. (player col . farm))
                    where (ox,oy) = farmOffset col


drawSupply :: Agricola -> Ag.Color -> Integer -> Update Integer
drawSupply agri color start =
  drawLines (start + 1) (fst $ farmOffset color) $
  lines $ show $ agri ^. (player color . supply)


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
  gameBoardBorder maxlen num
  , "|" ++ concatMap ((++ "|") . center maxlen . show ) (head bs)
  , gameBoardBorder maxlen num
  , "|" ++ concatMap ((++ "|") . center maxlen . show)  (last bs)
  ,gameBoardBorder maxlen num]
  where (x,y) = controlsOffset
        num = maximum $ map length bs
        maxlen = maximum $ map  (maximum . map (length . show)) bs


drawPlayer agri col = do
     end <- drawFarm agri col
     end <- drawSupply agri col end
     moveCursor (end + 1) $ fst $ farmOffset col
     drawString $ show $ agri ^. (player col . color)
     drawString $ " has " ++ show  (agri ^. (player col . workers)) ++ " workers"
     if agri ^. starting == col
       then drawString " and is the starting player."
       else drawString "."
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
     drawString $ show (agri ^. phase) ++ ", "
     drawString $ show (agri ^. whoseTurn) ++ "'s turn. "
     if agri ^. hasPlacedWorker
       then drawString "Turn can be ended if no animals remain unaccounted for."
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
    if (sy < 47) || (sx < 92)
      then do clear
              drawString $ "The current sceen size (" ++ show scz ++ ") is to small!"
              moveCursor 1 0
              drawString "Please make sure that the screen is at least  (47, 92) by fullscreening, or shrinking font."
      else drawState agri colRed colBlue colWhite
    setColor colWhite
    moveCursor my mx
  render
