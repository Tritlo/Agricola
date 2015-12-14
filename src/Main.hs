module Main where

import Agricola
import Render
import Input
import Update


import UI.NCurses hiding (Color)




gameLoop :: Agricola -> Curses Agricola
gameLoop agri = do
  (w,_,_,_) <- settings
  renderGame agri
  setKeypad w True
  event <- getNextEvent w
  action <- getAction event agri
  case update agri action of
    Nothing -> return agri
    Just agri -> gameLoop agri


main :: IO ()
main = do
  finalState <- runCurses $ gameLoop startingState
  print finalState
  return ()
