module Main where

import Agricola
import Render
import Update


import UI.NCurses hiding (Color)



waitFor :: Window -> Curses Event
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev -> return ev

gameLoop :: Agricola -> Curses Agricola
gameLoop agri = do
  (w,_,_,_) <- settings
  renderGame agri
  event <- waitFor w
  -- Make sure the cursor ends up where we clicked.
  case event of
    EventMouse i mouseState -> do
      let (mx,my,mz) =  mouseCoordinates mouseState
      updateWindow w $ moveCursor my mx
    _ -> return  ()
  case update agri event of
    Nothing -> return agri
    Just agri -> gameLoop agri


main :: IO ()
main = do
  finalState <- runCurses $ gameLoop startingState
  print finalState
  return ()
