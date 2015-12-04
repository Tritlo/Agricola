{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Haste

import Control.Lens
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import UI.NCurses





data AgricolaColor = Red | Blue

data Building = Stall | Stable | FarmHouse |
                HalfTimberedHouse | Storage | Shelter |
                OpenStable deriving (Show)
data Animal = Sheep | Pig | Cow | Horse deriving (Show, Eq)
data Good = Wood | Stone | Reed deriving (Show)


data Tile = Tile { _building :: Maybe Building
                 , _animals :: Maybe (Animal, Int)
                 , _trough :: Bool
                 }

makeLenses ''Tile

instance Show Tile where
  show t = "T"


data Border = ABorder | NoBorder



data Row = Row { _tiles :: [Tile], _borders :: [Border]}
makeLenses ''Row

instance Show Row where
  show (Row (t:ts) (b:bs)) = show b ++ show t ++ show (Row ts bs)
  show (Row [] [b]) = show b
  show _ = error "too many borders"

instance Show Border where
  show ABorder = "+"
  show NoBorder = "."


data Board = Board { _rows :: [Row], _vborders :: [[Border]]} deriving (Show)
makeLenses ''Board


emptyTile :: Tile
emptyTile = Tile Nothing Nothing False



showBoard :: Board -> String
showBoard (Board  [] []) = ""
showBoard (Board (row:rows) (vertborders:borders)) =
  concatMap show vertborders ++ "\n"
  ++ show row ++ "\n" ++ showBoard (Board rows borders)
showBoard (Board [] [vborder]) = concatMap show vborder
showBoard (Board _ _) = error "Too many borders"


emptyRow :: Row
emptyRow = Row (replicate 3 emptyTile) (replicate 4 NoBorder)
emptyBoard :: Board
emptyBoard = Board (replicate 2 emptyRow) (replicate 3 $ replicate 3 NoBorder)


pathToTile :: (Applicative f, Indexable Int p) =>
              Int -> Int ->  p Tile (f Tile) -> Board -> f Board
pathToTile n m = rows . element n . tiles . element m


updateBuilding :: Board -> Int -> Int -> Maybe Building -> Board
updateBuilding board n m build = pathToTile n m . building .~ build $ board

startingBoard :: Board
startingBoard = updateBuilding emptyBoard 0 1 $ Just FarmHouse


data Direction = North | South | East | West deriving (Eq)

updateBorder :: Board -> Int -> Int -> Direction -> Border -> Board
updateBorder board n m dir bord = undefined


-- data Animals = Animals { sheep   :: Int
--                        , pigs    :: Int
--                        , cows    :: Int
--                        , horses  :: Int
--                        }

-- data Supply = Supply { borders :: Int
--                      , wood    :: Int
--                      , stones  :: Int
--                      , reeds   :: Int
--                      }




data Piece = Worker Color | Border | Animal | Good | Trough

drawLines :: Integer -> Integer -> [String] -> Update Integer
drawLines n _ [] = return n
drawLines n m (l:ls) = do
    moveCursor n m
    drawString l
    drawLines (n+1) m ls

    


main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello qt3.14!"
        end <- drawLines 3 10 $ lines $ showBoard startingBoard
        moveCursor (end + 1) 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


-- main :: IO()
-- main = do
--   mapM_ putStrLn $ lines $ showBoard emptyBoard
--   putStrLn "Hello, baby"
