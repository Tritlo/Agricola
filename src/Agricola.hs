{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Haste

import Control.Lens
import Data.Either
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



-- data Row = Row { _tile :: [Tile], _borders :: [Border]}
-- makeLenses ''Row

-- instance Show Row where
--   show (Row (t:ts) (b:bs)) = show b ++ show t ++ show (Row ts bs)
--   show (Row [] [b]) = show b
--   show _ = error "too many borders"

instance Show Border where
  show ABorder = "+"
  show NoBorder = "."


-- data Board = Board { _rows :: [Row], _vborders :: [[Border]]} deriving (Show)
-- makeLenses ''Board


emptyTile :: Tile
emptyTile = Tile Nothing Nothing False



showBoard :: Board -> String
showBoard (Board  []) = ""
showBoard (Board (row:rows)) = concatMap (either show show) row ++ "\n" ++ showBoard (Board rows)

type BoardLoc = Either Tile Border
data Board = Board {_rows :: [[BoardLoc]]} deriving (Show)

makeLenses ''Board


showRow :: [BoardLoc] -> String
showRow = concatMap (either show show) 


tilesAndBorders :: Board -> ([[Tile]], [[Border]])
tilesAndBorders b = (_tiles b, _borders b)

boardFromPair :: ([[Tile]], [[Border]]) -> Board
boardFromPair (ts:tss, bs:bss) = Board $ [ map Right bs, map Left ts] ++ restRows
  where (Board restRows) = boardFromPair (tss,bss)
boardFromPair ([],[]) = error "too many tiles"
boardFromPair ([], [lastborder]) = Board [map Right lastborder]



_borders :: Board -> [[Border]]
_borders = filter (not . null)  . map rights  .  _rows

_tiles :: Board -> [[Tile]]
_tiles = filter (not . null)  . map lefts  .  _rows

_setBorders :: Board -> [[Border]] -> Board
_setBorders board newborders = boardFromPair (tiles,newborders)
  where (tiles,borders) = tilesAndBorders board

_setTiles ::  Board -> [[Tile]] -> Board
_setTiles board newtiles = boardFromPair (newtiles,borders)
  where (tiles,borders) = tilesAndBorders board



updateTile :: Int -> Int -> (Tile -> Tile) -> [[Tile]] -> [[Tile]]
updateTile n m = over (element n . element m)

updateBuilding :: Maybe Building -> Tile -> Tile
updateBuilding build (Tile _ t1 t2)  = Tile build t1 t2

setToFarmHouse = updateTile 0 1 $  updateBuilding $  Just FarmHouse




tiles :: Lens' Board [[Tile]]
tiles = lens _tiles _setTiles

borders :: Lens' Board [[Border]]
borders = lens _borders _setBorders

emptyRow :: [BoardLoc]
emptyRow = [ Right NoBorder
           , Left emptyTile
           , Right NoBorder
           , Left emptyTile
           , Right NoBorder
           , Left emptyTile
           , Right NoBorder
            ]

emptyVertBorder :: [BoardLoc]
emptyVertBorder = replicate 3 $ Right NoBorder

emptyBoard :: Board
emptyBoard = Board [ emptyVertBorder
                   , emptyRow
                   , emptyVertBorder
                   , emptyRow
                   , emptyVertBorder
                   ]



-- Sama og over tiles setToFarmHouse emptyBoard
startingBoard :: Board
startingBoard = emptyBoard & tiles %~ setToFarmHouse

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
        end <- drawLines 3 10 $ lines $ showBoard emptyBoard
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


