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
                OpenStable
data Animal = Sheep | Pig | Cow | Horse deriving ( Eq)
data Good = Wood | Stone | Reed deriving (Show)


data Tile = Tile { _building :: Maybe Building
                 , _animals :: Maybe (Animal, Int)
                 , _trough :: Bool
                 }

makeLenses ''Tile

instance Show Building where
  show Stall = "Stl"
  show Stable = "Stb"
  show FarmHouse = "FaH"
  show HalfTimberedHouse = "HTH"
  show Storage = "Sto"
  show Shelter = "She"
  show OpenStable = "OSt"

instance Show Animal where
  show Sheep = "S"
  show Pig  = "P"
  show Cow  = "C"
  show Horse = "H"


showTro :: Bool -> String
showTro False = " "
showTro True = "T"

showAn :: Maybe (Animal, Int) -> String
showAn Nothing = "0 A"
showAn (Just (an, count)) = show count ++ " " ++ show an

showBu :: Maybe Building -> String
showBu Nothing = replicate (length $ show Stall) ' '
showBu (Just bu) = show bu

instance Show Tile where
  show (Tile bu an tro) = unwords [showBu bu ,showAn an , showTro tro]

emptyTile :: Tile
emptyTile = Tile Nothing Nothing False



tileStrLength :: Int
tileStrLength = length $ show emptyTile


fillWith :: Char -> String
fillWith = replicate tileStrLength

fillerStr :: String
fillerStr = fillWith ' '


data Alignment = Horizontal | Vertical
data Border = Border Alignment Bool

instance Show Border where
  show (Border Vertical False) = "|"
  show (Border Vertical True) = "+"
  show (Border Horizontal False) = fillWith '.'
  show (Border Horizontal True) = fillWith '+'




data Farm = Farm { _tiles :: [[Tile]]
                 , _vborders :: [[Border]]
                 , _hborders :: [[Border]]
                 } deriving (Show)

makeLenses ''Farm


showHorizBorder :: [Border] -> String
showHorizBorder hs = " " ++ unwords (map show hs) ++ " "

showFarmLine :: [Tile] -> [Border] -> String
showFarmLine (t:ts) (b:bs) = show b ++ show t ++ showFarmLine ts bs

showFarmLine [] [b] = show b
showFarmLine [] [] = error "too few borders"

showFarm :: Farm -> String
showFarm (Farm (ts:tss) (vs:vss) (hs:hss)) = showHorizBorder hs ++ "\n" ++ showFarmLine ts vs ++ "\n" ++ showFarm (Farm tss vss hss)
showFarm (Farm [] [] [hs]) = showHorizBorder hs
showFarm _ = error "too many lines in farm"


emptyFarm :: Farm
emptyFarm = Farm (replicate 3 $ replicate 2 emptyTile) (replicate 3 $ replicate 3 $ Border Vertical False) (replicate 4 $ replicate 2 $ Border Horizontal False)


printFarm :: Farm -> IO()
printFarm farm = mapM_ putStrLn $ lines $ showFarm farm

homeTile :: Tile
homeTile = Tile (Just FarmHouse) Nothing False

startingFarm :: Farm
startingFarm = emptyFarm & (tiles . element 2 . element 0) .~ homeTile

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




--data Piece = Worker Color | Bo | Animal | Good | Trough

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
        end <- drawLines 3 10 $ lines $ showFarm emptyFarm
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


