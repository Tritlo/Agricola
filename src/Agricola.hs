{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Haste

import Control.Lens
import Control.Monad
-- import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import UI.NCurses hiding (Color)
import qualified UI.NCurses as NC




data Color = Red | Blue deriving (Show)

data Animals = Animals { _sheep   :: Int
                       , _pigs    :: Int
                       , _cows    :: Int
                       , _horses  :: Int
                       } deriving (Show)
makeLenses ''Animals

emptyAnimals :: Animals
emptyAnimals = Animals 0 0 0 0

data Supply = Supply { _borders :: Int
                     , _wood    :: Int
                     , _stones  :: Int
                     , _reeds   :: Int
                     , _animalsupply :: Animals
                     } deriving (Show)
makeLenses ''Supply

emptySupply :: Supply
emptySupply = Supply 0 0 0 0 emptyAnimals


data Building = Stall | Stable | FarmHouse |
                HalfTimberedHouse | Storage | Shelter |
                OpenStable

data Animal = Sheep | Pig | Cow | Horse deriving ( Eq)

data Good = Wood | Stone | Reed deriving (Show)

data Alignment = H | V
data Border = Border {  _alignment ::  Alignment
                      , _isThere   :: Bool
                      }

makeLenses ''Border

data Tile = Tile { _building :: Maybe Building
                 , _animals :: Maybe (Animal, Int)
                 , _trough :: Bool
                 }

makeLenses ''Tile

emptyTile :: Tile
emptyTile = Tile Nothing Nothing False

data Farm = Farm { _tiles :: [[Tile]]
                 , _vborders :: [[Border]]
                 , _hborders :: [[Border]]
                 }
makeLenses ''Farm


class  Show a => Volume a where
  volume :: Show a => a  -> (Integer,Integer)
  volume v = (fromIntegral $ maximum . map length . lines $  show v , fromIntegral $ length  . lines $ show v)


emptyFarm :: Farm
emptyFarm = Farm (replicate 3 $ replicate 2 emptyTile) (replicate 3 $ replicate 3 $ Border V False) (replicate 4 $ replicate 2 $ Border H False)

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

instance Show Farm where
  show = showFarm

data Player = Player { _farm :: Farm
                     , _supply :: Supply
                     , _workers :: Int
                     , _color :: Color
                     } deriving (Show)
makeLenses ''Player

emptyPlayer :: Color ->  Player
emptyPlayer = Player emptyFarm emptySupply 0


data Board = Board Bool deriving (Show)

emptyBoard :: Board
emptyBoard = Board False

data Agricola = Agricola { _red :: Player
                         , _blue :: Player
                         , _global :: Supply
                         , _board :: Board
                         , _starting :: Color
                         , _whoseTurn :: Color
                         } deriving (Show)

makeLenses ''Agricola

emptyAgricola :: Agricola
emptyAgricola = Agricola (emptyPlayer Red) (emptyPlayer Blue) emptySupply emptyBoard Blue Blue

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

fillWith :: Char -> String
fillWith = replicate tileStrLength

instance Show Border where
  show (Border V False) = "|"
  show (Border V True) = "+"
  show (Border H False) = fillWith '.'
  show (Border H True) = fillWith '+'



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


instance Volume Tile
instance Volume Farm
instance Volume Building
instance Volume Border


tileStrLength :: Int
tileStrLength = length $ show emptyTile




printFarm :: Farm -> IO()
printFarm farm = mapM_ putStrLn $ lines $ showFarm farm







--data Piece = Worker Color | Bo | Animal | Good | Trough

drawLines :: Integer -> Integer -> [String] -> Update Integer
drawLines n _ [] = return n
drawLines n m (l:ls) = do
    moveCursor n m
    drawString l
    drawLines (n+1) m ls


-- Getters and setters for a border
_border :: Alignment -> Int -> Int -> Farm -> Border
_border H n m  farm = farm ^. singular (hborders . element n . element m)
_border V n m farm = farm ^.  singular (vborders . element n  . element m)

_setBorder :: Alignment -> Int -> Int -> Farm -> Border -> Farm
_setBorder H n m farm nb@(Border H _) = farm & singular (hborders . element n . element m) .~ nb
_setBorder V n m farm nb@(Border V _) = farm & singular (vborders . element n . element m) .~ nb

border :: Alignment -> Int -> Int -> Lens' Farm Border
border al n m = lens (_border al n m) (_setBorder al n m)

-- Getters and setters for a tile
_tile :: Int -> Int -> Farm -> Tile
_tile n m farm = farm ^. singular (tiles . element n . element m)

_setTile :: Int -> Int -> Farm -> Tile -> Farm
_setTile n m farm newtile = farm & singular (tiles . element n . element m) .~ newtile

tile :: Int -> Int -> Lens' Farm Tile
tile n m = lens (_tile n m) (_setTile n m)


startingFarm :: Farm
startingFarm = emptyFarm & tile 2 0 . building .~ Just FarmHouse

-- We can also chain updates using &~ and .=
-- startingFarm = emptyFarm &~ do
--   tile 2 0 . building .= Just FarmHouse



player :: Color -> Lens' Agricola Player
player Red = red
player Blue = blue

placeBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Agricola
placeBorder agri al n m color = agri &~ do
  (player color . supply . borders) -= 1
  (player color . farm . border al n m) .= Border al True

canPlaceBorder :: Agricola -> Alignment -> Int -> Int -> Color -> Bool
canPlaceBorder agri al n m  color = hasBorders agri color && freeSpace agri color al n m

hasBorders :: Agricola -> Color -> Bool
hasBorders agri color = agri ^. (player color . supply . borders) >= 1

freeSpace :: Agricola -> Color -> Alignment -> Int -> Int -> Bool
freeSpace agri color al n m = agri ^. (player color . farm . border al n m . isThere )

tryPlaceBorder :: Maybe Agricola -> Alignment -> Int -> Int -> Color -> Maybe Agricola
tryPlaceBorder Nothing _ _ _ _ = Nothing
tryPlaceBorder (Just agri) al n m color =
  if canPlaceBorder agri al n m color then
    Just (placeBorder agri al n m color)
  else
    Nothing



initPlayer :: Player -> Player
initPlayer player = player &~ do
  (supply . borders) .= 9
  workers .= 3
  farm .= startingFarm

startingState :: Agricola
startingState = emptyAgricola &~ do
  player Blue %=  initPlayer
  player Red  %=  initPlayer




data Action = DoNothing | PlaceBorder Int Int | TakeResources deriving (Eq, Show)

tryTakeAction :: Agricola -> Action -> Maybe Agricola
tryTakeAction agri (PlaceBorder n m) = undefined
tryTakeAction agri TakeResources = undefined
tryTakeAction agri DoNothing = Nothing



-- Checks wheteher the given coo
inBox :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Bool
inBox (x,y) (bx,by) (xv,yv) = bx <= x && x < bx + xv && by <= y && y < by + yv

gameLoop :: Agricola -> Curses Event
gameLoop agri = gameLoopWCoords agri 0 0

gameLoopWCoords :: Agricola -> Integer -> Integer -> Curses Event
gameLoopWCoords agri mx my = do
  setEcho False
  w <- defaultWindow
  colRed <- newColorID NC.ColorRed NC.ColorBlack 1
  colBlue <- newColorID NC.ColorBlue NC.ColorBlack 2
  colWhite <- newColorID NC.ColorWhite NC.ColorBlack 3
  let redFarmStart = (2,4)
  let blueFarmStart = (2,51)
  updateWindow w $ do
     moveCursor 1 2
     drawString "Hello qt3.14!"
     moveCursor 2 2
     drawString "(press q to quit)"

     setColor colRed
     moveCursor 3 2
     drawString $ show $ agri ^. (red . supply)
     end <- drawLines (snd redFarmStart) (fst redFarmStart) $ lines $ showFarm (agri ^. (red . farm))
     moveCursor (end + 1) 2
     drawString $ show $ agri ^. (red . color)

     setColor colBlue
     moveCursor 49 2
     drawString $ show $ agri ^. (blue . supply)
     end <- drawLines (snd blueFarmStart) (fst blueFarmStart) $ lines $ showFarm (agri ^. (blue . farm))
     moveCursor (end + 1) 2
     drawString $ show $ agri ^. (blue . color)
     setColor colWhite
     moveCursor my mx
  render
  ev <- waitFor w
  case ev of
    EventCharacter 'q' -> return ev
    EventCharacter 'Q' -> return ev
    EventMouse _ mouseState -> do
     let (mx,my,mz) = mouseCoordinates mouseState
     let redFarmVol = volume (agri ^. (player Red . farm))
     let blueFarmVol = volume (agri ^. (player Blue . farm))
     let inBlueFarm = inBox (mx,my) blueFarmStart blueFarmVol
     let inRedFarm = inBox (mx,my) redFarmStart redFarmVol
     updateWindow w (do
                        moveCursor 0 0
                        drawString "Action failed"
                        setColor colBlue
                        drawString $ show inBlueFarm
                        setColor colRed
                        drawString $ show inRedFarm
                        setColor colWhite
                        drawString $ show (mx, my)
                        return ())
     render
     gameLoopWCoords agri mx my
    _ -> gameLoop agri

main :: IO ()
main = do
  ev <- runCurses $ gameLoop startingState
  print ev
  return ()

waitFor :: Window -> Curses Event
waitFor w = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev -> return ev


