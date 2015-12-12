{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Agricola where

import Control.Lens
import Data.Either
import Data.List
import Test.QuickCheck
import Control.Monad.State



type Coord = (Integer, Integer)

data Color = Red | Blue deriving (Show)

data Animals = Animals { _sheep   :: Integer
                       , _pigs    :: Integer
                       , _cows    :: Integer
                       , _horses  :: Integer
                       } deriving Eq
makeLenses ''Animals

emptyAnimals :: Animals
emptyAnimals = Animals 0 0 0 0

instance Show Animals where
  show (Animals sh pi co ho) = "Animals: " ++ show sh ++ " sheep "
                               ++ show pi ++ " pigs "
                               ++ show co ++ " cows "
                               ++ show ho  ++ " horses."

data Supply = Supply { _borders :: Integer
                     , _wood    :: Integer
                     , _stones  :: Integer
                     , _reeds   :: Integer
                     , _animals :: Animals
                     } deriving (Eq)
makeLenses ''Supply

instance Show Supply where
  show (Supply bo wo st re animals) = "Supply: "
                                      ++ show bo ++ " borders "
                                      ++ show wo ++ " wood "
                                      ++ show st ++ " stones "
                                      ++ show re ++ " reeds. "
                                      ++ "\n" ++ show animals

emptySupply :: Supply
emptySupply = Supply 0 0 0 0 emptyAnimals

data Building = Stall | Stable | FarmHouse |
                HalfTimberedHouse | Storage | Shelter |
                OpenStable

data Animal = Sheep | Pig | Cow | Horse deriving ( Eq)

data Good = Wood | Stone | Reed deriving (Show)

data Alignment = H | V deriving (Eq, Show)
data Border = Border {  _alignment ::  Alignment
                      , _isThere   :: Bool
                      }

makeLenses ''Border

data Tile = Tile { _building :: Maybe Building
                 , _tileanimals :: Maybe (Animal, Integer)
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

type Measurements = (Integer, Integer)



farmMapHoriz :: [Border] -> [Maybe (Either Tile Border)]
farmMapHoriz hs = (Nothing : intersperse Nothing (map (Just . Right ) hs))
                  ++ [Nothing]

type FarmMap = [[Maybe (Either Tile Border)]]

farmMap :: Farm -> FarmMap
farmMap (Farm (ts:tss) (vs:vss) (hs:hss)) =
  farmMapHoriz hs : farmMapTileLine ts vs :  farmMap (Farm tss vss hss)
farmMap (Farm [] [] [hs] ) = [farmMapHoriz hs]
farmMap _ = error "too many tiles or borders in farmMap"


farmMapTileLine :: [Tile] -> [Border] -> [Maybe (Either Tile Border)]
farmMapTileLine (t:ts) (b:bs) = map Just  [Right b, Left t]
                                ++ farmMapTileLine ts bs
farmMapTileLine [] [b] = [Just (Right b)]
farmMapTileLine _ _ = error "too many tiles or borders in farmMapTileLine"

class  Show a => Volume a where
  volume :: Show a => a  -> (Integer,Integer)
  volume v = ( fromIntegral $ maximum . map length . lines $  show v
             , fromIntegral $ length  . lines $ show v)


emptyFarm :: Farm
emptyFarm = Farm
            (replicate 3 $ replicate 2 emptyTile)
            (replicate 3 $ replicate 3 $ Border V False)
            (replicate 4 $ replicate 2 $ Border H False)

showHorizBorder :: [Border] -> String
showHorizBorder hs = "+" ++ intercalate "+" (map show hs) ++ "+"

showFarmLine :: [Tile] -> [Border] -> String
showFarmLine (t:ts) (b:bs) = show b ++ show t ++ showFarmLine ts bs

showFarmLine [] [b] = show b
showFarmLine [] [] = error "too few borders"

showFarm :: Farm -> String
showFarm (Farm (ts:tss) (vs:vss) (hs:hss) ) = showHorizBorder hs ++ "\n"
                                              ++ showFarmLine ts vs ++ "\n"
                                              ++ showFarm (Farm tss vss hss )
showFarm (Farm [] [] [hs] ) = showHorizBorder hs
showFarm _ = error "too many lines in farm"



instance Show Farm where
  show = showFarm

data Player = Player { _farm :: Farm
                     , _supply :: Supply
                     , _workers :: Integer
                     , _color :: Color
                     } deriving (Show)
makeLenses ''Player

emptyPlayer :: Color ->  Player
emptyPlayer = Player emptyFarm emptySupply 0


data Gameboard = Gameboard {
    _smallForest :: Maybe Integer
  , _bigForest :: Maybe Integer
  , _smallQuarry :: Maybe Integer
  , _bigQuarry :: Maybe Integer
  , _expand :: Maybe Integer
  , _millpond :: Maybe (Integer, Integer)
  , _pigsAndSheep :: Maybe (Integer, Integer)
  , _cowsAndPigs :: Maybe (Integer, Integer)
  , _horsesAndSheep :: Maybe (Integer, Integer)
  , _resources :: Maybe ()
                           }
makeLenses ''Gameboard



append :: MonadState [a] m => [a] -> m ()
append s = id %= flip (++) s


instance Show Gameboard where
  show board = "" &~ do
    append "Gameboard status: "
    case _smallForest board of
      Nothing -> append "Worker on"
      Just i -> append $ show i ++ " wood and starting token in"
    append " small forest, "
    case _bigForest board of
      Nothing -> append "Worker on"
      Just i -> append $ show i ++ " wood in"
    append " big forest, "
    case _smallQuarry board of
      Nothing -> append "Worker on"
      Just i -> append $ show i ++ " stones in"
    append " small quarry, "
    case _bigQuarry board of
      Nothing -> append "Worker on"
      Just i -> append $ show i ++ " stones in"
    append " big quarry, "
    case _expand board of
      Nothing -> append "Worker on"
      Just i -> append $ show i ++ " fences for"
    append " expansion. \n"
    case _millpond board of
      Nothing -> append "Worker on"
      Just (a, b) -> append $ show a ++ " reeds and " ++ show b ++ " sheep in"
    append " millpond, "
    case _pigsAndSheep board of
      Nothing -> append "Worker on"
      Just (a, b) -> append $ show a ++ " pigs and " ++ show b ++ " sheep in"
    append " pigs and sheep, "
    case _cowsAndPigs board of
      Nothing -> append  "Worker on"
      Just (a, b) -> append $ show a ++ " cows and " ++ show b ++ " pigs in"
    append " cows and pigs, "
    case _horsesAndSheep board of
      Nothing -> append  "Worker on"
      Just (a, b) -> append $ show a ++ " horses and " ++ show b ++ " sheep in"
    append " horses and sheep.\n"
    case _resources board of
      Nothing -> append "Worker on"
      Just _ -> append "No worker on"
    append " resources."



emptyBoard = Gameboard {
    _smallForest = Nothing
  , _bigForest = Nothing
  , _smallQuarry = Nothing
  , _bigQuarry = Nothing
  , _expand = Nothing
  , _millpond = Nothing
  , _pigsAndSheep = Nothing
  , _cowsAndPigs = Nothing
  , _horsesAndSheep = Nothing
  , _resources = Nothing
                       }



removeWorkerOfSingle :: Maybe Integer -> Maybe Integer
removeWorkerOfSingle Nothing = Just 0
removeWorkerOfSingle a = a

removeWorkerOfAnimals :: Maybe (Integer, Integer) -> Maybe (Integer, Integer)
removeWorkerOfAnimals Nothing = Just (0, 0)
removeWorkerOfAnimals a = a

takeWorkers :: Gameboard -> Gameboard
takeWorkers board = board &~ do
  smallForest %= removeWorkerOfSingle
  bigForest   %= removeWorkerOfSingle
  smallQuarry %= removeWorkerOfSingle
  bigQuarry   %= removeWorkerOfSingle
  expand      %= removeWorkerOfSingle
  millpond        %= removeWorkerOfAnimals
  pigsAndSheep    %= removeWorkerOfAnimals
  cowsAndPigs     %= removeWorkerOfAnimals
  horsesAndSheep  %= removeWorkerOfAnimals
  resources      .= Just ()

-- Workers must have been remove prior, i.e. no nothing
refillAnimals :: Maybe (Integer, Integer) -> Maybe (Integer, Integer)
refillAnimals (Just (0,0)) = Just (1,0)
refillAnimals (Just (a,b)) = Just (a,b+1)


refillBoard :: Gameboard -> Gameboard
refillBoard board = board &~ do
  smallForest %= fmap (+ 1)
  bigForest %= fmap (+ 3)
  smallQuarry %= fmap (+ 1)
  bigQuarry %= fmap (+ 2)
  expand %= fmap (+ 1)
  millpond %= refillAnimals
  pigsAndSheep %= refillAnimals
  cowsAndPigs %= refillAnimals
  horsesAndSheep %= refillAnimals

startingBoard :: Gameboard
startingBoard = (refillBoard . takeWorkers)  emptyBoard

data Phase = WorkPhase | BreedingPhase deriving (Show, Eq)

data Agricola = Agricola { _red :: Player
                         , _blue :: Player
                         , _global :: Supply
                         , _board :: Gameboard
                         , _starting :: Color
                         , _whoseTurn :: Color
                         , _hasPlacedWorker :: Bool
                         , _message :: String
                         , _phase :: Phase
                         } deriving (Show)

makeLenses ''Agricola


emptyAgricola :: Agricola
emptyAgricola = Agricola {_red = emptyPlayer Red
                         , _blue = emptyPlayer Blue
                         , _global = emptySupply
                         , _board = emptyBoard
                         , _starting = Red
                         , _whoseTurn = Red
                         , _hasPlacedWorker = False
                         , _message = ""
                         , _phase = WorkPhase
                         }


data Action = DoNothing
              | PlaceBorder Alignment Integer Integer
              | FreeAnimal Animal
              | EndTurn
              | EndPhase
              | TakeResources
              | TakeSmallForest
              | TakeBigForest
              | TakeSmallQuarry
              | TakeBigQuarry
              | TakeExpand
              | TakeMillpond
              | TakePigsAndSheep
              | TakeCowsAndPigs
              | TakeHorsesAndSheep
              | TakeAnimal Integer Integer
              | PlaceAnimal Animal Integer Integer
              | PlaceTrough Integer Integer
            deriving (Eq)


instance Show Action where
  show DoNothing = "do nothing"
  show (PlaceBorder al n m) = case al of
    V -> "place vertical border on " ++ show n ++ ", " ++ show m
    H -> "place horizontal border on " ++ show n ++ ", " ++ show m
  show EndTurn = "end turn"
  show TakeResources = "take resources"
  show TakeSmallForest = "take from small forest"
  show TakeBigForest = "take from big forest"
  show TakeSmallQuarry = "take from small quarry"
  show TakeBigQuarry = "take from big quarry"
  show TakeExpand = "take from expansion"
  show TakeMillpond = "take from millpond"
  show TakePigsAndSheep = "take from pigs and sheep"
  show TakeCowsAndPigs = "take from cows and pigs"
  show TakeHorsesAndSheep = "take from horses and sheep"
  show EndPhase = "end phase"
  show (FreeAnimal a) =  "free a " ++ show a
  show (TakeAnimal n m) = "take animal from tile " ++ show n ++", " ++ show m
  show (PlaceAnimal a n m) = "place " ++ show a ++ " on tile " ++ show n ++", " ++ show m
  show (PlaceTrough n m) = "place trough on tile " ++ show n ++", " ++ show m


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
  show (Border H False) = fillWith '-'
  show (Border H True) = fillWith '+'



showTro :: Bool -> String
showTro False = " "
showTro True = "T"

showAn :: Maybe (Animal, Integer) -> String
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


-- Getters and setters for a border
_border :: Alignment -> Integer -> Integer -> Farm -> Border
_border H n m  farm = farm ^. singular (hborders . element (fromInteger n) . element (fromInteger m))
_border V n m farm = farm ^.  singular (vborders . element (fromInteger n)  . element (fromInteger m))

_setBorder :: Alignment -> Integer -> Integer -> Farm -> Border -> Farm
_setBorder H n m farm nb@(Border H _) = farm & singular
                                        (hborders . element (fromInteger n) . element (fromInteger m))
                                        .~ nb
_setBorder V n m farm nb@(Border V _) = farm & singular
                                        (vborders . element (fromInteger n) . element (fromInteger m))
                                        .~ nb

border :: Alignment -> Integer -> Integer -> Lens' Farm Border
border al n m = lens (_border al n m) (_setBorder al n m)

-- Getters and setters for a tile
_tile :: Integer -> Integer -> Farm -> Tile
_tile n m farm = farm ^. singular (tiles . element (fromInteger n) . element (fromInteger m))

_setTile :: Integer -> Integer -> Farm -> Tile -> Farm
_setTile n m farm newtile = farm & singular
                            (tiles . element (fromInteger n) . element (fromInteger m))
                            .~ newtile

tile :: Integer -> Integer -> Lens' Farm Tile
tile n m = lens (_tile n m) (_setTile n m)


startingFarm :: Farm
startingFarm = emptyFarm & tile 2 0 . building .~ Just FarmHouse

player :: Color -> Lens' Agricola Player
player Red = red
player Blue = blue


initPlayer :: Player -> Player
initPlayer player = player &~ do
  (supply . borders) .= 9
  workers .= 3
  farm .= startingFarm

startingState :: Agricola
startingState = emptyAgricola &~ do
  player Blue %=  initPlayer
  player Red  %=  initPlayer
  board .= startingBoard




farmOffset :: Color -> Coord
farmOffset Red = (2,4)
farmOffset Blue = (60,4)

farmVolume :: Agricola -> Color -> Measurements
farmVolume agri col = volume (agri ^. (player col . farm))



showFarmMapLine :: [Maybe (Either Tile Border)] -> String
showFarmMapLine (Nothing:xs) = " " ++ showFarmMapLine xs
showFarmMapLine (Just (Right (Border H True)):xs) = "+" ++ showFarmMapLine xs
showFarmMapLine (Just (Right (Border H False)):xs) = "-" ++ showFarmMapLine xs
showFarmMapLine (Just (Right (Border V True)):xs) = "+" ++ showFarmMapLine xs
showFarmMapLine (Just (Right (Border V False)):xs) = "|" ++ showFarmMapLine xs
showFarmMapLine (Just (Left _):xs) = "T" ++ showFarmMapLine xs
showFarmMapLine [] = ""

showFarmMap :: FarmMap -> String
showFarmMap farmMap = concat [r ++ "\n" | r <- map showFarmMapLine farmMap]

instance Show FarmMap where
  show = showFarmMap

subSeqSum :: [Integer] -> [Integer]
subSeqSum = subSeqSum' [] 0
  where subSeqSum' ys s [] = reverse ys
        subSeqSum' ys s (x:xs) = subSeqSum' ((x + s):ys) (x+s) xs


farmMapLineLengths :: [Maybe (Either Tile Border)] -> [Integer]
farmMapLineLengths (Nothing:xs) = 1 : farmMapLineLengths xs
farmMapLineLengths (Just (Right b):xs) =
  fst (volume b): farmMapLineLengths xs
farmMapLineLengths (Just (Left t):xs)  =
  fst (volume t): farmMapLineLengths xs
farmMapLineLengths [] = []

farmMapLengths = map farmMapLineLengths
