{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Agricola where

import Control.Lens
import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Char (toUpper, toLower)

-- Definitions of game components

type Coord = (Integer, Integer)

data Color = Red | Blue | No deriving (Show, Eq)

data Animals = Animals { _sheep   :: Integer
                       , _pigs    :: Integer
                       , _cows    :: Integer
                       , _horses  :: Integer
                       } deriving Eq
makeLenses ''Animals

emptyAnimals :: Animals
emptyAnimals = Animals 0 0 0 0

instance Show Animals where
  show (Animals s p c h) = "Animals: " ++ show s ++ " sheep "
                               ++ show p ++ " pigs "
                               ++ show c ++ " cows "
                               ++ show h  ++ " horses."

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
                                      ++ show re ++ " reeds."
                                      ++ "\n" ++ show animals

emptySupply :: Supply
emptySupply = Supply 0 0 0 0 emptyAnimals

data Building = Stall | Stable | Cottage |
                HalfTimberedHouse | Storage | Shelter |
                OpenStable
                deriving (Eq)

data GlobalSupply = GlobalSupply { _troughs :: Integer
                                 , _stalls :: Integer
                                 , _expansions :: Integer
                                 , _globalBorders :: Integer
                                 , _htHouse :: Integer
                                 , _storage :: Integer
                                 , _shelter :: Integer
                                 , _openStables :: Integer
                                 } deriving (Eq,Show)
makeLenses ''GlobalSupply

emptyGlobal :: GlobalSupply
emptyGlobal = GlobalSupply { _troughs = 0
                           , _stalls = 0
                           , _expansions = 0
                           , _globalBorders = 0
                           , _htHouse = 0
                           , _storage = 0
                           , _shelter = 0
                           , _openStables = 0
                           }

startingGlobalSupply :: GlobalSupply
startingGlobalSupply = emptyGlobal { _troughs = 10
                                   , _stalls = 4
                                   , _expansions = 4
                                   , _globalBorders = 8
                                   , _htHouse = 1
                                   , _storage = 1
                                   , _shelter = 1
                                   , _openStables = 1
                                   }

globalBuildingLens
  :: Functor f =>
     Building
     -> (Integer -> f Integer) -> GlobalSupply -> f GlobalSupply
globalBuildingLens HalfTimberedHouse = htHouse
globalBuildingLens Storage = storage
globalBuildingLens Shelter = shelter
globalBuildingLens OpenStable = openStables
globalBuildingLens Stall = stalls

data Animal = Sheep | Pig | Cow | Horse deriving (Eq, Ord)

animalLens
  :: Functor f =>
     Animal -> (Integer -> f Integer) -> Animals -> f Animals
animalLens Sheep = sheep
animalLens Pig = pigs
animalLens Cow = cows
animalLens Horse = horses

data Good = Wood | Stone | Reed | Fence deriving (Eq,Show)

goodLens
  :: Functor f =>
     Good -> (Integer -> f Integer) -> Supply -> f Supply
goodLens Wood = wood
goodLens Stone = stones
goodLens Reed = reeds
goodLens Fence = borders

data Alignment = H | V deriving (Eq, Show)


data Border = Border {  _alignment ::  Alignment
                      , _isThere   :: Bool
                      } deriving (Eq)

makeLenses ''Border


data Tile = Tile { _building :: Maybe Building
                 , _tileanimals :: Maybe (Animal, Integer)
                 , _trough :: Bool
                 , _isExpansion :: Bool
                 } deriving (Eq)

makeLenses ''Tile

emptyTile :: Tile
emptyTile = Tile { _building = Nothing
                 , _tileanimals = Nothing
                 , _trough = False
                 , _isExpansion = False
                               }

data Farm = Farm { _tiles :: [[Tile]]
                 , _vborders :: [[Border]]
                 , _hborders :: [[Border]]
                 }
makeLenses ''Farm


farmcols :: Farm -> Integer
farmcols farm = toInteger $ length $ head $  farm ^. tiles 

farmrows :: Farm -> Integer
farmrows farm = toInteger $ length $  farm ^. tiles 

type Measurements = (Integer, Integer)

capitalize "" = ""
capitalize (s:rs) = toUpper s : map toLower rs


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
  volume v = (fromIntegral $ maximum . map length . lines $  show v
             , fromIntegral $ length  . lines $ show v)


emptyFarm :: Farm
emptyFarm = Farm
            (replicate 3 $ replicate 2 emptyTile)
            (replicate 3 $ replicate 3 $ Border V False)
            (replicate 4 $ replicate 2 $ Border H False)

emptyFarmWrows rows = Farm
            (replicate 3 $ replicate rows emptyTile)
            (replicate 3 $ replicate (rows + 1) $ Border V False)
            (replicate 4 $ replicate (rows) $ Border H False)
            
showHorizBorder :: [Border] -> String
showHorizBorder hs = "+" ++ intercalate "+" (map show hs) ++ "+"


showFarmLine :: [Tile] -> [Border] -> String
showFarmLine ts bs = (init . unlines .  map concat . joinLines) lns
  where tlines = map (lines . show) ts
        blines = map (lines . show) bs
        strs (b:bs) (a:as) = [b,a] ++ strs bs as
        strs [a] [] = [a]
        strs [] [] = error "not enough borders"
        lns = strs blines tlines

joinLines :: [[b]] -> [[b]]
joinLines = lss
  where lss as | any null as = []
        lss as = map head as : lss (map tail as)

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


type UnitBoardTile = Maybe Color
type MonoBoardTile = Either Color Integer
type DuoBoardTile = Either Color (Integer, Integer)
type SpecialBoardTile =  (Maybe Color, Maybe Color)


data Gameboard = Gameboard {
    _smallForest  :: MonoBoardTile
  , _bigForest    :: MonoBoardTile
  , _smallQuarry  :: MonoBoardTile
  , _bigQuarry    :: MonoBoardTile
  , _expand       :: MonoBoardTile
  , _woodFence    :: UnitBoardTile
  , _stoneWall    :: UnitBoardTile
  , _resources    :: UnitBoardTile
  , _buildStall   :: UnitBoardTile
  , _buildTroughs :: UnitBoardTile
  , _buildStable   :: UnitBoardTile
  , _specialBuilding :: SpecialBoardTile
  , _millpond :: DuoBoardTile
  , _pigsAndSheep :: DuoBoardTile
  , _cowsAndPigs :: DuoBoardTile
  , _horsesAndSheep :: DuoBoardTile
                           }
makeLenses ''Gameboard


append :: MonadState [a] m => [a] -> m ()
append s = id %= flip (++) s


data TileType = Unit
              | Mono
              | Duo
              | Special
              deriving (Eq, Show)


data GameBoardTile =   SmallForest
                     | BigForest
                     | SmallQuarry
                     | BigQuarry
                     | Expand
                     | WoodFence
                     | StoneWall
                     | Resources
                     | BuildStall
                     | BuildTroughs
                     | BuildStable
                     | SpecialBuilding
                     | Millpond
                     | PigsAndSheep
                     | CowsAndPigs
                     | HorsesAndSheep
                     deriving (Eq, Show)


resourcesOnTile :: GameBoardTile -> [String]
resourcesOnTile SmallForest = [show Wood]
resourcesOnTile BigForest = [show Wood]
resourcesOnTile SmallQuarry = [show Stone]
resourcesOnTile BigQuarry = [show Stone]
resourcesOnTile Expand = ["Fences"]
resourcesOnTile WoodFence = []
resourcesOnTile StoneWall = []
resourcesOnTile Resources = []
resourcesOnTile BuildStall = []
resourcesOnTile BuildTroughs = []
resourcesOnTile BuildStable = []
resourcesOnTile SpecialBuilding = []
resourcesOnTile Millpond =  [show Reed ++ "s", show Sheep]
resourcesOnTile PigsAndSheep = [ show Pig ++ "s", show Sheep]
resourcesOnTile CowsAndPigs =  [ show Cow ++"s", show Pig ++ "s"]
resourcesOnTile HorsesAndSheep = [show Horse ++ "s", show Sheep]

tileType :: GameBoardTile -> TileType
tileType SmallForest = Mono
tileType BigForest = Mono
tileType SmallQuarry = Mono
tileType BigQuarry = Mono
tileType Expand = Mono

tileType WoodFence = Unit
tileType StoneWall = Unit
tileType Resources = Unit
tileType BuildStall = Unit
tileType BuildTroughs = Unit
tileType BuildStable = Unit

tileType Millpond = Duo
tileType PigsAndSheep = Duo
tileType CowsAndPigs = Duo
tileType HorsesAndSheep = Duo

tileType SpecialBuilding = Special


gameBoardLayout :: [[GameBoardTile]]
gameBoardLayout = [ [SmallForest, BigForest, SmallQuarry, BigQuarry]
                  , [WoodFence, StoneWall, Resources, Expand]
                  , [BuildStall, BuildTroughs, Millpond, PigsAndSheep]
                  , [BuildStable, SpecialBuilding, CowsAndPigs, HorsesAndSheep]
                  ]


monoTileLens
  :: Functor f =>
     GameBoardTile
     -> (MonoBoardTile -> f MonoBoardTile) -> Gameboard -> f Gameboard
monoTileLens SmallForest = smallForest
monoTileLens BigForest = bigForest
monoTileLens SmallQuarry = smallQuarry
monoTileLens BigQuarry = bigQuarry
monoTileLens Expand = expand

unitTileLens
  :: Functor f =>
     GameBoardTile
     -> (UnitBoardTile -> f UnitBoardTile) -> Gameboard -> f Gameboard
unitTileLens WoodFence = woodFence
unitTileLens StoneWall = stoneWall
unitTileLens Resources = resources
unitTileLens BuildStall = buildStall
unitTileLens BuildTroughs = buildTroughs
unitTileLens BuildStable = buildStable

duoTileLens
  :: Functor f =>
     GameBoardTile
     -> (DuoBoardTile -> f DuoBoardTile) -> Gameboard -> f Gameboard
duoTileLens Millpond = millpond
duoTileLens PigsAndSheep = pigsAndSheep
duoTileLens CowsAndPigs = cowsAndPigs
duoTileLens HorsesAndSheep = horsesAndSheep


specialTileLens
  :: Functor f =>
     GameBoardTile
     -> (SpecialBoardTile -> f SpecialBoardTile)
     -> Gameboard -> f Gameboard
specialTileLens SpecialBuilding = specialBuilding

-- Descriptions of the actions for the gameboard tiles
instrs :: GameBoardTile -> String
instrs SmallForest = unlines ["Get Starting marker"] ++ "and take"
instrs WoodFence = unlines ["","unlimited"] ++ "1 Wood -> Fence"
instrs StoneWall = unlines ["2 x Fence", "also unlimited"] ++ "2 Stone -> Fence"
instrs Resources = unlines ["+1 Wood","+1 Stone"] ++ "+1 Reed"
instrs Expand = unlines ["Expand your farm"]
instrs BuildStall = unlines ["3 Stone 1 Reed", "->"] ++ "Stall"
instrs BuildStable = unlines ["5 Wood or 5 Stone ", "->"] ++ "Stall -> Stables"
instrs BuildTroughs = unlines ["+1 Trough", "also unlmited"] ++ "3 Wood -> Trough"
instrs SpecialBuilding = unlines ["Build a"]++ "special building"
instrs _ = unlines [""] ++ "Take"

showGameboardTile :: Gameboard -> GameBoardTile -> String
showGameboardTile board tile =
  unlines  [show tile ++ ":"
           , case tileType tile of
           Unit -> case board ^. unitTileLens tile of
             Just c -> unlines ["",show c ++ " worker" ]
             Nothing -> unlines [instrs tile]
           Mono -> case board ^. monoTileLens tile of
             Left c -> unlines ["",show c ++ " worker"]
             Right i ->  unlines [instrs tile] ++ unwords [show i, head reses]
           Duo -> case board ^. duoTileLens tile of
             Left c -> unlines ["",show c ++ " worker"]
             Right (a,b) -> unlines [instrs tile] ++ unwords  [show a , head reses, "and",
                                      show b ,last reses]
           Special -> case board ^. specialTileLens tile of
             (Nothing, Nothing) -> unlines ["",instrs tile]
             (Just a, Nothing) -> unlines [instrs tile] ++ unwords  [show a ,"worker"]
             (Just a, Just b) -> unlines ["", unwords  [show a,"and", show b, "workers"]]
          ]
  where reses = resourcesOnTile tile


showBoard board = ers
  where ers = map (map (showGameboardTile board)) gameBoardLayout

center :: Int -> String -> String
center max s | even diff = replicate hdiff ' ' ++ s ++ replicate hdiff ' '
           | odd diff = replicate (hdiff + 1) ' ' ++ s ++ replicate hdiff ' '
  where diff = max - length s
        hdiff = diff `div` 2

-- print the gameboard borders
gameBoardBorder :: Int -> Int -> [Char]
gameBoardBorder maxlen num =
  concat ["+",intercalate "+" $ replicate  num $ replicate maxlen '-', "+","\n"]

instance Show Gameboard where
  show b = gameBoardBorder maxlen num ++
           (concatMap ((++ gameBoardBorder maxlen num) . unlines . f . joinLines . map lines)
            . showBoard)  b
    where f  =  map (( "|" ++) . k maxlen)
          k len = concatMap ((++ "|") . center len)
          maxlen  = maximum $ map (maximum . map (maximum . map length . lines))  $ showBoard b
          appendIfShorter len a | length a < len = a ++ replicate  (len - length a) ' ' 
          appendIfShorter _ a = a
          num = length $ head gameBoardLayout

emptyBoard = Gameboard {
    _smallForest = Left No
  , _bigForest = Left No
  , _smallQuarry = Left No
  , _bigQuarry = Left No
  , _expand = Left No
  , _millpond = Left No
  , _pigsAndSheep = Left No
  , _cowsAndPigs = Left No
  , _horsesAndSheep = Left No
  , _resources = Nothing
  , _woodFence = Nothing
  , _stoneWall = Nothing
  , _buildStall = Nothing
  , _buildTroughs = Nothing
  , _buildStable = Nothing
  , _specialBuilding = (Nothing,Nothing)
                       }

-- The remove Worker functions remove worker tokens from
-- gameboard tiles

removeWorkerOfSingle :: MonoBoardTile -> MonoBoardTile
removeWorkerOfSingle (Left _) = Right 0
removeWorkerOfSingle a = a

removeWorkerOfAnimals :: DuoBoardTile -> DuoBoardTile
removeWorkerOfAnimals (Left _) = Right (0, 0)
removeWorkerOfAnimals a = a

removeWorkerOfTile :: GameBoardTile -> Gameboard -> Gameboard
removeWorkerOfTile t b
  | tileType t == Mono = b & monoTileLens t .~ (Right 0)
  | tileType t == Unit = b & unitTileLens t .~ Nothing
  | tileType t == Duo = b &  duoTileLens t .~ (Right (0,0))
  | tileType t == Special = b & specialTileLens t .~ (Nothing, Nothing)

-- Remove all worker tokens from the gameboard
takeWorkers :: Gameboard -> Gameboard
takeWorkers board = board &~ do
  mapM_ (\t -> id %= removeWorkerOfTile t)
    $ concat $ gameBoardLayout


-- Workers must have been removed prior.
refillAnimals :: DuoBoardTile -> DuoBoardTile
refillAnimals (Right (0,0)) = Right (1,0)
refillAnimals (Right (a,b)) = Right (a,b+1)

-- Refill the gameboard with the goods that go with
-- each space
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

data Phase = WorkPhase | BreedingPhase | Finished deriving (Eq)

instance Show Phase where
  show WorkPhase = "Work phase"
  show BreedingPhase = "Breeding phase"
  show Finished = "Game is over!"

-- Definition of a game instance
data Agricola = Agricola { _red :: Player
                         , _blue :: Player
                         , _global :: GlobalSupply
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
                         , _global = emptyGlobal
                         , _board = emptyBoard
                         , _starting = Red
                         , _whoseTurn = Red
                         , _hasPlacedWorker = False
                         , _message = ""
                         , _phase = BreedingPhase
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
              | ChooseAnimal Animal
              | PlaceExpand Bool
              | PlaceTrough Integer Integer
              | PlaceBuilding Building Integer Integer
              | SpendResources Good Integer
              | StartBuildingTroughs
              | StartBuildingStoneWalls
              | StartBuildingWoodFences
              | SetMessage String
              | StartBuilding Building
              | MultiAction [Action]
            deriving (Eq)


instance Show Action where
  show DoNothing = "do nothing"
  show (PlaceBorder al n m) = case al of
    V -> "place vertical border on (" ++ show n ++ ", " ++ show m ++ ")"
    H -> "place horizontal border on (" ++ show n ++ ", " ++ show m ++")"
  show (FreeAnimal a) =  "free a " ++ map toLower (show a)
  show EndTurn = "end turn"
  show EndPhase = "end phase"
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
  show (TakeAnimal n m) = "take animal from tile (" ++ show n ++", " ++ show m ++ ")"
  show (PlaceAnimal a n m) = "place " ++ map toLower (show a) ++ " on tile (" ++ show n ++", " ++ show m ++ ")"
  show (PlaceTrough n m) = "place trough on tile (" ++ show n ++", " ++ show m ++")"
  show (PlaceBuilding b n m) = "place " ++ map toLower (show b) ++ " on tile (" ++ show n ++", " ++ show m ++")"
  show (SpendResources good n) = "spend " ++ show n ++ " of your " ++  map toLower (show good)
  show (SetMessage str) = str
  show StartBuildingTroughs = "Start building troughs"
  show StartBuildingWoodFences = "Start building wood fences"
  show StartBuildingStoneWalls = "Start building stone walls"
  show (StartBuilding b ) = "Build " ++ show b
  show (ChooseAnimal a) = "Chose " ++ map toLower (show a)
  show (MultiAction as) = "Cannot do all of " ++ unwords (map show as)
  show (PlaceExpand True) = "Place expansion on left side."
  show (PlaceExpand False) = "Place expansion on left side."

instance Show Building where
  show Stall             = "Stall"
  show Stable            = "Stable"
  show Cottage           = "Cottage"
  show HalfTimberedHouse = "HalfT House"
  show Storage           = "Storage"
  show Shelter           = "Shelter"
  show OpenStable        = "Open stable"


instance Show Animal where
  show Sheep = "Sheep"
  show Pig  = "Pig"
  show Cow  = "Cow"
  show Horse = "Horse"

-- Fill the tiles with spaces to make them equal sized
fillWithW :: String -> String
fillWithW s = concat $ replicate tileStrLength s

fillWithH :: String -> String
fillWithH s = concat $ replicate tileStrHeight s

tileStrLength :: Int
tileStrLength = maximum $ map length $ lines $ show emptyTile

tileStrHeight :: Int
tileStrHeight = length $ lines $ show emptyTile

instance Show Border where
  show (Border V False) =  fillWithH "|\n"
  show (Border V True) = fillWithH "+\n"
  show (Border H False) = fillWithW "-"
  show (Border H True) = fillWithW "+"


showTro :: Bool -> String
showTro False = center lenLongestTileItem ""
showTro True = center lenLongestTileItem "Trough"

showAn :: Maybe (Animal, Integer) -> String
showAn Nothing = center lenLongestTileItem "No Animals"
showAn (Just (an, count)) = center lenLongestTileItem $ show count ++ " " ++ show an

-- the half-timbered house has the longest name
lenLongestTileItem = length $ show HalfTimberedHouse

showBu :: Maybe Building -> String
showBu Nothing = center lenLongestTileItem ""
showBu (Just bu) = center lenLongestTileItem $ show bu

minLengthStringPrepend :: Int -> [Char] -> [Char]
minLengthStringPrepend len str | length str >= len = str
minLengthStringPrepend len str = replicate diff ' ' ++ str
  where
    diff = len - length str

instance Show Tile where
  show Tile {_building = bu, _tileanimals = an, _trough = tro} = items
    where
      stuff = [showBu bu ,showAn an , showTro tro]
      maxl = maximum $ map length stuff
      items = unlines $ map (minLengthStringPrepend maxl) stuff


instance Volume Tile
instance Volume Farm
instance Volume Building
instance Volume Border
instance Volume Gameboard


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

-- At the beginning of the game each farm is empty except
-- for one cottage
startingFarm :: Farm
startingFarm = emptyFarm & tile 2 0 . building .~ Just Cottage

player :: Color -> Lens' Agricola Player
player Red = red
player Blue = blue
player No = error "Attempted to use no player"

-- Initialize player
initPlayer :: Player -> Player
initPlayer player = player &~ do
  (supply . borders) .= 9
  workers .= 3
  farm .= startingFarm

-- Number of expansions in use
totalNumExpansions agri = numberOfExpansions Red agri + numberOfExpansions Blue agri

-- Offset for cursor actions
farmOffset :: Agricola -> Color -> Coord
farmOffset agri Red = (x,29)
  where x = max 0 $ middle - max (numexpR+1) 3 * expXVol  - (numexpB `div` 2)*expXVol
        expXVol = fst  (volume $ emptyFarmWrows 1) - 1
        numexpR = numberOfExpansions Red agri
        numexpB = numberOfExpansions Blue agri
        numexp = 1 + totalNumExpansions agri

farmOffset agri Blue = (x,fyr)
  where x = vxr + fxr + spacebetween
        (vxr,vyb) = volume $ agri ^. player Red . farm
        (fxr, fyr) = farmOffset agri Red
        numexpR = numberOfExpansions Red agri
        spacebetween = 24 `div` (numexpR + 1)

-- Minimum screen size needed to render game
minScreenSize :: Coord
minScreenSize = (47,123)

middle :: Integer
middle = snd minScreenSize `div` 2

boardOffset :: Coord
boardOffset = (x,8)
  where x = middle - (fst (volume emptyBoard) `div` 2)

controlsOffset :: Coord
controlsOffset = (2,2)


data Button =   StopButton
              | EndTurnButton
              | EndPhaseButton
              | ChoiceB Animal Good Building
              | PlaceAnimalButton
              | TakeAnimalButton
              | FreeAnimalButton
              | QuitButton
              | EmptyButton
              | CancelButton
              deriving (Eq)

instance Show Button where
  show StopButton = "Stop"
  show EndTurnButton = "End turn"
  show EndPhaseButton = "End phase"
  show (ChoiceB a g b) = show a ++ "/" ++ show g ++ "/" ++ show b
  show PlaceAnimalButton = "Place animal"
  show TakeAnimalButton = "Take animal"
  show FreeAnimalButton = "Free animal"
  show QuitButton = "Quit"
  show EmptyButton = ""
  show CancelButton = "Cancel"

defaultControls = [
  [StopButton, EndTurnButton, EndPhaseButton, PlaceAnimalButton, TakeAnimalButton, FreeAnimalButton]
  ,[ ChoiceB Sheep Wood HalfTimberedHouse
   , ChoiceB Cow Stone Storage
   , ChoiceB Horse Reed Shelter
   , ChoiceB Pig Fence OpenStable
   ,  CancelButton, QuitButton]
  ]


-- Number of expansions on the given player's farm
numberOfExpansions ::  Color -> Agricola -> Integer
numberOfExpansions col agri =
  toInteger $ sum $ map (fromEnum . _isExpansion . head) $ transpose ts
  where ts = agri ^. player col . farm . tiles

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


-- Functions to find which part of a farm has been clicked

farmMapLineLengths :: [Maybe (Either Tile Border)] -> [Integer]
farmMapLineLengths (Nothing:xs) = 1 : farmMapLineLengths xs
farmMapLineLengths (Just (Right b):xs) =
  fst (volume b): farmMapLineLengths xs
farmMapLineLengths (Just (Left t):xs)  =
  fst (volume t): farmMapLineLengths xs
farmMapLineLengths [] = []


farmMapLengths :: [[Maybe (Either Tile Border)]] -> [Integer]
farmMapLengths  = head .  map farmMapLineLengths

farmMapLineHeights :: [Maybe (Either Tile Border)] -> [Integer]
farmMapLineHeights (Nothing:xs) = 1 : farmMapLineHeights xs
farmMapLineHeights (Just (Right b):xs) =
  snd (volume b): farmMapLineHeights xs
farmMapLineHeights (Just (Left t):xs)  =
  snd (volume t): farmMapLineHeights xs
farmMapLineHeights [] = []


farmMapHeights :: [[Maybe (Either Tile Border)]] -> [Integer]
farmMapHeights =  map (head .farmMapLineHeights)
