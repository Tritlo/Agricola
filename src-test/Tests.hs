module Tests(tests) where


import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Data.List
import Test.QuickCheck.Monadic
import Control.Monad.Identity

import Agricola
import Update
import Data.Maybe

instance Arbitrary Building where
  arbitrary = elements [Stall,Stable,Cottage
                       ,HalfTimberedHouse,Storage,Shelter,OpenStable]


instance Arbitrary Animal where
  arbitrary = elements [Sheep,Pig,Cow,Horse]

instance Arbitrary Alignment where
  arbitrary = elements [H,V]


instance Arbitrary Border where
  arbitrary = do al <- arbitrary
                 there <- arbitrary
                 return Border {_alignment = al, _isThere = there} 


instance Arbitrary Tile where
  arbitrary = do
    b <- arbitrary
    ta <- arbitrary `suchThat` maybe True ((> 0) . snd)
    t <- arbitrary
    e <- arbitrary
    return Tile {_building = b, _tileanimals = ta, _trough = t, _isExpansion = e}


borderWithAl al = arbitrary `suchThat` ((al == ) ._alignment)

farmTiles numrows = vector

instance Arbitrary Farm where
  arbitrary = do
    n <- arbitrary :: Gen (Positive Integer)
    arbitraryFarmWithAtLeast n


arbitraryFarmWithAtLeast :: Positive Integer -> Gen Farm
arbitraryFarmWithAtLeast (Positive nrows) = do
    numrows <- fromInteger <$>  (arbitrary :: Gen Integer) `suchThat` (>= nrows)
    ts <- vectorOf 3 (vector numrows)
    vs <- vectorOf 3 (vectorOf (numrows + 1) $ borderWithAl V)
    hs <- vectorOf 4 (vectorOf numrows  $  borderWithAl H)
    return Farm {_tiles = ts, _vborders = vs , _hborders =hs}


legalLineVals :: Alignment -> (Integer,Integer)
legalLineVals V = (0,2)
legalLineVals H = (0,3)

leastNumRows :: Alignment -> NonNegative Integer -> Positive Integer
leastNumRows V (NonNegative m) = Positive (max m 1)
leastNumRows H (NonNegative m) = Positive (m + 1)

prop_borderGetSet :: Property
prop_borderGetSet = monadic runIdentity $ do
  al <- pick  arbitrary
  nb <- pick $ arbitrary `suchThat` ((== al) . _alignment)
  nnm@(NonNegative m) <- pick arbitrary
  n <- pick $ choose $ legalLineVals al
  farm  <- pick $ arbitraryFarmWithAtLeast $ leastNumRows al  nnm
  assert $ _border al n m (_setBorder al n m farm nb) == nb

checkBorders :: Test
checkBorders = testGroup "Borders" [
  testProperty "borderGetSet" prop_borderGetSet
                                   ]
prop_tileGetSet :: Property
prop_tileGetSet = monadic runIdentity $ do
  t <- pick arbitrary
  pm@(Positive m) <- pick arbitrary
  n <- pick $ choose (0,2)
  farm <- pick $ arbitraryFarmWithAtLeast pm
  assert $ _tile n (m-1) (_setTile n (m-1) farm t) == t

checkTiles :: Test
checkTiles = testGroup "Tiles" [
  testProperty "tileGetSet" prop_tileGetSet
                               ]
instance Arbitrary Color where
  arbitrary = elements [Red,Blue]

instance Arbitrary Animals where
  arbitrary = do
    (NonNegative s) <- arbitrary
    (NonNegative p) <- arbitrary
    (NonNegative c) <- arbitrary
    (NonNegative h) <- arbitrary
    return $ Animals s p c h
    
instance Arbitrary Supply where
  arbitrary = do
    (NonNegative b) <- arbitrary
    (NonNegative w) <- arbitrary
    (NonNegative s) <- arbitrary
    (NonNegative r) <- arbitrary
    a <- arbitrary
    return $ Supply b w s r a



maybeSensibleAction :: Agricola -> Gen Action
maybeSensibleAction agri
 | _phase agri == Finished = return $ SetMessage $ finalScore agri
maybeSensibleAction agri
  | isNothing (isProblem agri EndPhase) = return EndPhase
maybeSensibleAction agri
  | isNothing (isProblem agri EndTurn) = return EndTurn
maybeSensibleAction agri
  | hasAnimalsInSupply agri = do
      an <- arbitrary
      let col = _whoseTurn agri
      let pf | col == Red = _farm $ _red  agri
             | otherwise = _farm $ _blue agri
      n <- choose (0,farmrows pf)
      m <- choose (0,farmcols pf)
      elements [FreeAnimal an, PlaceAnimal an n m]
maybeSensibleAction _ = arbitrary

instance Arbitrary Agricola where
  arbitrary = do
    agri <- frequency [(1, return startingState), (1000 , arbitrary)]
    sensb <- maybeSensibleAction agri
    return $ fromJust $ tryTakeAction agri sensb


instance Arbitrary Good where
  arbitrary = elements [Stone, Wood, Reed]

instance Arbitrary Action where
  arbitrary = do
    al <- arbitrary
    al1 <- arbitrary
    al2 <- arbitrary
    n <- choose (0,5)
    n1 <- choose (0,5)
    n2 <- choose (0,5)
    m <- choose (0,3)
    m1 <- choose (0,3)
    m2 <- choose (0,3)
    animal <- arbitrary
    build <- arbitrary
    bool <- arbitrary
    frequency [
               (30 , return $ MultiAction [StartBuildingStoneWalls
                                          , PlaceBorder al n m
                                          , PlaceBorder al1 n1 m1
                                          , SpendResources Stone 2
                                          , PlaceBorder al2 n2 m2
                                          ])
               , (30 , return $ MultiAction [StartBuildingWoodFences
                                          , SpendResources Wood 1
                                          , PlaceBorder al n m
                                          , SpendResources Wood 1
                                          , PlaceBorder al1 n1 m1
                                          , SpendResources Wood 1
                                          , PlaceBorder al2 n2 m2
                                          ])
              , (5 , return $ PlaceBorder al n m)
              , (5 , return  TakeResources)
              , (5 , return  TakeSmallForest)
              , (5 , return  TakeBigForest)
              , (5 , return  TakeSmallQuarry)
              , (5 , return  TakeBigQuarry)
              , (5, return $ MultiAction [TakeMillpond, PlaceAnimal Sheep n m])
              , (5, return $ MultiAction [TakePigsAndSheep, PlaceAnimal Pig n m])
              , (5, return $ MultiAction [TakeHorsesAndSheep, PlaceAnimal Horse n m])
              , (5, return $ MultiAction [TakeCowsAndPigs, PlaceAnimal Cow n m])
              , (5 , return  TakeCowsAndPigs)
              , (5 , return  TakeHorsesAndSheep)
              , (5 , return $ TakeAnimal n m)
              , (10 , return $ PlaceAnimal animal n m)
              , (5 , return $ PlaceTrough n m )
              , (5, return $ MultiAction [TakeExpand, PlaceExpand bool])
              , (20 , return $ MultiAction [StartBuilding build, PlaceBuilding build n m] )
              ]
tests :: IO [Test]
tests = return [checkBorders,checkTiles]
