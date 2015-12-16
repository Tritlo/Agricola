module Tests(tests) where


import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Data.List
import Test.QuickCheck.Monadic
import Control.Monad.Identity

import Agricola

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
    return Tile {_building = b, _tileanimals = ta, _trough = t}


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


legalLineVals :: Alignment -> [Integer]
legalLineVals V = [0..2]
legalLineVals H = [0..3]

leastNumRows :: Alignment -> NonNegative Integer -> Positive Integer
leastNumRows V (NonNegative m) = Positive (max m 1)
leastNumRows H (NonNegative m) = Positive (m + 1)

prop_borderGetSet :: Property
prop_borderGetSet = monadic runIdentity $ do
  al <- pick  arbitrary
  nb <- pick $ arbitrary `suchThat` ((== al) . _alignment)
  nnm@(NonNegative m) <- pick arbitrary
  n <- pick $ elements $ legalLineVals al
  farm  <- pick $ arbitraryFarmWithAtLeast $ leastNumRows al  nnm
  assert $ _border al n m (_setBorder al n m farm nb) == nb

checkBorders :: Test
checkBorders = testGroup "Borders" [
  testProperty "borderGetSet" prop_borderGetSet
                                   ]

tests :: IO [Test]
tests = return [checkBorders]
