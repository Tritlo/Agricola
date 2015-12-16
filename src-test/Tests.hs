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
prop_tileGetSet :: Property
prop_tileGetSet = monadic runIdentity $ do
  t <- pick arbitrary
  pm@(Positive m) <- pick arbitrary
  n <- pick $ elements [0..2]
  farm <- pick $ arbitraryFarmWithAtLeast pm
  assert $ _tile n (m-1) (_setTile n (m-1) farm t) == t

checkTiles :: Test
checkTiles = testGroup "Tiles" [
  testProperty "tileGetSet" prop_tileGetSet
                               ]
{-
emptyFarm :: Farm
emptyFarm = Farm
            (replicate 3 $ replicate 2 emptyTile)
            (replicate 3 $ replicate 3 $ Border V False)
            (replicate 4 $ replicate 2 $ Border H False)
startingFarm :: Farm
startingFarm = emptyFarm & tile 2 0 . building .~ Just Cottage
-}
instance Arbitrary Color where
  arbitrary = elements [Red,Blue]

instance Arbitrary GlobalSupply where
  arbitrary = do
    (NonNegative t) <- arbitrary
    (NonNegative s) <- arbitrary
    (NonNegative e) <- arbitrary
    (NonNegative g) <- arbitrary
    (NonNegative ht) <- arbitrary
    (NonNegative st) <- arbitrary
    (NonNegative sh) <- arbitrary
    (NonNegative os) <- arbitrary
    return $ GlobalSupply t s e g ht st sh os


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

instance Arbitrary Player where
  arbitrary = do
    f <- arbitrary
    s <- arbitrary
    (NonNegative w) <- arbitrary
    c <- arbitrary
    return $ Player f s w c

instance Arbitrary Phase where
  arbitrary = elements [WorkPhase,BreedingPhase,Finished]

instance Arbitrary Gameboard where
  arbitrary = do
    sf <- arbitrary
    bf <- arbitrary
    sq <- arbitrary
    bq <- arbitrary
    ex <- arbitrary
    wf <- arbitrary
    sw <- arbitrary
    re <- arbitrary
    bs <- arbitrary
    bt <- arbitrary
    bst <- arbitrary
    sb <- arbitrary
    mp <- arbitrary
    ps <- arbitrary
    cp <- arbitrary
    hs <- arbitrary
    return $ Gameboard sf bf sq bq ex wf sw re bs bt bst sb mp ps cp hs

instance Arbitrary Agricola where
  arbitrary = do
    r <- arbitrary
    b <- arbitrary
    g <- arbitrary
    bo <- arbitrary
    st <- arbitrary
    wh <- arbitrary
    hpw <- arbitrary
    ph <- arbitrary
    return $ Agricola r b g bo st wh hpw "" ph

{-
initPlayer :: Player -> Player
initPlayer player = player &~ do
  (supply . borders) .= 9
  workers .= 3
  farm .= startingFarm


hasBorders :: Agricola -> Color -> Bool
hasBorders agri color = agri ^. (player color . supply . borders) >= 1

freeSpace :: Agricola -> Color -> Alignment -> Integer -> Integer -> Bool
freeSpace agri color al n m = not $ agri ^.
                              (player color . farm . border al n m . isThere )
-}
tests :: IO [Test]
tests = return [checkBorders,checkTiles]
