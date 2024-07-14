module Customs (run, addProp) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC

run ::
  (Show t, Ord t, Num t, Arbitrary t, Testable prop) =>
  (t -> t -> Positive t -> Positive t -> prop) ->
  Property
run f = forAll genNatOrd $ \(contW@(Positive contW'), chW@(Positive chW')) ->
  forAll (genNatAndDistOrd contW' chW') $ \(chx, contX) ->
    f chx contX chW contW

addProp :: (Show t, Testable prop, Arbitrary t) => (t -> prop) -> Property
addProp f = forAll arbitrary $ \x -> f x

genNatOrd :: (Ord o, Arbitrary o) => Gen (o, o)
genNatOrd = swp `fmap` arbitrary `suchThat` uncurry (>)
  where
    swp (a, b)
      | a > b = (a, b)
      | otherwise = (b, a)

genNatAndDistOrd :: (Num o, Ord o, Arbitrary o) => o -> o -> Gen (o, o)
genNatAndDistOrd c1 c2 = do
  (a, b) <- genNatOrd
  pure (if dist a b > dist c1 c2 then (c1, c2) else (a, b))
  where
    dist a b = abs (a - b)
