module Main (main) where

import Customs (addProp, run)
import MyApplicaton.Utils (calculateMargins)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  defaultMain $ testGroup "Tests" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "(Calculate Margin Check)"
    [ QC.testProperty "Margin is always greater than zero" $ run testGreatZero,
      QC.testProperty "Adding one width with margin allows you to get another width" $ run testSum,
      QC.testProperty "Changes through multipliers are proportional" $ addProp $ run . testMult
    ]

testGreatZero :: Int -> Int -> Positive Int -> Positive Int -> Bool
testGreatZero chX contX (Positive chW) (Positive contW) = ml >= 0 && mr >= 0
  where
    (ml, mr) = calculateMargins 1 1 chX chW contX contW

testSum :: Int -> Int -> Positive Int -> Positive Int -> Bool
testSum chX contX (Positive chW) (Positive contW) =
  getWidth chW r == fromIntegral contW
  where
    r = calculateMargins 1 1 chX chW contX contW

testMult :: Positive Rational -> Int -> Int -> Positive Int -> Positive Int -> Bool
testMult (Positive p) chX contX (Positive chW) (Positive contW) =
  gw r1 == gw r2 / p
  where
    gw = getWidth (-contW)
    r1 = calculateMargins 1 1 chX chW contX contW
    r2 = calculateMargins p p chX chW contX contW

getWidth :: (Integral a1, Num a2) => a1 -> (a2, a2) -> a2
getWidth w (ml, mr) = mr + fromIntegral w + ml
