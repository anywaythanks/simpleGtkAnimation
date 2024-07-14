{-# LANGUAGE OverloadedStrings #-}

module MyApplicaton.Utils (numToString, calculateMargins, toCss, margins, applyCss, loadCss, loadFromData) where

import Data.String (IsString (fromString))
import GI.Gtk (IsCssProvider, IsStyleProvider, IsWidget, cssProviderLoadFromData, cssProviderLoadFromPath, cssProviderNew, styleContextAddProvider, widgetGetStyleContext)
import GI.Gtk.Constants

-- | Converts absolute x with the width of the widget
--   to margin relative to x and the width of the container
--   in which the widget is located.
--
-- The left and right multipliers respectively multiply the 
-- left and right half of the resulting width.
calculateMargins :: (Integral p1, Integral p2, Fractional b) => b -> b -> p1 -> p2 -> p1 -> p2 -> (b, b)
calculateMargins multL multR childX childWeight contaienerX containerWeight = (marginLeft', marginRight)
  where
    mul :: (Fractional a) => a -> a
    mul m = fromIntegral childWeight * (m - 1) / 2
    marginLeft = fromIntegral (childX - contaienerX)
    marginLeft' = marginLeft - mul multL
    marginRight = fromIntegral (containerWeight - childWeight) - marginLeft - mul multR

numToString :: (Show a, Num a, IsString s) => a -> s
numToString = fromString . show

toCss :: (Foldable t, Semigroup a, IsString a) => a -> t (a, a) -> a
toCss name properites = f properites <> "}"
  where
    f = foldr (\x acc -> acc <> concat x) (name <> " {")
    concat (name, value) = name <> ": " <> value <> ";"

margins :: (Show a1, Num a1, Semigroup a2, IsString a2) => a2 -> (a1, a2) -> (a1, a2) -> a2
margins name left right = toCss name [("margin-left", left'), ("margin-right", right')]
  where
    (vals, names) = unzip [left, right]
    vals' = numToString <$> vals
    [left', right'] = zipWith (<>) vals' names

-- NOT PURE --

applyCss provider to = do
  cont <- widgetGetStyleContext to
  styleContextAddProvider cont provider $ fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION

loadCss path = do
  cssP <- cssProviderNew
  cssProviderLoadFromPath cssP path
  pure cssP

loadFromData data' = do
  cssP <- cssProviderNew
  print data'
  cssProviderLoadFromData cssP data'
  pure cssP