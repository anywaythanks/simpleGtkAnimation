{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MyApplicaton.Animations (MyAnimationWidget (..), defaultAnimation) where

import Control.Monad (forM, forM_)
import GI.Gdk (getRectangleWidth, getRectangleX)
import GI.Gtk
  ( containerGetChildren,
    labelNew,overlaySetOverlayPassThrough,
    overlayAddOverlay,
    overlayNew,
    setContainerChild,
    styleContextAddClass,
    styleContextRemoveClass,
    widgetGetAllocation,
    widgetGetName,
    widgetGetStyleContext,
    widgetSetName,
  )
import qualified GI.Gtk as GI.Gtk.Objects.Overlay
import qualified GI.Gtk.Objects.Label
import qualified GI.Gtk.Objects.Widget
import MyApplicaton.Utils (applyCss, calculateMargins, loadCss, loadFromData, margins, numToString)
import GI.Gtk.Objects.Widget (widgetSetSensitive)

data MyAnimationWidget w c ch1 ch2 ch3 ch4 where
  MyAnimationWidget ::
    ( GI.Gtk.Objects.Widget.IsWidget w,
      GI.Gtk.Objects.Widget.IsWidget c,
      GI.Gtk.Objects.Widget.IsWidget ch1,
      GI.Gtk.Objects.Widget.IsWidget ch2,
      GI.Gtk.Objects.Widget.IsWidget ch3,
      GI.Gtk.Objects.Widget.IsWidget ch4
    ) =>
    { createAnimationWidget :: IO w,
      preUpdateMyAnimationWidget ::
        w ->
        c ->
        ch1 ->
        ch2 ->
        IO (),
      updateMyAnimationWidget ::
        w ->
        c ->
        ch3 ->
        ch4 ->
        IO ()
    } ->
    MyAnimationWidget w c ch1 ch2 ch3 ch4

defaultAnimation ::
  ( GI.Gtk.Objects.Widget.IsWidget c,
    GI.Gtk.Objects.Widget.IsWidget ch1,
    GI.Gtk.Objects.Widget.IsWidget ch2,
    GI.Gtk.Objects.Widget.IsWidget ch3,
    GI.Gtk.Objects.Widget.IsWidget ch4
  ) =>
  MyAnimationWidget GI.Gtk.Objects.Overlay.Overlay c ch1 ch2 ch3 ch4
defaultAnimation = MyAnimationWidget (create 5) preUpdate update

marginProvider n l r = loadFromData $ margins n l r

create :: Int -> IO GI.Gtk.Objects.Overlay.Overlay
create q = do
  provider <- loadCss "src/styles/tests.css"
  overlay <- overlayNew
  widgetSetSensitive overlay False
  forM_ [1 .. q] $ \i -> do
    lb <- labelNew Nothing
    c <- widgetGetStyleContext lb
    applyCss provider lb
    widgetSetSensitive lb False
    overlayAddOverlay overlay lb
    overlaySetOverlayPassThrough overlay lb True
    let name = "animation-child-" <> numToString i
    widgetSetName lb name
    styleContextAddClass c name
    styleContextAddClass c "animation-childs"
  c <- widgetGetStyleContext overlay
  widgetSetName overlay "main-container"
  styleContextAddClass c "main-container"
  pure overlay

calc ml mr ch cont = do
  chA <- widgetGetAllocation ch
  contA <- widgetGetAllocation cont
  [x1, x2] <- mapM getRectangleX [chA, contA]
  [w1, w2] <- mapM getRectangleWidth [chA, contA]
  let (ml, mr) = calculateMargins 1 1 x1 w1 x2 w2
  marginProvider "label" (ml, "px") (mr, "px")

preUpdate overlay widgetsContainer widgetLast widgetNow = do
  last <- widgetGetName widgetLast
  p <- calc 1 1 widgetLast widgetsContainer
  l <- containerGetChildren overlay
  forM_ l $ \lb -> do
    c <- widgetGetStyleContext lb
    applyCss p lb
    styleContextRemoveClass c $ "color_" <> last
    styleContextAddClass c "transition-none"
    styleContextRemoveClass c "a_opp"

update overlay widgetsContainer widgetLast widgetNow = do
  now <- widgetGetName widgetNow
  l <- containerGetChildren overlay
  p <- calc 1 1 widgetNow widgetsContainer
  forM_ l $ \lb -> do
    c <- widgetGetStyleContext lb
    styleContextRemoveClass c "transition-none"
    applyCss p lb
    styleContextAddClass c "a_opp"
    styleContextAddClass c $ "color_" <> now

-- where
--   calc lChX chX
--     | lChX > chX = calculateMargins m2 m1 chX -- <==
--     | otherwise = calculateMargins m1 m2 chX --  ==>
--     where
--       (m1, m2) = (2, 1)