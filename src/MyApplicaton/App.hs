{-# LANGUAGE OverloadedStrings #-}

module MyApplicaton.App (app) where

import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import GI.Gtk
  ( Orientation (OrientationHorizontal, OrientationVertical),
    ReliefStyle (ReliefStyleNone),
    aspectFrameNew,
    buttonBoxNew,
    buttonNewWithLabel,
    buttonSetRelief,
    containerGetChildren,
    cssProviderLoadFromData,
    cssProviderNew,
    gridNew,
    labelNew,
    onButtonClicked,
    onButtonPressed,
    onButtonReleased,
    overlayAddOverlay,
    overlayNew,
    overlaySetOverlayPassThrough,
    radioButtonNewFromWidget,
    setContainerChild,
    widgetDestroy,
    widgetSetName,
    widgetShowAll
  )
import GI.Gtk.Objects.RadioButton (RadioButton (RadioButton), radioButtonNew)
import MyApplicaton.Animations
  ( MyAnimationWidget
      ( createAnimationWidget,
        preUpdateMyAnimationWidget,
        updateMyAnimationWidget
      ),
  )
import MyApplicaton.Utils (applyCss, loadCss, numToString, toCss)
import GI.Gdk (windowGetDeviceCursor)

app animation = do
  butP <- buttonNewWithLabel "+"
  butM <- buttonNewWithLabel "-"
  lb <- createAnimationWidget animation
  vbutbox <- buttonBoxNew OrientationVertical
  gridGenButt <- gridNew
  boxControlButt <- buttonBoxNew OrientationHorizontal
  overlay <- overlayNew

  overlayAddOverlay overlay lb
  setContainerChild overlay gridGenButt
  overlaySetOverlayPassThrough overlay lb True
  overlaySetOverlayPassThrough overlay gridGenButt False

  setContainerChild boxControlButt butP
  setContainerChild boxControlButt butM
  setContainerChild vbutbox overlay
  setContainerChild vbutbox boxControlButt

  z <- radioButtonNew ([] :: [RadioButton])
  lastWidget <- newIORef z
  i <- newIORef 0
  provider <- loadCss "src/styles/tests.css"

  onButtonClicked
    butP
    ( do
        index <- readIORef i
        (asf, but) <- squareButtonGen i (Just z)
        setContainerChild gridGenButt asf
        applyCss provider but
        applyCss provider asf
        onButtonPressed
          but
          ( do
              last <- readIORef lastWidget
              preUpdate lb overlay last but
          )
        onButtonReleased
          but
          ( do
              last <- readIORef lastWidget
              update lb overlay last but
              writeIORef lastWidget but
          )
        buttonSetRelief but ReliefStyleNone
        widgetShowAll asf
    )
  onButtonClicked
    butM
    ( do
        list <- containerGetChildren gridGenButt
        case list of
          [] -> pure ()
          child : _ -> do
            widgetDestroy child
            modifyIORef i (\x -> x - 1)
    )

  applyCss provider gridGenButt
  pure vbutbox
  where
    update = updateMyAnimationWidget animation
    preUpdate = preUpdateMyAnimationWidget animation

squareButtonGen i group = boxedSquare i $ radioButtonNewFromWidget group

squareLabelGen i t = boxedSquare i $ labelNew t

boxedSquare i widget = do
  j <- readIORef i
  modifyIORef i (+ 1)
  widget' <- widget
  asf <- aspectFrameNew Nothing 0.5 0.5 1 False
  setContainerChild asf widget'
  widgetSetName widget' $ "but" <> numToString j
  widgetSetName asf $ "asf" <> numToString j
  pure (asf, widget')

opacityProvider name op = do
  cssP <- cssProviderNew
  cssProviderLoadFromData cssP css
  pure cssP
  where
    op' = numToString op
    css = toCss name [("opacity", op')]
