module Main (main) where

import GI.Gtk
  ( WindowType (WindowTypeToplevel),
    mainQuit,
    onWidgetDestroy,
    setContainerBorderWidth,
    setContainerChild,
    widgetShow,
    windowNew, 
    widgetShowAll,
  )
import qualified GI.Gtk as GI (init, main)
import MyApplicaton.Animations (defaultAnimation)
import MyApplicaton.App (app)

main :: IO ()
main = do
  GI.init Nothing
  window <- windowNew WindowTypeToplevel
  onWidgetDestroy window mainQuit
  setContainerBorderWidth window 10
  application <- app defaultAnimation
  setContainerChild window application
  widgetShowAll window
  GI.main