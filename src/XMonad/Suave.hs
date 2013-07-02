{-# OPTIONS -Wall #-}

-- | A suave borderless webkit panel.

module XMonad.Suave where

import Control.Monad
import Data.Monoid
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import XMonad hiding (Window)
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier
import XMonad.StackSet

-- | Start up a Suave panel.
suaveStart :: IO Window
suaveStart = do
  void initGUI
  window <- windowNew
  vContainer <- vBoxNew False 0
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyNever
  webview <- webViewNew
  set window [containerChild := vContainer
             ,windowTitle    := suaveWindowTitle]
  boxPackStart vContainer scrolledWindow PackGrow 0
  set scrolledWindow [containerChild := webview]
  webViewLoadUri webview "file:///home/chris/.webkit-window.html"
  void (onDestroy window mainQuit)
  void (widgetShowAll window)
  return window

-- | Run the Suave panel.
suaveMain :: IO ()
suaveMain =
  mainGUI

-- | Setup the right panel layout for Suave.
suaveLayout :: ModifiedLayout Gaps (Choose Tall Full) a
suaveLayout = gaps [(U,40)] (Tall 1 (3/100) (1/2) ||| Full)

-- | Set the position of the Suave panel.
suaveStartupHook :: Window -> X ()
suaveStartupHook suave = withWindowSet $ \stackset -> do
  liftIO (windowResize suave
                       (head (map (fromIntegral . rect_width . screenRect . screenDetail)
                                  (screens stackset)))
                       40)

  return ()

-- | Ignore the Suave window as a panel.
suaveManageHook :: Query (Endo WindowSet)
suaveManageHook = composeAll [ title =? suaveWindowTitle --> doIgnore]

-- | Window title of the Suave panel.
suaveWindowTitle :: String
suaveWindowTitle = "xmonad-suave-panel"
