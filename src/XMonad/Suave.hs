{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

-- | A suave borderless webkit panel.

module XMonad.Suave where

import           Control.Concurrent
import           Control.Monad
import           Data.Monoid
import           Graphics.UI.Gtk hiding (LayoutClass)
import           Graphics.UI.Gtk.WebKit.WebView
import qualified Graphics.X11.Types as X11 (Window)
import           XMonad hiding (Window)
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutModifier
import           XMonad.StackSet

-- | Suave instance.
newtype Suave = Suave Window

xmonadSuave :: (LayoutClass l X11.Window, Read (l X11.Window)) => (Suave -> XConfig l) -> IO ()
xmonadSuave f = do
  s <- suaveStart
  void (forkIO (xmonad (f s)))
  suaveMain

-- | Start up a Suave panel.
suaveStart :: IO Suave
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
  return (Suave window)

-- | Run the Suave panel.
suaveMain :: IO ()
suaveMain =
  mainGUI

-- | Setup the right panel layout for Suave.
suaveLayout :: ModifiedLayout Gaps (Choose Tall Full) a
suaveLayout = gaps [(U,40)] (Tall 1 (3/100) (1/2) ||| Full)

-- | Set the position of the Suave panel.
suaveStartupHook :: Suave -> X ()
suaveStartupHook (Suave suave) = withWindowSet $ \stackset -> do
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
