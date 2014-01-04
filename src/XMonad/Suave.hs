{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

-- | A suave borderless webkit-based panel.

module XMonad.Suave
  (xmonadSuave
  ,suaveStart
  ,suaveLayout
  ,suaveStartupHook
  ,suaveManageHook)
  where

import           XMonad.Suave.Types
import           XMonad.Suave.Window

import           Control.Concurrent
import           Control.Monad
import           Data.Monoid
import           Graphics.UI.Gtk (mainGUI)
import           Graphics.UI.Gtk (windowResize)
import qualified Graphics.X11.Types as X11 (Window)
import           XMonad hiding (Window)
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutModifier
import           XMonad.StackSet

-- | Launch XMonad with the Suave panel.
xmonadSuave :: (LayoutClass l X11.Window, Read (l X11.Window)) => (Suave -> XConfig l) -> IO ()
xmonadSuave f = do
  s <- suaveStart
  void (forkIO (xmonad (f s)))
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

-- | Ignore the Suave window as a panel.
suaveManageHook :: Query (Endo WindowSet)
suaveManageHook = composeAll [ title =? suaveWindowTitle --> doIgnore]
