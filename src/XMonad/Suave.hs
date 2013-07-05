{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

-- | A suave borderless webkit panel.

module XMonad.Suave
  (xmonadSuave
  ,suaveStart
  ,suaveMain
  ,suaveLayout
  ,suaveStartupHook
  ,suaveManageHook)
  where

import           XMonad.Suave.Types
import           XMonad.Suave.Window
import           XMonad.Suave.Server

import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.Gtk hiding (LayoutClass)
import qualified Graphics.X11.Types as X11 (Window)
import           XMonad hiding (Window)

xmonadSuave :: (LayoutClass l X11.Window, Read (l X11.Window)) => (Suave -> XConfig l) -> IO ()
xmonadSuave f = do
  s <- suaveStart
  void (forkIO (xmonad (f s)))
  suaveMain

-- | Run the Suave panel.
suaveMain :: IO ()
suaveMain = do
  void (forkIO suaveServer)
  mainGUI
