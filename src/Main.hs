{-# OPTIONS -Wall #-}

-- | Chris Done's XMonad program.

module Main where

import           Control.Monad
import qualified Data.Map                  as M
import           XMonad
import           XMonad.Actions.DeManage   (demanage)
import           XMonad.Config.Gnome       (gnomeConfig)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import           XMonad.Util.Run

-- | Main entry point.
main :: IO ()
main = do
  xmonad (ewmh gnomeConfig
               { terminal          = "gnome-terminal"
               , modMask           = mod4Mask
               , focusFollowsMouse = False
               , borderWidth       = 0
               , logHook           = fadeInactiveLogHook 0xbbbbbbbb
               , keys              = newKeys
               })
  where newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
        myKeys (XConfig{modMask=modm}) =
          [((modm,xK_d),withFocused demanage)
          ,((modm,xK_b),liftIO (void (spawnPipe "chromium-browser")))]
