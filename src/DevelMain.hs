-- | For updating XMonad.

module DevelMain where

import Control.Concurrent
import Foreign.Store
import Main
import System.Process

-- | Start or restart xmonad.
update =
  do callCommand "killall xmonad"
     forkIO (callCommand "dist/build/xmonad/xmonad")
