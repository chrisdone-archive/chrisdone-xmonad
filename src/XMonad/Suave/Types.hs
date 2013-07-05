-- | All types of the project.

module XMonad.Suave.Types where

import Graphics.UI.Gtk

-- | Suave instance.
newtype Suave = Suave Window

-- | Suave port for the server.
suavePort :: Int
suavePort = 8888
