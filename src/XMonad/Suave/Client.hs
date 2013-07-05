-- | Client-side code.

module XMonad.Suave.Client where

import DOM
import FFI
import Prelude

-- | Main entry point for the script.
main :: Fay ()
main = do
  body <- getElementById "date"
  case body of
    Null -> return ()
    Nullable body -> do
      date <- newDate
      setInnerText body (toString date)
