-- | Client-side code.

module XMonad.Suave.Client where

import DOM
import ECMA
import FFI
import Prelude

-- | Main entry point for the script.
main :: Fay ()
main = do
  body <- getElementById "date"
  case body of
    Null -> return ()
    Nullable body -> updateDate body

-- | Update the date display.
updateDate :: Element -> Fay ()
updateDate body = void $ do
  date <- newDate
  setInnerText body (toString date)
  setTimeout 1000 (const (updateDate body))

--------------------------------------------------------------------------------
-- Misc

-- | Ignore the return value of the given action.
void :: Fay a -> Fay ()
void m = m >> return ()
