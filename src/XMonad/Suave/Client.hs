-- | Client-side code.

module XMonad.Suave.Client where

import DOM
import ECMA
import FFI
import Prelude
import JQuery

-- | Main entry point for the script.
main :: Fay ()
main = do
  body <- getElementById "date"
  case body of
    Null -> return ()
    Nullable body -> do updateDate body
                        updateI3

-- | Update with some system-specific info.
updateI3 :: Fay ()
updateI3 = void $
  setInterval 1000 $ \_ -> do
    i3 <- select "#i3"
    get "/i3status" $ \html -> do
      setHtml html i3
      quality <- select "#wifi-quality"
      text <- getText quality
      setText (drop 1 text) quality

-- | Update the date display.
updateDate :: Element -> Fay ()
updateDate body = void $
  setInterval 1000 $ \_ -> do
    date <- newDate
    setInnerText body (toString date)

--------------------------------------------------------------------------------
-- Misc

-- | Ignore the return value of the given action.
void :: Fay a -> Fay ()
void m = m >> return ()
