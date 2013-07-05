-- | ECMA standard functions.

module ECMA where

import FFI
import Prelude

--------------------------------------------------------------------------------
-- Date

-- | JS Date value.
data Date
instance ToString Date

-- | Get the date.
newDate :: Fay Date
newDate = ffi "new Date()"

--------------------------------------------------------------------------------
-- ToString

-- | Built-in toString.
class ToString a

-- | Built-in JS toString function.
toString :: ToString a => a -> String
toString = ffi "%1.toString()"

--------------------------------------------------------------------------------
-- Timers

data Timer

-- | setInterval except the calling function gets the timer as an
-- | argument so the interval can be cancelled from within it.
setInterval :: Int -> (Timer -> Fay a) -> Fay Timer
setInterval = ffi "(function (f,i) { var id = window.setInterval(function () { f(id); }, i); return id; })(%2,%1)"

clearInterval :: Timer -> Fay ()
clearInterval = ffi "window['clearInterval'](%1)"

-- | setTimeout except the calling function gets the timer as an
-- | argument. Primarily for symmetry with setInterval.
setTimeout :: Int -> (Timer -> Fay a) -> Fay Timer
setTimeout = ffi "(function (f,i) { var id = window.setTimeout(function () { f(id); }, i); return id; })(%2,%1)"

-- | Delete a timeout.
clearTimeout :: Timer -> Fay ()
clearTimeout = ffi "window['clearTimeout'](%1)"
