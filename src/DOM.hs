-- | Document object model functions.

module DOM where

import FFI
import Prelude

-- | DOM element.
data Element

-- | JS Date value.
data Date
instance ToString Date

-- | Built-in toString.
class ToString a

-- | Get the element by id.
getElementById :: String -> Fay (Nullable Element)
getElementById = ffi "document.getElementById(%1)"

-- | Set the inner text.
setInnerText :: Element -> String -> Fay ()
setInnerText = ffi "%1.innerText = %2"

-- | Get the date.
newDate :: Fay Date
newDate = ffi "new Date()"

-- | Built-in JS toString function.
toString :: ToString a => a -> String
toString = ffi "%1.toString()"
