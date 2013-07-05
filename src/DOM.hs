-- | Document object model functions.

module DOM where

import FFI
import Prelude

--------------------------------------------------------------------------------
-- Elements

-- | DOM element.
data Element

-- | Get the element by id.
getElementById :: String -> Fay (Nullable Element)
getElementById = ffi "document.getElementById(%1)"

-- | Set the inner text.
setInnerText :: Element -> String -> Fay ()
setInnerText = ffi "%1.innerText = %2"
