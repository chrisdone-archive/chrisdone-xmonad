{-# LANGUAGE EmptyDataDecls #-}

-- | JQuery bindings.

module JQuery where

import Prelude
import FFI

data JQuery

-- | Make a query.
select :: String -> Fay JQuery
select = ffi "window.jQuery(%1)"

-- | Load some content into the given element set.
load :: String -> JQuery -> Fay JQuery
load = ffi "%2.load(%1)"
