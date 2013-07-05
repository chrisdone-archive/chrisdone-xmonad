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

-- | Load some content into the given element set.
get :: String -> (String -> Fay a) -> Fay ()
get = ffi "window.jQuery.get(%1,%2)"

-- | Set the HTML content.
setHtml :: String -> JQuery -> Fay JQuery
setHtml = ffi "%2.html(%1)"

-- | Set the text content.
setText :: String -> JQuery -> Fay JQuery
setText = ffi "%2.text(%1)"

-- | Set the text content.
getText :: JQuery -> Fay String
getText = ffi "%1.text()"
