{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | View for the client.

module XMonad.Suave.View where

import Data.Monoid
import Prelude (($),return)
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (span,style)
import Text.Blaze.Internal

-- | View for the panel.
page :: Html
page = do
  docType
  html $ do
    head $ do
      theme
    body $ do
      rhs

-- | CSS theme.
theme :: Html
theme = do
  link ! href "http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css"
       ! rel "stylesheet"
  style $ mconcat ["body {"
                  ,"font-size: 22px;"
                  ,"font-family: ubuntu;"
                  ,"background: #ff0000;"
                  ,"color: #bbbbbb;"
                  ,"height: 40px;"
                  ,"overflow: hidden;"
                  ,"padding: 0.3em;"
                  ,"margin: 0;"
                  ,"text-shadow: -0.08em -0.08em 0.01em #292929;"
                  ,"background: -webkit-linear-gradient(top, #444, #333) no-repeat #333;"
                  ,"-webkit-touch-callout: none;"
                  ,"-webkit-user-select: none;"
                  ,"-khtml-user-select: none;"
                  ,"-moz-user-select: none;"
                  ,"-ms-user-select: none;"
                  ,"user-select: none;"
                  ,"cursor: default;"
                  ,"}"
                  ,".indicator {"
                  ,"margin-right: 0.5em; visibility:visible"
                  ,"}"
                  ,".ip { display: none }"
                  ,"#wifi:hover .ip { display: inline }"
                  ,"#date {"
                  ,"}"
                  ,"#rhs {"
                  ,"float: right;"
                  ,"}"
                  ,"#center {"
                  ,"text-align:center; position: absolute; top:0;left:0;bottom:0;right:0; line-height: 40px"
                  ,"}"
                  ,"#lhs,#rhs{"
                  ,"visibility:hidden"
                  ,"}"
                  ,"#power-off {"
                  ,"cursor: pointer;"
                  ,"}"
                  ,"#power-off:hover {"
                  ,"color: #fff;"
                  ,"}"]

-- | Right-hand size.
rhs :: Html
rhs = do
  span !# "lhs" $ do
    span !# "i3" $ return ()
  span !# "center" $ do
    span !# "clockin" $ return ()
  span !# "rhs" $ do
    span !. "indicator" !# "date" $ return ()

-- | Class names.
(!.) :: Attributable h => h -> AttributeValue -> h
span !. value = span ! class_ value

-- | Class names.
(!#) :: Attributable h => h -> AttributeValue -> h
span !# value = span ! id value
