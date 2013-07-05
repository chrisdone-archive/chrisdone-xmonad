{-# LANGUAGE OverloadedStrings #-}

-- | Server for providing information to the client.

module XMonad.Suave.Server where

import Paths_xmonad_chrisdone
import XMonad.Suave.Types
import XMonad.Suave.View

import Control.Monad.Trans
import Data.Default
import Data.Text.Lazy
import Fay
import Fay
import Fay.Compiler
import Fay.Compiler.Config
import Fay.Compiler.Debug
import Fay.Types
import Text.Blaze.Html.Renderer.Text
import Web.Scotty

-- | Start a web server for the web client window to load.
suaveServer :: IO ()
suaveServer =
  scotty suavePort
         (do get "/script"
                 (do dir <- io (fmap (++"/src") getDataDir)
                     fp <- io (getDataFileName "src/XMonad/Suave/Client.hs")
                     result <- io (compileFile (addConfigDirectoryIncludePaths [dir] def)
                                               fp)
                     case result of
                       Left err -> raise (pack (showCompileError err))
                       Right js -> text (pack js))
             get "/"
                 (html (renderHtml view)))

  where io = liftIO
