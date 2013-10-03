{-# LANGUAGE OverloadedStrings #-}

-- | Server for providing information to the client.

module XMonad.Suave.Server where

import Paths_xmonad_chrisdone
import XMonad.Suave.Types
import XMonad.Suave.View
import Control.Monad.Trans
import Data.Default
import Data.Text.Lazy
import Data.Text.Lazy.IO as T
import Fay
import Fay
import Fay.Compiler
import Fay.Compiler.Config
import Fay.Types
import System.Process (readProcess,runInteractiveCommand,terminateProcess)
import Text.Blaze.Html.Renderer.Text
import Web.Scotty

-- | Start a web server for the web client window to load.
suaveServer :: IO ()
suaveServer =
  scotty suavePort
         (do get "/client" client
             get "/i3status" i3status
             get "/" renderView)

client = do dir <- io (fmap (++"/src") getDataDir)
            fp <- io (getDataFileName "src/XMonad/Suave/Client.hs")
            result <- io (compileFile (addConfigDirectoryIncludePaths [dir] def)
                                      fp)
            addHeader "Content-Type" "text/javascript"
            case result of
              Left err -> raise (pack (showCompileError err))
              Right (js,_) -> text (pack js)

renderView = html (renderHtml view)

i3status = do (_in,out,_err,pid) <- io $ runInteractiveCommand "i3status"
              line <- io $ T.hGetLine out
              io $ terminateProcess pid
              text line

io :: MonadIO m => IO a -> m a
io = liftIO
