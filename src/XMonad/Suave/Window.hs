-- | The client window (webkit).

module XMonad.Suave.Window where

import           Paths_xmonad_chrisdone
import           XMonad.Suave.Types
import           XMonad.Suave.View

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Data.Text.Lazy (unpack,Text)
import qualified Data.Text.Lazy.IO as T
import           Data.Time
import           Graphics.UI.Gtk hiding (LayoutClass)
import           Graphics.UI.Gtk.WebKit.DOM.Document
import           Graphics.UI.Gtk.WebKit.DOM.HTMLElement
import           Graphics.UI.Gtk.WebKit.Types hiding (Text)
import           Graphics.UI.Gtk.WebKit.WebView
import           System.Locale
import           System.Process
import           Text.Blaze.Html.Renderer.Text

-- | Start up a Suave panel.
suaveStart :: IO (Suave)
suaveStart = do
  void initGUI
  window <- windowNew
  vContainer <- vBoxNew False 0
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyNever
  webview <- webViewNew
  set window [containerChild := vContainer
             ,windowTitle    := suaveWindowTitle]
  boxPackStart vContainer scrolledWindow PackGrow 0
  set scrolledWindow [containerChild := webview]
  Just document <- webViewGetDomDocument webview
  Just body <- documentGetBody document
  htmlElementSetInnerHTML body (unpack (renderHtml page))
  Just i3 <- fmap (fmap castToHTMLElement)
                  (documentGetElementById document "i3")
  Just date <- fmap (fmap castToHTMLElement)
                    (documentGetElementById document "date")
  void (forkIO (fix (\loop -> do postGUISync (updateUI i3 date)
                                 threadDelay (1000 * 1000)
                                 loop)))
  void (onDestroy window mainQuit)
  void (widgetShowAll window)
  return (Suave window)

-- | Update the contents of the panel.
updateUI :: HTMLElement -> HTMLElement -> IO ()
updateUI i3 date =
  do status <- i3status
     htmlElementSetInnerHTML i3 (unpack status)
     now <- getZonedTime
     htmlElementSetInnerHTML date (formatTime defaultTimeLocale "%F %T %z (%Z)" now)

-- | Get the output from i3status.
i3status :: IO Text
i3status =
  do fp <- getDataFileName "i3status.conf"
     (_in,out,_err,pid) <- runInteractiveCommand ("i3status -c " ++ show fp)
     line <- T.hGetLine out
     terminateProcess pid
     return line

-- | Window title of the Suave panel.
suaveWindowTitle :: String
suaveWindowTitle = "xmonad-suave-panel"
