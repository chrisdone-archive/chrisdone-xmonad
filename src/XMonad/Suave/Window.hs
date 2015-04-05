-- | The client window (webkit).

module XMonad.Suave.Window where

import           Clockin
import           Control.Concurrent
import           Control.Exception (try,SomeException)
import           Control.Monad
import           Control.Monad.Fix
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy (unpack,Text)
import qualified Data.Text.Lazy.IO as T
import           Data.Time
import           Data.Time.Zone
import           Graphics.UI.Gtk hiding (LayoutClass)
import           Graphics.UI.Gtk.WebKit.DOM.Document
import           Graphics.UI.Gtk.WebKit.DOM.HTMLElement
import           Graphics.UI.Gtk.WebKit.Types hiding (Text)
import           Graphics.UI.Gtk.WebKit.WebView
import           Paths_xmonad_chrisdone
import           System.IO
import           System.Locale
import           System.Process
import           Text.Blaze.Html.Renderer.Text
import           XMonad.Suave.Types
import           XMonad.Suave.View

-- | Start up a Suave panel.
suaveStart :: IO (Suave)
suaveStart =
  do void initGUI
     window <- windowNew
     vContainer <- vBoxNew False 0
     scrolledWindow <-
       scrolledWindowNew Nothing Nothing
     scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyNever
     webview <- webViewNew
     set window [containerChild := vContainer,windowTitle := suaveWindowTitle]
     boxPackStart vContainer scrolledWindow PackGrow 0
     set scrolledWindow [containerChild := webview]
     Just document <- webViewGetDomDocument webview
     Just body <- documentGetBody document
     htmlElementSetInnerHTML body
                             (unpack (renderHtml page))
     Just i3 <-
       fmap (fmap castToHTMLElement)
            (documentGetElementById document "i3")
     Just date <-
       fmap (fmap castToHTMLElement)
            (documentGetElementById document "date")
     Just clockin <-
       fmap (fmap castToHTMLElement)
            (documentGetElementById document "clockin")
     void (forkIO (fix (\loop ->
                          do postGUISync (void (try (updateUI i3 date clockin) :: IO (Either SomeException ())))
                             threadDelay (1000 * 1000)
                             loop)))
     void (onDestroy window mainQuit)
     void (widgetShowAll window)
     return (Suave window)

-- | Update the contents of the panel.
updateUI :: HTMLElement -> HTMLElement -> HTMLElement -> IO ()
updateUI i3 date clockin =
  do status <- i3status
     mem <- readProcessLine "mem-use.sh"
     htmlElementSetInnerHTML i3
                             (unpack status ++ " <span class='indicator'><i class='fa fa-adjust'></i> " ++ mem ++ "</span>")
     now <- getZonedTime
     htmlElementSetInnerHTML date
                             (dateDisplays now)
     config <- getClockinConfig
     entries <- readClockinEntries config
     now <-
       fmap zonedTimeToLocalTime getZonedTime
     let desc =
           onelinerStatus now
                          (clockinStatus config now entries)
     htmlElementSetInnerHTML clockin
                             ("<i class='fa fa-clock-o'></i> " ++ T.unpack desc)
  where readInt :: Text -> Int
        readInt = read . unpack

dateDisplays now =
  unwords [timeAtZone (zoneOf "PST") now
          ,"/"
          ,timeAtZone (zoneOf "EST") now
          ,"/"
          -- ,timeAtZone (zoneOf "IST") now -- not needed presently, IST=CEST
          -- ,"/"
          ,bold (formatTime defaultTimeLocale "%a %d %b" now)
          ,bold (timeAtZone this now)]
  where bold x = "<span class='bold'>" ++ x ++ "</span>"
        this = zonedTimeZone now
        timeAndZone =
          formatTime defaultTimeLocale "%H:%M %Z (UTC%z)"
        timeAtZone zone t =
          timeAndZone
            (utcToZonedTime zone
                            (zonedTimeToUTC t))
        zoneOf name =
          fromMaybe (error ("Unable to get zone for " ++ name))
                    (getZone name)

-- | Get the output from i3status.
i3status :: IO Text
i3status =
  do fp <- getDataFileName "i3status.conf"
     (inp,out,err,pid) <-
       runInteractiveCommand ("i3status -c " ++ show fp)
     line <- T.hGetLine out
     hClose out
     hClose inp
     hClose err
     terminateProcess pid
     return line

-- | Read a process line.
readProcessLine :: String -> IO String
readProcessLine cmd =
  do (inp,out,err,pid) <-
       runInteractiveCommand cmd
     line <- T.hGetLine out
     hClose out
     hClose inp
     hClose err
     terminateProcess pid
     return (LT.unpack line)

-- | Window title of the Suave panel.
suaveWindowTitle :: String
suaveWindowTitle = "xmonad-suave-panel"
