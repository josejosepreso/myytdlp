module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import qualified Data.Text as T

butLast :: [String] -> [String]
butLast [a,b] = [a,b]
butLast (x:xs) = butLast xs

fetch :: String -> IO [[String]]
fetch url = drop 8
            <$> map (\l -> [words l !! 0, words l !! 1] ++ (butLast $ words l))
            <$> (lines <$> readProcess "/bin/yt-dlp" ["-F", url] [])

main :: IO ()
main = do
  initGUI

  -- Create the builder, and load the UI file
  builder <- builderNew
  builderAddFromFile builder "layout.ui"

  -- Retrieve some objects from the UI
  window <- builderGetObject builder castToWindow "window1"
  dlButton <- builderGetObject builder castToButton "dlButton"
  searchButton <- builderGetObject builder castToButton "searchButton"
  urlEntry <- builderGetObject builder castToEntry "urlEntry"
  comboBox <- builderGetObject builder castToComboBox "format1"

  -- Basic user interaction
  on searchButton buttonActivated $ entryGetText urlEntry >>= fetch >>= print
  on window objectDestroy mainQuit

  -- comboBoxAppendText comboBox (T.pack "JAJA") >>= print

  -- Display the window
  widgetShowAll window
  mainGUI
