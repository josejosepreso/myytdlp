module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import qualified Data.Text as T

butLast :: [String] -> [String]
butLast [a, b] = [a, b]
butLast (x:xs) = butLast xs

fetch :: String -> IO [String]
fetch [] = return []
fetch url = drop 8
            <$> map (\l -> rows $ words l)
            <$> (lines <$> readProcess "/bin/yt-dlp" ["-F", url] [])
  where
    rows [] = []
    rows (x:xs) = x ++ " " ++  rows xs

putFormatList :: ComboBox -> [String] -> IO ()
putFormatList _ [] = pure ()
putFormatList comboBox (f:formats) = do
  id <- comboBoxAppendText comboBox (T.pack f)
  putFormatList comboBox formats

main :: IO ()
main = do
  initGUI

  -- Create the builder, and load the UI file
  builder <- builderNew
  builderAddFromFile builder "layout.ui"

  -- Retrieve some objects from the UI
  window <- builderGetObject builder castToWindow "window1"
  
  searchButton <- builderGetObject builder castToButton "searchButton"
  urlEntry <- builderGetObject builder castToEntry "urlEntry"
  
  formatBox <- builderGetObject builder castToBox "formatBox"
  comboBox <- builderGetObject builder castToComboBox "format1"
  comboBoxSetModelText comboBox
  
  dlButton <- builderGetObject builder castToButton "dlButton"

  -- Basic user interaction
  on searchButton buttonActivated
    $ entryGetText urlEntry
    >>= fetch
    >>= putFormatList comboBox
  
  on window objectDestroy mainQuit
  
  widgetShowAll window
  mainGUI
