module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import qualified Data.Text as T

fetch :: String -> IO [String]
fetch [] = return []
fetch url = drop 8
            <$> map (rows . words)
            <$> (lines <$> readProcess "/bin/yt-dlp" ["-F", url] [])
  where
    rows [] = []
    rows (x:xs) = x ++ " " ++ rows xs

putFormatList :: ComboBox -> ComboBox -> [String] -> IO ()
putFormatList _ _ [] = pure ()
putFormatList comboBox comboBox' (f:formats) = do
  id <- comboBoxAppendText comboBox (T.pack f)
  id <- comboBoxAppendText comboBox' (T.pack f)
  putFormatList comboBox comboBox' formats

done :: String -> Maybe ComboBoxText -> Maybe ComboBoxText -> Maybe FilePath -> IO ()
--done [] _ _ _ = return ()
--done a b c Nothing = done a b c (Just "~/")
done url format1 format2 path = print $ url

main :: IO ()
main = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "layout.ui"

  window <- builderGetObject builder castToWindow "window1"
  on window objectDestroy mainQuit
  searchButton <- builderGetObject builder castToButton "searchButton"
  urlEntry <- builderGetObject builder castToEntry "urlEntry"
  comboBox <- builderGetObject builder castToComboBox "format1"
  comboBox' <- builderGetObject builder castToComboBox "format2"
  comboBoxSetModelText comboBox
  comboBoxSetModelText comboBox'
  dlButton <- builderGetObject builder castToButton "dlButton"
  pathSelect <- builderGetObject builder castToFileChooser "pathSave"
  
  on searchButton buttonActivated
    $ entryGetText urlEntry
    >>= fetch
    >>= putFormatList comboBox comboBox'
    
  on dlButton buttonActivated
    =<< done
    <$> entryGetText urlEntry
    <*> comboBoxGetActiveText comboBox'
    <*> comboBoxGetActiveText comboBox
    <*> fileChooserGetCurrentFolder pathSelect
    
  widgetShowAll window
  mainGUI
