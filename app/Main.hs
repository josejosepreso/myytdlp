module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import Text.Regex.Posix
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (fromJust)

fetch :: String -> IO [String]
fetch [] = pure []
fetch url = map (intercalate " " . words)
            <$> filter (=~ "[a-z0-9]+.*\\|.*")
            <$> lines
            <$> readProcess "/bin/yt-dlp" ["-F", url] []

putFormatList :: ComboBox -> ComboBox -> [String] -> IO ()
putFormatList _ _ [] = pure ()
putFormatList comboBox comboBox' (f:formats) = do
  comboBoxAppendText comboBox (T.pack f)
  comboBoxAppendText comboBox' (T.pack f)
  putFormatList comboBox comboBox' formats

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just $ head xs

download :: String -> Maybe ComboBoxText -> Maybe ComboBoxText -> Maybe String -> IO ()
download [] _ _ _ = pure ()
download a b c Nothing = download a b c (Just "file://~")
download _ Nothing Nothing _ = pure ()
download a Nothing c d = download a (Just $ T.pack "") c d
download a b Nothing d = download a b (Just $ T.pack "") d
download url format1 format2 path = callCommand
                                $ printf
                                  "/bin/yt-dlp -f %s \"%s\" -P %s/"
                                  ((\a b ->
                                      if x == "" then y
                                      else if y == "" then x
                                      else x ++ "+" ++ y
                                   ) format1 format2)
                                  url
                                  (drop 7 $ fromJust path)
  where
    f a = head' . words $ T.unpack $ fromJust a
    g b
      | f b == Nothing = ""
      | otherwise = fromJust $ f b
    x = g format1
    y = g format2
    
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
  listStore <- comboBoxSetModelText comboBox
  listStore' <- comboBoxSetModelText comboBox'

  dlButton <- builderGetObject builder castToButton "dlButton"
  pathSelect <- builderGetObject builder castToFileChooser "pathSave"
  
  searchButton `on` buttonActivated
    $ entryGetText urlEntry
    >>= fetch
    >>= putFormatList comboBox comboBox'
    
  dlButton `on` buttonActivated $ (\u c c' p -> do
    url <- entryGetText u
    f1 <- comboBoxGetActiveText c
    f2 <- comboBoxGetActiveText c'
    p <- fileChooserGetURI p
    download url f1 f2 p) urlEntry comboBox comboBox' pathSelect
    
  widgetShowAll window
  mainGUI
