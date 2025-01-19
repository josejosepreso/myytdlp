module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import Text.Regex.Posix
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe

fetch :: String -> IO [String]
fetch [] = pure []
fetch url = map (intercalate " " . take 7 . words)
            <$> filter (\l -> l =~ "[a-z0-9]+.*\\|.*")
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

done :: String -> Maybe ComboBoxText -> Maybe ComboBoxText -> Maybe String -> IO ()
done [] _ _ _ = pure ()
done a b c Nothing = done a b c (Just "~/")
done _ Nothing Nothing _ = pure ()
done a Nothing c d = done a (Just $ T.pack "") c d
done a b Nothing d = done a b (Just $ T.pack "") d
done url format1 format2 path = callCommand
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
  comboBoxSetModelText comboBox
  comboBoxSetModelText comboBox'
  
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
    done url f1 f2 p) urlEntry comboBox comboBox' pathSelect
    
  widgetShowAll window
  mainGUI
