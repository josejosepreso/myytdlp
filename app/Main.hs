module Main where

import Graphics.UI.Gtk
import System.Process
import Text.Printf
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe

import Data.Text.Lazy.Builder (Builder, fromString)

fetch :: String -> IO [String]
fetch [] = pure []
fetch url = drop 8
            <$> map (cells . words)
            <$> (lines <$> readProcess "/bin/yt-dlp" ["-F", url] [])
  where
    cells w = intercalate " " $ take 3 w ++ drop (length w - 4) w

putFormatList :: ComboBox -> ComboBox -> [String] -> IO ()
putFormatList _ _ [] = pure ()
putFormatList comboBox comboBox' (f:formats) = do
  comboBoxAppendText comboBox (T.pack f)
  comboBoxAppendText comboBox' (T.pack f)
  putFormatList comboBox comboBox' formats

done :: Entry -> ComboBox -> ComboBox -> FileChooser -> IO ()
--done [] _ _ _ = return ()
--done a b c Nothing = done a b c (Just "~/")
done url format1 format2 path = do
  u <- entryGetText url :: IO String
  f1 <- tail . head . words . show . fromJust <$> comboBoxGetActiveText format1
  f2 <- tail . head . words . show . fromJust <$> comboBoxGetActiveText format2
  p <- fileChooserGetURI path
  callCommand $ printf "/bin/yt-dlp -f %s+%s \"%s\" -P %s/" f1 f2 u (drop 7 $ fromJust p)

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
    
  on dlButton buttonActivated $ done urlEntry comboBox comboBox' pathSelect
    
  widgetShowAll window
  mainGUI
