module TeenyTT.Frontend.Driver
  ( loadFile
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import TeenyTT.Frontend.Parser

loadFile :: FilePath -> IO ()
loadFile path = do
    str <- readFile path
    print =<< parseExpr str
