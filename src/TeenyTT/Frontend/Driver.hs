module TeenyTT.Frontend.Driver
  ( loadFile
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Data.Text (Text)
import Data.Text qualified as T

import TeenyTT.Frontend.Parser

loadFile :: FilePath -> IO ()
loadFile path = do
    bytes <- BS.readFile path
    print $ debugLexer bytes
