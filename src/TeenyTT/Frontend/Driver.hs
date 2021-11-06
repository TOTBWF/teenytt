module TeenyTT.Frontend.Driver
  ( loadFile
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Data.Text (Text)
import Data.Text qualified as T

import TeenyTT.Frontend.Parser qualified as P

divider :: IO ()
divider = putStrLn (replicate 80 '-')

loadFile :: FilePath -> IO ()
loadFile path = do
    bytes <- BS.readFile path
    divider
    print $ P.tokens bytes
    divider
    print $ P.commands bytes
