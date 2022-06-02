-- | The main driver of @teenytt@.
module TeenyTT.Frontend.Driver
  ( runDriver
  , loadFile
  , lexFile
  , parseFile
  ) where

import Control.Monad.IO.Class

import Data.ByteString qualified as BS

import TeenyTT.Frontend.Parser qualified as P

import TeenyTT.Frontend.Driver.Monad

unimplemented :: String -> Driver ()
unimplemented msg = liftIO $ putStrLn $ "Unimplemented: " <> msg

loadFile :: FilePath -> Driver ()
loadFile path = unimplemented "File loading"

lexFile :: FilePath -> Driver ()
lexFile path = do
    bytes <- liftIO $ BS.readFile path
    toks <- hoistError =<< (liftIO $ P.tokenize path bytes)
    liftIO $ print toks  

parseFile :: FilePath -> Driver ()
parseFile path = do
    bytes <- liftIO $ BS.readFile path
    cmds <- hoistError =<< (liftIO $ P.commands path bytes)
    liftIO $ print cmds

