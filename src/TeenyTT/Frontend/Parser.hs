-- | The interface to the @teenytt@ parser.
module TeenyTT.Frontend.Parser
  ( tokenize
  , commands
  ) where

import Control.Monad.IO.Class

import Data.ByteString (ByteString)

import TeenyTT.Frontend.Command
import TeenyTT.Frontend.Parser.Monad
import TeenyTT.Frontend.Parser.Token (Token)

import TeenyTT.Frontend.Parser.Lexer qualified as L
import TeenyTT.Frontend.Parser.Grammar qualified as P

tokenize :: (MonadIO m) => FilePath -> ByteString -> m [Token]
tokenize path bs = liftIO $ runParser path [L.layout] bs L.lexer

commands :: (MonadIO m) => FilePath -> ByteString -> m [Command]
commands path bs = liftIO $ runParser path [L.layout] bs $ do
    toks <- L.lexer
    P.toplevel toks
