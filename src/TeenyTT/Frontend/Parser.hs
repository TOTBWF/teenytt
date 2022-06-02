-- | The interface to the @teenytt@ parser.
module TeenyTT.Frontend.Parser
  ( tokenize
  , commands
  ) where

import Data.ByteString (ByteString)

import TeenyTT.Frontend.Command
import TeenyTT.Frontend.Parser.Monad
import TeenyTT.Frontend.Parser.Token (Token)

import TeenyTT.Frontend.Parser.Lexer qualified as L
import TeenyTT.Frontend.Parser.Grammar qualified as P

tokenize :: FilePath -> ByteString -> IO (Either ParseError [Token])
tokenize path bs = runParser path [L.layout] bs L.lexer

commands :: FilePath -> ByteString -> IO (Either ParseError [Command])
commands path bs = runParser path [L.layout] bs $ do
    toks <- L.lexer
    P.toplevel toks
