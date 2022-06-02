-- | The interface to the @teenytt@ parser.
module TeenyTT.Frontend.Parser
  ( tokenize
  ) where

import Data.ByteString (ByteString)

import TeenyTT.Frontend.Parser.Monad
import TeenyTT.Frontend.Parser.Token (Token)

import TeenyTT.Frontend.Parser.Lexer qualified as L

tokenize :: FilePath -> ByteString -> IO (Either ParseError [Token])
tokenize path bs = runParser path [L.layout] bs L.lexer
