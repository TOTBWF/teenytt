module TeenyTT.Frontend.Parser
  ( tokens
  , commands
  ) where

import Data.ByteString (ByteString)


import TeenyTT.Frontend.ConcreteSyntax

import TeenyTT.Frontend.Parser.Monad
import TeenyTT.Frontend.Parser.Token (Token)

import TeenyTT.Frontend.Parser.Grammar qualified as P
import TeenyTT.Frontend.Parser.Lexer qualified as L
import System.IO (FilePath)

tokens :: FilePath -> ByteString -> Either ParseError [Token]
tokens path bs = runParser path [L.layout] bs L.lexer

commands :: FilePath -> ByteString -> Either ParseError [Command]
commands path bs = runParser path [L.layout] bs $ do
    toks <- L.lexer
    P.toplevel toks
