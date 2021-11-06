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

tokens :: ByteString -> Either ByteString [Token]
tokens bs = runParser [L.layout] bs L.lexer

commands :: ByteString -> Either ByteString [Command]
commands bs = runParser [L.layout] bs $ do
    toks <- L.lexer
    P.toplevel toks
