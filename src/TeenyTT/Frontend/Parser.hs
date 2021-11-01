module TeenyTT.Frontend.Parser
  ( debugLexer
  ) where

import Data.ByteString (ByteString)

import TeenyTT.Frontend.Parser.Grammar
import TeenyTT.Frontend.Parser.Lexer

import TeenyTT.Frontend.ConcreteSyntax
import TeenyTT.Frontend.Parser.Token (Token)
import TeenyTT.Frontend.Parser.Lexer
import TeenyTT.Frontend.Parser.Lexer.Monad


debugLexer :: ByteString -> Either ByteString [Token]
debugLexer bs = runLexer bs lexer

-- parseExpr :: String -> Either String Expr
-- parseExpr str =
--     let toks = lexer str
--     in exprParser toks
