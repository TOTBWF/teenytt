module TeenyTT.Frontend.Parser
  ( parseExpr
  ) where

import TeenyTT.Frontend.Parser.Grammar
import TeenyTT.Frontend.Parser.Lexer

import TeenyTT.Frontend.ConcreteSyntax

parseExpr :: String -> IO (Either String Expr)
parseExpr str = do
    let tokens = lexer str
    print tokens
    pure $ parser tokens
