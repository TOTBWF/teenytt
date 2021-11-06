module TeenyTT.Frontend.Parser.Token
  ( Token(..)
  ) where

data Token
    = Colon
    | Arrow
    | Lambda
    | ForAll
    | LParen
    | RParen
    | Identifier String
    deriving (Show)
