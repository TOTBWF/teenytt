module TeenyTT.Frontend.Parser.Token
  ( Token(..)
  ) where

import Data.Text (Text)

data Token
    = Colon
    | Arrow
    | Lambda
    | Equal
    | ForAll
    | LParen
    | RParen
    | Identifier Text
    | EOF
    | BlockOpen
    | BlockBreak
    | BlockClose
    deriving (Show)
