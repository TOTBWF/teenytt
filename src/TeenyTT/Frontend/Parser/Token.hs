module TeenyTT.Frontend.Parser.Token
  ( Token(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Text (Text)

data Token
    = Colon
    | Arrow
    | Lambda
    | Equal
    | ForAll
    | LParen
    | RParen
    | LBang
    | RBang
    | Question
    -- Keywords
    | Type
    | Nat
    | Suc
    -- Literals
    | NumLit Int
    -- Identifiers
    | Identifier Text
    | Directive Text
    | Underscore
    -- Layout
    | BlockOpen
    | BlockBreak
    | BlockClose
    | EOF
    deriving (Show, Generic)

instance NFData Token
