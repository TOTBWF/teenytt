module TeenyTT.Frontend.Parser.Token
  ( Symbol(..)
  , Keyword(..)
  , Literal(..)
  , Token(..)
  ) where

import GHC.Generics
import Control.DeepSeq

import Data.Text (Text)

import TeenyTT.Core.Position

data Symbol
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
    | Underscore
    -- Layout
    | BlockOpen
    | BlockBreak
    | BlockClose
    deriving stock (Show, Generic)
    deriving anyclass NFData

data Keyword
    = Type
    | Nat
    | Suc
    deriving stock (Show, Generic)
    deriving anyclass NFData


data Literal
    = NumLit Int
    deriving stock (Show, Generic)
    deriving anyclass NFData

-- NOTE: We need to use tuples here so that this
-- plays nicely with the '%token' directive.
data Token
    = TokSymbol Symbol Span
    | TokKeyword Keyword Span
    | TokLiteral Literal
    | TokIdent (Text, Span)
    | TokDirective (Text, Span)
    | EOF
    deriving (Show, Generic)
    deriving anyclass NFData
