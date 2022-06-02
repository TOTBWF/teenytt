-- | The tokens used by the @teenytt@ parser/lexer.
module TeenyTT.Frontend.Parser.Token
  (
  -- * Tokens
    Token(..)
  , Literal(..)
  , Keyword(..)
  , Symbol(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Text (Text)
import TeenyTT.Base.Location

data Token
    = TokSymbol (Loc Symbol)
    | TokKeyword (Loc Keyword)
    | TokLiteral Literal
    | TokIdent (Loc Text)
    | TokDirective (Loc Text)
    | EOF Span
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Literal
    = NumLit (Loc Integer)
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

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
    deriving anyclass (NFData)

data Keyword
    = Type
    | Nat
    | Suc
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
