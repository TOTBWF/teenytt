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

data Token
    = TokSymbol Symbol Span
    | TokKeyword Keyword Span
    | TokLiteral {-# UNPACK #-} (Loc Literal)
    | TokIdent {-# UNPACK #-} (Loc Text)
    | TokDirective {-# UNPACk #-} (Loc Text)
    | EOF Span
    deriving (Show, Generic)
    deriving anyclass NFData

instance Located Token where
    locate (TokSymbol _ sp)   = sp
    locate (TokKeyword _ sp)  = sp
    locate (TokLiteral loc)   = locate loc
    locate (TokIdent loc)     = locate loc
    locate (TokDirective loc) = locate loc
    locate (EOF sp)           = sp
