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
import TeenyTT.Base.Pretty

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
    | Times
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
    | Fst
    | Snd
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance Located Token where
    locate (TokSymbol sym) = locate sym
    locate (TokKeyword keyword) = locate keyword
    locate (TokLiteral lit) = locate lit
    locate (TokIdent txt) = locate txt
    locate (TokDirective txt) = locate txt
    locate (EOF sp) = sp

instance Located Literal where
    locate (NumLit n) = locate n

--------------------------------------------------------------------------------
-- Debugging

instance Pretty Token where
    pretty (TokSymbol sym) = pretty sym
    pretty (TokKeyword keyword) = pretty keyword
    pretty (TokLiteral lit) = pretty lit
    pretty (TokIdent txt) = parens $ "ident" <+> pretty txt
    pretty (TokDirective txt) = parens $ "directive" <+> pretty txt
    pretty (EOF _) = "eof"

instance Pretty Symbol where
    pretty Colon = ":"
    pretty Arrow = "→"
    pretty Lambda = "λ"
    pretty Equal = "="
    pretty ForAll = "∀"
    pretty Times = "×"
    pretty LParen = "("
    pretty RParen = ")"
    pretty LBang = "{!"
    pretty RBang = "!}"
    pretty Question = "?"
    pretty Underscore = "_"
    pretty BlockOpen = "block/open"
    pretty BlockBreak = "block/break"
    pretty BlockClose = "block/close"

instance Pretty Keyword where
    pretty Type = "Type"
    pretty Nat = "ℕ"
    pretty Suc = "suc"
    pretty Fst = "fst"
    pretty Snd = "snd"

instance Pretty Literal where
    pretty (NumLit n) = pretty n
