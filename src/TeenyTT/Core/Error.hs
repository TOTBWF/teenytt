module TeenyTT.Core.Error
  ( Error(..)
  , Literal(..)
  , Connective(..)
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

data Error
    = ValMismatch Connective D.Value
    | TpMismatch Connective D.Type
    | GoalMismatch Connective S.Type
    | UnboundVariable Ident
    | InvalidLiteral Literal S.Type
    | QuotationMismatch D.Type D.Value
    | ExpectedEqual S.Term S.Term
    | ExpectedTpEqual S.Type S.Type
    deriving (Show)

instance Debug Error where
    dump _ (ValMismatch con v) = "Value Mismatch: expected" <+> squotes (dump NoPrec con) <+> "but got" <+> squotes (dump NoPrec v)
    dump _ (TpMismatch con tp) = "Type Mismatch: expected" <+> squotes (dump NoPrec con) <+> "but got" <+> squotes (dump NoPrec tp)
    dump _ (GoalMismatch con tp) = "Goal Mismatch: expected" <+> squotes (dump NoPrec con) <+> "but got" <+> squotes (dump NoPrec tp)
    dump _ (UnboundVariable id) = "Unbound Variable:" <+> pretty id
    dump _ (InvalidLiteral lit tp) = "Invalid Literal:" <+> pretty lit <+> "is not a valid literal for type" <+> dump NoPrec tp
    dump _ (QuotationMismatch tp v) = "Quotation Mismatch:" <+> squotes (dump NoPrec v) <+> "is not of type" <+> squotes (dump NoPrec tp)
    dump _ (ExpectedEqual tm0 tm1) = "Expected Equal:" <+> squotes (dump NoPrec tm0) <+> squotes (dump NoPrec tm1)
    dump _ (ExpectedTpEqual tp0 tp1) = "Expected Equal Types:" <+> squotes (dump NoPrec tp0) <+> squotes (dump NoPrec tp1)

data Literal
    = NatLit Int
    deriving (Show)

instance Pretty Literal where
    pretty (NatLit n) = pretty n

data Connective
    = Pi
    | Nat
    | El D.Type
    | Univ Int
    | Small
    deriving (Show)

instance Debug Connective where
    dump prec Pi = "Pi"
    dump prec Nat = "Nat"
    dump prec (El tp) = parensIf (prec > NoPrec) $ "el" <+> dump AppPrec tp
    dump prec (Univ n) = parensIf (prec > NoPrec) $ "Type" <+> pretty n
    dump prec Small = "Small"
