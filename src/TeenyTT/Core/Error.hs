module TeenyTT.Core.Error
  ( Error(..)
  , Literal(..)
  , Connective(..)
  ) where

import TeenyTT.Core.Ident

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

data Error
    = ValMismatch Connective D.Value
    | TpMismatch Connective D.Type
    | GoalMismatch Connective S.Type
    | UnboundVariable Ident
    | InvalidLiteral Literal S.Type
    | QuotationMismatch D.Type D.Value
    | ExpectedEqual D.Value D.Value
    | ExpectedEqualTp D.Type D.Type
    | ExpectedEqualHead D.Head D.Head
    | ExpectedSpineEqual [D.Frame] [D.Frame]
    deriving (Show)

data Literal
    = NatLit Int
    deriving (Show)

data Connective
    = Pi
    | Nat
    | El D.Type
    | Univ Int
    | Small
    deriving (Show)
