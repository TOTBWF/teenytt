module TeenyTT.Core.Error
  ( Error(..)
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
    | QuotationMismatch D.Type D.Value
    | ExpectedEqualTp D.Type D.Type
    deriving Show

data Connective
    = Pi
    deriving (Show)
