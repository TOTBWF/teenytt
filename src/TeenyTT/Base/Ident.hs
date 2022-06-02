-- | Identifiers.
module TeenyTT.Base.Ident
  ( Ident(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Hashable
import Data.Text (Text)

import Prettyprinter as PP

data Ident
    = User Text
    | Anon
    deriving stock (Show, Generic, Eq)
    deriving anyclass (NFData, Hashable)

instance Pretty Ident where
    pretty (User txt) = pretty txt
    pretty Anon = "_"
