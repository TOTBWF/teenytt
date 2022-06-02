-- | Identifiers.
module TeenyTT.Base.Ident
  ( Ident(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Text (Text)

data Ident
    = User Text
    | Anon
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
