-- | The core syntax of @teenytt@
module TeenyTT.Core.Syntax
  ( Term
  , Type
  , Syntax(..)
  , SyntaxType(..)
  ) where

import TeenyTT.Core.Types

type Term = Syntax
type Type = SyntaxType
