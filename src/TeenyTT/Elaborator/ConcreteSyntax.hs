-- | The Concrete Syntax Tree for TeenyTT.
module TeenyTT.Elaborator.ConcreteSyntax
  (
  ) where

import Data.Text (Text)

data Syntax
    = Var Text
