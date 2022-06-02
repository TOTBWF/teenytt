-- | Driver Commands
module TeenyTT.Frontend.Command
  ( Command(..)
  ) where

import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Elaborator.ConcreteSyntax

data Command
    = Annotate Ident Term
    | Define Ident Term
    | Directive Text
