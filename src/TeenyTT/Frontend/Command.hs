-- | Driver Commands
module TeenyTT.Frontend.Command
  ( Command(..)
  , displayCommand
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Base.Pretty

import TeenyTT.Elaborator.ConcreteSyntax

data Command
    = Annotate Ident Term
    | Define Ident Term
    | Directive Text
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- [FIXME: Reed M, 02/06/2022] Refactor this
displayCommand :: Command -> Doc ()
displayCommand (Annotate x tp) = pretty x <+> ":" <+> presentTop tp
displayCommand (Define x tp) = pretty x <+> "=" <+> presentTop tp
displayCommand (Directive txt) = pretty txt
