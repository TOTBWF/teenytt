-- | The Concrete Syntax Tree for TeenyTT.
module TeenyTT.Elaborator.ConcreteSyntax
  ( Term
  , Term_(..)
  , Cell(..)
  , Case(..)
  , Pattern(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Base.Location
import TeenyTT.Base.Pretty

type Term = Loc Term_
data Term_
    = Var Text
    | Let Term Ident Term
    | Ann Term Term
    | Hole
    | Incomplete Term

    | Pi [Cell] Term
    | Lam (NonEmpty Ident) Term
    | Ap Term [Term]

    | Sigma [Cell] Term
    | Fst Term
    | Snd Term

    | Univ

    | Nat
    | Lit Integer
    | Suc Term

    | Elim Term [Case] Term
    | LamElim [Case]
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Cell = Cell { names :: NonEmpty Ident, tp :: Term }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Case = Case { label :: Text, patterns :: [Pattern], body :: Term }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Pattern
    = Simple Ident
    | Inductive { value :: Ident, inductive :: Ident }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Pretty-Printing
