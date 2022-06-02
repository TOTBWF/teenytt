-- | The Concrete Syntax Tree for TeenyTT.
module TeenyTT.Elaborator.ConcreteSyntax
  (
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Text (Text)

import TeenyTT.Base.Ident

data Term
    = Var Text
    | Let Term Ident Term
    | Ann { term :: Term, tp :: Term }
    | Hole { name :: Maybe Text, term :: Term }

    | Pi [Cell] Term
    | Lam [Ident] Term
    | Ap Term [Term]

    | Sigma [Cell] Term
    | Fst Term
    | Snd Term

    | Univ

    | Nat
    | Lit Int
    | Suc Term

    | Elim { mot :: Term, cases :: [Case], scrut :: Term }
    | LamElim [Case]
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Cell = Cell { names :: [Ident], tp :: Term }
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
