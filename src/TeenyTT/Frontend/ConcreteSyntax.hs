module TeenyTT.Frontend.ConcreteSyntax
  ( Expr(..)
  , Command(..)
  ) where

import GHC.Generics

import Control.DeepSeq
import Data.Text (Text)

import TeenyTT.Core.Pretty
import TeenyTT.Core.Position
import TeenyTT.Core.Ident

data Expr
    = Lam [Ident] (Loc Expr)
    | App (Loc Expr) [Loc Expr]
    | Zero
    | Suc (Loc Expr)
    | NatLit Int
    | Var Ident
    | Univ Int
    | Nat
    | Pi [Cell (Loc Expr)] (Loc Expr)
    | Hole
    | Incomplete (Loc Expr)
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- [FIXME: Reed M, 07/11/2021] Actually implement this
instance Debug Expr where
    dump = pretty . show

-- [FIXME: Reed M, 07/11/2021] Don't use 'Expr' for directive arguments!
data Command
    = TypeAnn Ident (Loc Expr)
    | Def Ident (Loc Expr)
    | Directive Text [Loc Expr]
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
