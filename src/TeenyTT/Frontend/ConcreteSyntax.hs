module TeenyTT.Frontend.ConcreteSyntax
  ( Expr(..)
  ) where

import TeenyTT.Core.Ident

data Expr
    = Lam [Ident] Expr
    | App Expr [Expr]
    | Zero
    | Suc Expr
    | Var Ident
    | Univ
    | Nat
    | Pi [Cell Expr] Expr
    deriving (Show)
