module TeenyTT.Frontend.ConcreteSyntax
  ( Expr(..)
  , Command(..)
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

data Command
    = Def Ident Expr Expr
    deriving (Show)
