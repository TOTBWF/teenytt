module TeenyTT.Frontend.ConcreteSyntax
  ( Expr(..)
  , Command(..)
  ) where

import Data.Text (Text)

import TeenyTT.Core.Ident

data Expr
    = Lam [Ident] Expr
    | App Expr [Expr]
    | Zero
    | Suc Expr
    | NatLit Int
    | Var Ident
    | Univ Int
    | Nat
    | Pi [Cell Expr] Expr
    | Hole
    | Incomplete Expr
    deriving (Show)

data Command
    = TypeAnn Ident Expr
    | Def Ident Expr
    | Directive Text [Expr]
    deriving (Show)
