module TeenyTT.Core.Syntax
  ( Term(..)
  , Type(..)
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Env

data Term
    = Lam Ident Term
    | App Term Term
    | Zero
    | Suc Term
    | Local Index
    | Global Level
    deriving (Show)

data Type
    = Univ
    | Nat
    | Pi Ident Type Type
    deriving (Show)
