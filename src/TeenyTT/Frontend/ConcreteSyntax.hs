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

data Loc a = Loc Span a

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
    deriving (Show, Generic)

instance NFData Expr

-- [FIXME: Reed M, 07/11/2021] Actually implement this
instance Debug Expr where
    dump = pretty . show

data Command
    = TypeAnn Ident Expr
    | Def Ident Expr
    | Directive Text [Expr]
    deriving (Show, Generic)

instance NFData Command
