module TeenyTT.Frontend.ConcreteSyntax
  ( Expr(..)
  , Command(..)
  ) where

import GHC.Generics

import Control.DeepSeq
import Data.Text (Text)

import TeenyTT.Core.Pretty
import TeenyTT.Core.Pretty.Unicode as Pp
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

instance Debug Expr where
    dump prec (Lam ids body) =
        parensIf (prec > NoPrec) (Pp.lambda <+> hsep (fmap (dump AppPrec) ids) <+> Pp.arrow <+> dump NoPrec body)
    dump prec (App f args) =
        parensIf (prec >= AppPrec) (dump AppPrec f <+> sep (fmap (dump AppPrec) args))
    dump prec Zero =
        "zero"
    dump prec (Suc e) =
        parensIf (prec > NoPrec) ("suc" <+> dump AppPrec e)
    dump prec (NatLit n) =
        pretty n
    dump prec (Var id) =
        pretty id
    dump prec (Univ n) =
        parensIf (prec > NoPrec) ("Type" <+> pretty n)
    dump prec Nat =
        Pp.nat
    dump prec (Pi cells body) =
        parensIf (prec > NoPrec) (Pp.forall <+> sep (fmap (dump AppPrec) cells) <+> Pp.arrow <+> (dump NoPrec body))
    dump prec Hole =
        "?"
    dump prec (Incomplete e) =
        "{!" <+> dump NoPrec e <+> "!}"

-- [FIXME: Reed M, 07/11/2021] Don't use 'Expr' for directive arguments!
data Command
    = TypeAnn Ident (Loc Expr)
    | Def Ident (Loc Expr)
    | Directive Text [Loc Expr]
    deriving stock (Show, Generic)
    deriving anyclass (NFData)
