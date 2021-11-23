module TeenyTT.Core.Syntax
  ( Term(..)
  , Type(..)
  , Subst(..)
  ) where

import GHC.Generics
import Control.DeepSeq

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty
import TeenyTT.Core.Pretty.Unicode qualified as Pp

import TeenyTT.Core.Env

data Term
    = Lam Ident Term
    | App Term Term
    | Zero
    | Suc Term
    | Rel Type Term
    | NatSmall
    | PiSmall Term Term
    | Local Index
    | Global Level
    | Subst Subst Term
    | Hole Ident Type
    deriving (Show, Generic)

instance NFData Term

data Type
    = Univ Int
    | Nat
    | Pi Ident Type Type
    | El Type Term
    | Small Type Type
    | TpVar Index
    | TpSubst Subst Type
    deriving (Show, Generic)

instance NFData Type

data Subst
    = Id
    -- ^ The identity substitution @Γ → Γ@
    | Comp Subst Subst
    -- ^ Composition of substitutions @σ ∘ τ@ 
    | Emp
    -- ^ The Empty Substitution @Γ → ∅@
    | Weak
    -- ^ Weakening @Γ , A → Γ@
    | Extend Subst Term
    -- ^ Substitution into an extended context @σ, a@.
    deriving (Show, Generic)

instance NFData Subst

--------------------------------------------------------------------------------
-- Pretty Printing

instance Debug Term where
    dump prec (Lam x tm) =
        parensIf (prec > NoPrec) (Pp.lambda <+> pretty x <+> Pp.arrow <+> dump NoPrec tm)
    dump prec (App f arg) =
        parensIf (prec >= AppPrec) (dump AppPrec f <+> dump AppPrec arg)
    dump prec Zero =
        "zero"
    dump prec (Suc n) =
        parensIf (prec > NoPrec) ("suc" <+> dump AppPrec n)
    dump prec (Rel tp small) =
        parensIf (prec > NoPrec) ("rel" <+> dump AppPrec tp <+> dump AppPrec small)
    dump prec NatSmall =
        "nat-small"
    dump prec (PiSmall base fam) =
        parensIf (prec > NoPrec) ("pi-small" <+> dump AppPrec base <+> dump AppPrec fam)
    dump prec (Local ix) =
        "var" <> brackets (pretty ix)
    dump prec (Global lvl) =
        "global" <> brackets (pretty lvl)
    dump prec (Subst sub tm) =
        brackets (dump AppPrec sub <+> slash <+> dump NoPrec tm)
    dump prec (Hole nm tp) =
        parensIf (prec >= AnnPrec) ("?" <> pretty nm <+> colon <+> dump AnnPrec tp)

instance Debug Type where
    dump prec (Univ i) =
        parensIf (prec >= AppPrec) ("Type" <+> pretty i)
    dump prec Nat =
        Pp.nat
    dump prec (Pi x base fam) =
        parensIf (prec >= AppPrec) (Pp.forall <+> parens (pretty x <+> colon <+> dump AnnPrec base))
    dump prec (El univ tm) =
        parensIf (prec >= AppPrec) ("El" <+> dump AppPrec univ <+> dump AppPrec tm)
    dump prec (Small tp univ) =
        parensIf (prec >= AppPrec) ("Small" <+> dump AppPrec tp <+> dump AppPrec univ)
    dump prec (TpVar ix) =
        "tpvar" <> brackets (pretty ix)
    dump prec (TpSubst sub tp) =
        brackets (dump AppPrec sub <+> slash <+> dump NoPrec tp)

instance Debug Subst where
    dump prec Id =
        "id"
    dump prec (Comp sub0 sub1) =
        parensIf (prec >= AppPrec) (dump AppPrec sub0 <+> Pp.compose <+> dump AppPrec sub1)
    dump prec Emp =
        Pp.emptySet
    dump prec Weak =
        "weak"
    dump prec (Extend sub tm) =
        parensIf (prec >= AppPrec) (dump AppPrec sub <> comma <+> dump AppPrec tm)
