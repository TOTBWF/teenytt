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
    dump (Lam x tm) = Pp.lambda <+> dump x <+> Pp.arrow <+> dump tm
    dump (App f a) = parens (dump f) <+> parens (dump a)
    dump Zero = "zero"
    dump (Suc n) = "suc" <+> parens (dump n)
    dump (Rel tp small) = "rel" <+> parens (dump tp) <+> parens (dump small) 
    dump NatSmall = "nat-small"
    dump (PiSmall base fam) = "pi-small" <+> parens (dump base) <+> parens (dump fam)
    dump (Local ix) = dump ix
    dump (Global lvl) = dump lvl
    dump (Subst sub tm) = brackets (dump sub <+> dump tm)
    dump (Hole nm tp) = "?" <> dump nm <+> parens (dump tp)

instance Debug Type where
    dump (Univ i) = "Type" <+> pretty i
    dump Nat = Pp.nat
    dump (Pi x base fam) = Pp.forall <+> parens (dump x <+> colon <+> dump base) <+> Pp.arrow <+> parens (dump fam)
    dump (El univ tm) = "El" <+> parens (dump univ) <+> parens (dump tm)
    dump (Small tp univ) = "Small" <+> parens (dump tp) <+> parens (dump univ)
    dump (TpVar ix) = dump ix
    dump (TpSubst sub tp) = brackets (dump sub <+> dump tp)

instance Debug Subst where
    dump Id = "id"
    dump (Comp sub0 sub1) = dump sub0 <+> Pp.compose <+> dump sub1
    dump Emp = Pp.emptySet
    dump Weak = "weak"
    dump (Extend su tm) = dump su <+> comma <+> dump tm
