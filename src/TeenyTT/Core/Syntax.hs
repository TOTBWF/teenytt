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
    | Subst Subst Term
    deriving (Show)

data Type
    = Univ
    | Nat
    | Pi Ident Type Type
    | TpVar Index
    | TpSubst Subst Type
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
    deriving (Show)
