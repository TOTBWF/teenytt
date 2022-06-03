-- | The core data types for @teenytt@.
-- [TODO: Reed M, 02/06/2022] Explore the possibilty of using backpack here.
module TeenyTT.Core.Types (
  -- * Syntax
    Syntax(..)
  , SyntaxType(..)
  -- * Domain
  , Value(..)
  , ValueType(..)
  , Neu(..)
  , Head(..)
  , Frame(..)
  , Clo(..)
  ) where

import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Base.Env (Env)
import TeenyTT.Base.Prec qualified as Prec
import TeenyTT.Base.Pretty (Pretty(..), Display(..), display, (<+>))
import TeenyTT.Base.Pretty qualified as Pp

--------------------------------------------------------------------------------
-- Syntax

data Syntax
    = Local Int
    | Global Text ~Value
    | Let Ident Syntax Syntax
    | Hole
    | Lam Ident Syntax
    | Ap Syntax Syntax
    | Pair Syntax Syntax
    | Fst Syntax
    | Snd Syntax
    | Zero
    | Suc Syntax
    | NatElim Syntax Syntax Syntax Syntax
    | CodePi Ident Syntax Syntax
    | CodeSigma Ident Syntax Syntax
    | CodeUniv
    | CodeNat
    deriving stock (Show)

data SyntaxType
    = Pi Ident SyntaxType SyntaxType
    | Sigma Ident SyntaxType SyntaxType
    | El Syntax
    | Nat
    | Univ
    deriving stock (Show)


--------------------------------------------------------------------------------
-- Domain

data Value
    = VNeu Neu
    | VLam Ident (Clo Syntax)
    | VPair Value Value
    | VZero
    | VSuc Value
    | VCodePi Ident Value (Clo Syntax)
    | VCodeSigma Ident Value (Clo Syntax)
    | VCodeUniv
    | VCodeNat
    deriving stock (Show)

data ValueType
    = VElNeu Neu
    | VPi Ident ValueType (Clo SyntaxType)
    | VSigma Ident ValueType (Clo SyntaxType)
    | VNat
    | VUniv

data Neu = Neu { hd :: Head, spine :: [Frame] }
    deriving stock (Show)

data Head
    = KLocal Int
    | KGlobal Text ~Value
    | KHole
    deriving stock (Show)

data Frame
    = KAp Value
    | KFst
    | KSnd
    | KNatElim Value Value Value
    deriving stock (Show)

--------------------------------------------------------------------------------
-- Closures

data Clo a = Clo (Env Value) a
    deriving stock (Show)

--------------------------------------------------------------------------------
-- Display

instance Display Syntax where
    classify (Local _)         = Prec.atom
    classify (Global _ _)      = Prec.atom
    classify (Let _ _ _)       = Prec.juxtaposition <> Prec.in_
    classify Hole              = Prec.atom
    classify (Lam _ _)         = Prec.lambda
    classify (Ap _ _)          = Prec.juxtaposition
    classify (Pair _ _)        = Prec.delimited
    classify (Fst _)           = Prec.juxtaposition
    classify (Snd _)           = Prec.juxtaposition
    classify Zero              = Prec.atom
    classify (Suc _)           = Prec.juxtaposition
    classify (NatElim _ _ _ _) = Prec.juxtaposition
    classify (CodePi _ _ _)    = Prec.arrow
    classify (CodeSigma _ _ _) = Prec.times 
    classify CodeUniv          = Prec.atom
    classify CodeNat           = Prec.atom

    display' env (Local ix) =
        Pp.index ix env
    display' env (Global txt _) =
        pure $ pretty txt
    display' env (Let x tm body) = do
        ptm <- display (Pp.isolated env) tm
        Pp.extend x env \px -> do
            pbody <- display (Pp.rightOf Prec.in_ env) body
            pure $ "let" <+> px <+> "=" <+> ptm <+> "in" <+> pbody
    display' env Hole =
        pure "?"
    display' env (Lam x body) =
        Pp.extend x env \px -> do
            pbody <- display (Pp.rightOf Prec.arrow env) body
            pure $ "λ" <+> px <+> "→" <+> pbody 
    display' env (Ap fn arg) = do
        pfn <- display (Pp.leftOf Prec.juxtaposition env) fn
        parg <- display (Pp.rightOf Prec.juxtaposition env) arg
        pure $ pfn <+> parg
    display' env (Pair l r) = do
        pl <- display (Pp.leftOf Prec.comma env) l
        pr <- display (Pp.leftOf Prec.comma env) r
        pure $ Pp.parens $ pl <+> "," <> pr
    display' env (Fst tm) = do
        ptm <- display (Pp.rightOf Prec.juxtaposition env) tm
        pure $ "fst" <+> ptm
    display' env (Snd tm) = do
        ptm <- display (Pp.rightOf Prec.juxtaposition env) tm
        pure $ "snd" <+> ptm
    display' env Zero = do
        pure "0"
    display' env (Suc n) = do
        pn <- display (Pp.rightOf Prec.juxtaposition env) n
        pure $ "suc" <+> pn
    display' env (NatElim mot z s scrut) = do
        pmot <- display (Pp.rightOf Prec.juxtaposition env) mot
        pz <- display (Pp.rightOf Prec.juxtaposition env) z
        ps <- display (Pp.rightOf Prec.juxtaposition env) s
        pscrut <- display (Pp.rightOf Prec.juxtaposition env) scrut
        pure $ "nat/elim" <+> pmot <+> pz <+> ps <+> pscrut
    display' env (CodePi x base fam) = do
        pbase <- display (Pp.rightOf Prec.colon env) base
        Pp.extend x env \px -> do
            pfam <- display (Pp.rightOf Prec.arrow env) fam
            pure $ "∀" <> (Pp.parens $ px <+> ":" <+> pbase) <+> "→" <+> pfam
    display' env (CodeSigma x base fam) = do
        pbase <- display (Pp.rightOf Prec.colon env) base
        Pp.extend x env \px -> do
            pfam <- display (Pp.rightOf Prec.times env) fam
            pure $ (Pp.parens $ px <+> ":" <+> pbase) <+> "×" <+> pfam
    display' env CodeUniv =
        pure "Type"
    display' env CodeNat =
        pure "ℕ"

instance Display SyntaxType where
    classify (Pi _ _ _) = Prec.arrow
    classify (Sigma _ _ _) = Prec.times
    classify (El _) = Prec.passed
    classify Nat = Prec.atom
    classify Univ = Prec.atom

    display' env (Pi x base fam) = do
        pbase <- display (Pp.rightOf Prec.colon env) base
        Pp.extend x env \px -> do
            pfam <- display (Pp.rightOf Prec.arrow env) fam
            pure $ "∀" <> (Pp.parens $ px <+> ":" <+> pbase) <+> "→" <+> pfam
    display' env (Sigma x base fam) = do
        pbase <- display (Pp.rightOf Prec.colon env) base
        Pp.extend x env \px -> do
            pfam <- display (Pp.rightOf Prec.times env) fam
            pure $ (Pp.parens $ px <+> ":" <+> pbase) <+> "×" <+> pfam
    display' env (El tm) = do
        display env tm
    display' env Univ =
        pure "Type"
    display' env Nat =
        pure "ℕ"
