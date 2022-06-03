-- | The Concrete Syntax Tree for TeenyTT.
module TeenyTT.Elaborator.ConcreteSyntax
  ( Term
  , Term_(..)
  , Cell(..)
  , Case(..)
  , Pattern(..)
  ) where

import GHC.Generics

import Control.DeepSeq

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Base.Location
import TeenyTT.Base.Pretty
import TeenyTT.Base.Prec qualified as Prec

type Term = Loc Term_
data Term_
    = Var Text
    | Let Term Ident Term
    | Ann Term Term
    | Hole
    | Incomplete Term

    | Pi [Cell] Term
    | Lam (NonEmpty Ident) Term
    | Ap Term [Term]

    | Sigma [Cell] Term
    | Pair Term Term
    | Fst Term
    | Snd Term

    | Univ

    | Nat
    | Lit Integer
    | Zero
    | Suc Term

    | Elim Term [Case] Term
    | LamElim [Case]
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Cell = Cell { names :: NonEmpty Ident, tp :: Term }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Case = Case { lbl :: Text, patterns :: [Pattern], body :: Term }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

data Pattern
    = Simple Ident
    | Inductive Ident Ident
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Display
--
-- [NOTE: Displaying Concrete Syntax]
-- We do something a little bit evil here, and use 'pretty' on identifiers
-- instead of 'bindVar'. This is fine for the concrete syntax, as we don't have
-- any tricky shadowing situations like we do with the core syntax.


instance Display Cell where
    classify _ = Prec.delimited
    display' env (Cell names tp) = do
        let pnames = hsep $ toList $ fmap pretty names
        ptp <- display (rightOf Prec.colon env) tp
        pure $ parens $ pnames <+> ":" <+> ptp

instance Pretty Pattern where
    pretty (Simple n) = pretty n
    pretty (Inductive v ih) = parens $ pretty v <+> "→" <+> pretty ih

instance Display Case where
    classify _ = Prec.lambda
    display' env c = do
        pbody <- display (leftOf Prec.arrow env) c.body
        pure $ pretty c.lbl <+> hsep (fmap pretty c.patterns) <+> "→" <+> pbody

instance Display Term_ where
    classify (Var _)        = Prec.atom
    classify (Let _ _ _)    = Prec.juxtaposition <> Prec.in_
    classify (Ann _ _)      = Prec.delimited
    classify Hole           = Prec.atom
    classify (Incomplete _) = Prec.delimited
    classify (Pi _ _)       = Prec.arrow
    classify (Lam _ _)      = Prec.lambda
    classify (Ap _ _)       = Prec.juxtaposition
    classify (Sigma _ _)    = Prec.times
    classify (Pair _ _)     = Prec.delimited
    classify (Fst _)        = Prec.juxtaposition
    classify (Snd _)        = Prec.juxtaposition
    classify Univ           = Prec.atom
    classify Nat            = Prec.atom
    classify (Lit _)        = Prec.atom
    classify Zero           = Prec.atom
    classify (Suc _)        = Prec.juxtaposition
    classify (Elim _ _ _)   = Prec.juxtaposition
    classify (LamElim _)    = Prec.lambda

    display' env (Var txt) = pure $ pretty txt
    display' env (Let tm x body) = do
        ptm <- display (isolated env) tm
        pbody <- display (rightOf Prec.in_ env) body
        pure $ "let" <+> pretty x <+> "=" <+> ptm <+> "in" <+> pbody
    display' env (Ann tm tp) = do
        ptm <- display (leftOf Prec.colon env) tm
        ptp <- display (rightOf Prec.colon env) tp
        pure $ ptm <+> ":" <+> ptp
    display' env Hole = pure $ "?"
    display' env (Incomplete tm) = do 
        ptm <- display (isolated env) tm
        pure $ "{!" <+> ptm <+> "!}"
    display' env (Pi cells body) = do
        pcells <- traverse (display env) cells
        pbody <- display (rightOf Prec.arrow env) body
        pure $ "∀" <+> hsep pcells <+> "→" <+> pbody
    display' env (Lam idents body) = do
        let pidents = hsep $ toList $ fmap pretty idents
        pbody <- display (leftOf Prec.lambda env) body
        pure $ "λ" <+> pidents <+> "→" <+> pbody
    display' env (Ap tm args) = do
        ptm <- display (leftOf Prec.juxtaposition env) tm
        pargs <- traverse (display (rightOf Prec.juxtaposition env)) args
        pure $ ptm <+> hsep pargs
    display' env (Sigma cells body) = do
        pcells <- traverse (display env) cells
        pbody <- display (rightOf Prec.times env) body
        pure $ hsep pcells <+> "×" <+> pbody
    display' env (Pair l r) = do
        pl <- display (leftOf Prec.comma env) l
        pr <- display (rightOf Prec.comma env) l
        pure $ parens $ pl <+> "," <> pr
    display' env (Fst tm) = do
        ptm <- display (rightOf Prec.juxtaposition env) tm
        pure $ "fst" <+> ptm
    display' env (Snd tm) = do
        ptm <- display (rightOf Prec.juxtaposition env) tm
        pure $ "snd" <+> ptm
    display' env Univ = 
        pure "Type"
    display' env Nat =
        pure "ℕ"
    display' env (Lit n) =
        pure $ pretty n
    display' env Zero =
        pure $ "zero"
    display' env (Suc tm) = do
        ptm <- display (rightOf Prec.juxtaposition env) tm
        pure $ "suc" <+> ptm
    display' env (Elim mot cases scrut) = do
        pscrut <- display (leftOf Prec.juxtaposition env) scrut
        pmot <- display (rightOf Prec.juxtaposition env) mot
        pcases <- traverse (display env) cases
        pure $ "elim" <+> pscrut <+> "with" <+> pmot <+> (brackets $ sep $ punctuate "," pcases)
    display' env (LamElim cases) = do
        pcases <- traverse (display env) cases
        pure $ "elim" <+> (brackets $ sep $ punctuate "," pcases)
