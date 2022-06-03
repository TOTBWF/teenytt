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

passed, atom, comma, colon, delimited, juxtaposition, times, arrow, lambda, in_ :: Prec
passed = nonassoc 8
atom = nonassoc 7
delimited = nonassoc 6
juxtaposition = nonassoc 5
comma = nonassoc 3
colon = nonassoc 3
times = right 2
arrow = right 2
lambda = right 1
in_ = nonassoc 0

instance Display Cell where
    classify _ = delimited
    display' env (Cell names tp) = do
        let pnames = hsep $ toList $ fmap pretty names
        ptp <- display (rightOf colon env) tp
        pure $ parens $ pnames <+> ":" <+> ptp

instance Pretty Pattern where
    pretty (Simple n) = pretty n
    pretty (Inductive v ih) = parens $ pretty v <+> "→" <+> pretty ih

instance Display Case where
    classify _ = lambda
    display' env c = do
        pbody <- display (leftOf arrow env) c.body
        pure $ pretty c.lbl <+> hsep (fmap pretty c.patterns) <+> "→" <+> pbody

instance Display Term_ where
    classify (Var _)        = atom
    classify (Let _ _ _)    = juxtaposition <> in_
    classify (Ann _ _)      = delimited
    classify Hole           = atom
    classify (Incomplete _) = delimited
    classify (Pi _ _)       = arrow
    classify (Lam _ _)      = lambda
    classify (Ap _ _)       = juxtaposition
    classify (Sigma _ _)    = times
    classify (Pair _ _)     = delimited
    classify (Fst _)        = juxtaposition
    classify (Snd _)        = juxtaposition
    classify Univ           = atom
    classify Nat            = atom
    classify (Lit _)        = atom
    classify Zero           = atom
    classify (Suc _)        = juxtaposition
    classify (Elim _ _ _)   = juxtaposition
    classify (LamElim _)    = lambda

    display' env (Var txt) = pure $ pretty txt
    display' env (Let tm x body) = do
        ptm <- display (isolated env) tm
        pbody <- display (rightOf in_ env) body
        pure $ "let" <+> pretty x <+> "=" <+> ptm <+> "in" <+> pbody
    display' env (Ann tm tp) = do
        ptm <- display (leftOf colon env) tm
        ptp <- display (rightOf colon env) tp
        pure $ ptm <+> ":" <+> ptp
    display' env Hole = pure $ "?"
    display' env (Incomplete tm) = do 
        ptm <- display (isolated env) tm
        pure $ "{!" <+> ptm <+> "!}"
    display' env (Pi cells body) = do
        pcells <- traverse (display env) cells
        pbody <- display (rightOf arrow env) body
        pure $ "∀" <+> hsep pcells <+> "→" <+> pbody
    display' env (Lam idents body) = do
        let pidents = hsep $ toList $ fmap pretty idents
        pbody <- display (leftOf lambda env) body
        pure $ "λ" <+> pidents <+> "→" <+> pbody
    display' env (Ap tm args) = do
        ptm <- display (leftOf juxtaposition env) tm
        pargs <- traverse (display (rightOf juxtaposition env)) args
        pure $ ptm <+> hsep pargs
    display' env (Sigma cells body) = do
        pcells <- traverse (display env) cells
        pbody <- display (rightOf times env) body
        pure $ hsep pcells <+> "×" <+> pbody
    display' env (Pair l r) = do
        pl <- display (leftOf comma env) l
        pr <- display (rightOf comma env) l
        pure $ parens $ pl <+> "," <> pr
    display' env (Fst tm) = do
        ptm <- display (rightOf juxtaposition env) tm
        pure $ "fst" <+> ptm
    display' env (Snd tm) = do
        ptm <- display (rightOf juxtaposition env) tm
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
        ptm <- display (rightOf juxtaposition env) tm
        pure $ "suc" <+> ptm
    display' env (Elim mot cases scrut) = do
        pscrut <- display (leftOf juxtaposition env) scrut
        pmot <- display (rightOf juxtaposition env) mot
        pcases <- traverse (display env) cases
        pure $ "elim" <+> pscrut <+> "with" <+> pmot <+> (brackets $ sep $ punctuate "," pcases)
    display' env (LamElim cases) = do
        pcases <- traverse (display env) cases
        pure $ "elim" <+> (brackets $ sep $ punctuate "," pcases)
