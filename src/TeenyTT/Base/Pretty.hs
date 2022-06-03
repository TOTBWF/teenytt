{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Pretty-Printing Machinery
module TeenyTT.Base.Pretty
  (
  -- * Precedences
   Prec
  , nonassoc
  , left
  , right
  , prefix
  , postfix
  -- * Display
  , Display(..)
  , display
  , leftOf
  , rightOf
  , surrounded
  , isolated
  , isolateLeft
  , isolateRight
  , presentTop
  -- * Re-Exports
  , Doc
  , Pretty(..)
  , (<+>)
  , vcat
  , hcat
  , vsep
  , hsep
  , sep
  , punctuate
  , indent
  , parens
  , brackets
  ) where

import GHC.TypeLits

import Control.Monad.Primitive
import Control.Monad.ST

import Data.Kind
import Data.Text

import Prettyprinter as PP

import TeenyTT.Base.Ident
import TeenyTT.Base.SymbolTable (SymbolTable)
import TeenyTT.Base.SymbolTable qualified as Tbl

--------------------------------------------------------------------------------
-- Precedences

data Prec = Prec Int Int

instance Semigroup Prec where
    (Prec l _) <> (Prec _ r) = Prec l r

nonassoc :: Int -> Prec
nonassoc n = Prec (2*n) (2*n)

left :: Int -> Prec
left n = Prec (2*n) (2*n + 1)

right :: Int -> Prec
right n = Prec (2*n + 1) (2*n)

prefix :: Int -> Prec
prefix n = Prec maxBound (2*n)

postfix :: Int -> Prec
postfix n = Prec (2*n) maxBound

--------------------------------------------------------------------------------
-- Display Environments
--
-- [NOTE: Mutable Display Environments]
-- It may be tempting to use a @[Text]@ or a @Seq Text@ to store the mapping
-- between DeBruijin Indicies/Levels and their names. However, this can become
-- a /huge/ bottleneck when we are pretty-printing terms with large numbers of nested
-- binders. In @cooltt@, we found that index->name resolution with these datastructures
-- could consume up to 90% (!!!) of the time required to normalize + print terms.
--
-- Therefore, the mutable option is truly the best one we have here from a performance
-- perspective.

data DisplayEnv s =
    DisplayEnv
    { prec :: (Int, Int)
    -- [TODO: Reed M, 02/06/2022] This is probably not the best datastructure?
    -- Somewhat unclear...
    , vars :: SymbolTable s Text Int
    }

leftOf :: Prec -> DisplayEnv s -> DisplayEnv s
leftOf (Prec l _) env = env { prec = (minBound, l) }

rightOf :: Prec -> DisplayEnv s -> DisplayEnv s
rightOf (Prec _ r) env = env { prec = (r, minBound) }

surrounded :: Prec -> DisplayEnv s -> DisplayEnv s
surrounded (Prec l r) env = env { prec = (r, l) }

isolated :: DisplayEnv s -> DisplayEnv s
isolated env = env { prec = (minBound, minBound) }

isolateLeft :: Prec -> DisplayEnv s -> DisplayEnv s
isolateLeft (Prec _ r) env = env { prec = (minBound, r) }

isolateRight :: Prec -> DisplayEnv s -> DisplayEnv s
isolateRight (Prec l _) env = env { prec = (l, minBound) }

shouldParens :: Prec -> DisplayEnv s -> Bool
shouldParens (Prec pl pr) DisplayEnv{ prec = (el, er) } = el >= pl || er >= pr

class Display a where
    classify :: a -> Prec
    display' :: (PrimMonad m) => DisplayEnv (PrimState m) -> a -> m (Doc ())

display :: (PrimMonad m, Display a) => DisplayEnv (PrimState m) -> a -> m (Doc ())
display env a = do
    doc <- display' env a
    if shouldParens (classify a) env then
      pure $ parens doc
    else
      pure $ doc

presentTop :: (Display a) => a -> Doc ()
presentTop a = runST do
    let prec = (minBound, minBound)
    vars <- Tbl.new 120
    display' (DisplayEnv { prec, vars }) a

type family CannotDisplayIdentifiers :: Constraint where
  CannotDisplayIdentifiers = TypeError
    ( 'Text "🚫 You should not try to display identifiers!" ':$$:
      'Text "💡 Use 'bindVar' to bind the identifier in the display environment instead."
    )

instance CannotDisplayIdentifiers => Display Ident where
    classify = undefined
    display' = undefined
