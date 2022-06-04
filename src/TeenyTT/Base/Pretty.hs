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
  , DisplayEnv
  , initEnv
  , display
  -- ** Precedences
  , leftOf
  , rightOf
  , surrounded
  , isolated
  , isolateLeft
  , isolateRight
  -- ** Variables
  , index
  , extend
  , presentTop
  , module PP
  ) where

import GHC.TypeLits

import Control.Monad.Primitive
import Control.Monad.ST

import GHC.Char (chr)
import Data.Functor
import Data.Kind

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
    , vars :: SymbolTable s Ident (Int, Doc ())
    }

initEnv :: (PrimMonad m) => m (DisplayEnv (PrimState m))
initEnv = do
    let prec = (minBound, minBound)
    vars <- Tbl.new 128
    pure $ DisplayEnv {prec, vars}

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

extend :: (PrimMonad m) => Ident -> DisplayEnv (PrimState m) -> (Doc () -> m a) -> m a
extend ident env k = do 
    (n, doc) <- Tbl.lookup ident env.vars <&> \case
        Just (n, doc) -> (n, doc)
        Nothing -> (0, pretty ident)
    let mangled = mangle n doc
    Tbl.push ident (n + 1, mangled) env.vars
    a <- k mangled
    Tbl.pop_ env.vars
    pure a
    where
      mangle :: Int -> Doc ann -> Doc ann
      mangle 0 doc = fuse Shallow doc
      mangle n doc =
          let (d, r) = n `divMod` 10
              subscript = pretty $ chr $ 8320 + r
          in mangle d doc <> subscript

index :: (PrimMonad m) => Int -> DisplayEnv (PrimState m) -> m (Doc ())
index ix env =
    snd <$> Tbl.index ix env.vars

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
    vars <- Tbl.new 128
    display' (DisplayEnv { prec, vars }) a

type family CannotDisplayIdentifiers :: Constraint where
  CannotDisplayIdentifiers = TypeError
    ( 'Text "🚫 You should not try to display identifiers!" ':$$:
      'Text "💡 Use 'extend' to bind the identifier in the display environment instead."
    )

instance CannotDisplayIdentifiers => Display Ident where
    classify = undefined
    display' = undefined
