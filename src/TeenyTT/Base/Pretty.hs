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
  -- * Debug Output
  , Debug(..)
  -- * Re-Exports
  , module PP
  ) where

import Data.Functor
import Control.Monad.Primitive

import Data.Char (chr)
import Data.Text
import Prettyprinter as PP

import TeenyTT.Base.Ident
import TeenyTT.Base.SymbolTable (SymbolTable)
import TeenyTT.Base.SymbolTable qualified as Tbl

--------------------------------------------------------------------------------
-- Precedences

data Prec = Prec Int Int

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

data PrecEnv = PrecEnv Int Int

leftOf :: Prec -> PrecEnv
leftOf (Prec l _) = PrecEnv minBound l

rightOf :: Prec -> PrecEnv
rightOf (Prec _ r) = PrecEnv r minBound

surroundedBy :: Prec -> PrecEnv
surroundedBy (Prec l r) = PrecEnv r l

isolated :: PrecEnv
isolated = PrecEnv minBound minBound

isolateLeft :: Prec -> PrecEnv
isolateLeft (Prec _ r) = PrecEnv minBound r

isolateRight :: Prec -> PrecEnv
isolateRight (Prec l r) = PrecEnv l minBound


--------------------------------------------------------------------------------
-- [NOTE: Mutable Display Environments]
-- It may be tempting to use a @[Text]@ or a @Seq Text@ to store the mapping
-- between DeBruijin Indicies/Levels and their names. However, this can become
-- a /huge/ bottleneck when we are pretty-printing terms with large numbers of nested
-- binders. In @cooltt@, we found that index->name resolution with these datastructures
-- could consume up to 90% (!!!) of the time required to normalize + print terms.
--
-- Therefore, the mutable option is truly the best one we have here from a performance
-- perspective.

data DisplayEnv s ann =
    DisplayEnv
    { prec :: PrecEnv
    -- [TODO: Reed M, 02/06/2022] This is probably not the best datastructure?
    -- Somewhat unclear...
    , vars :: SymbolTable s Text Int
    }

class Display a where
    display :: (PrimMonad m) => DisplayEnv (PrimState m) ann -> a -> m (Doc ann)

-- bindVar :: forall m ann a. (PrimMonad m) => Ident -> DisplayEnv (PrimState m) ann -> (Doc ann -> m a) -> m a
-- bindVar ident env k = do
--     _
--     where
--       mangle :: Text -> m (Text, Int)
--       mangle = _
-- --     (mangled, n) <- Tbl.lookup ident env.vars <&> \case
-- --         Just (_, n) -> (subscript n (pretty ident), n + 1)
-- --         Nothing -> (pretty ident, 1)
-- --     Tbl.push ident (mangled, n) env.vars
-- --     a <- k mangled
-- --     Tbl.pop_ env.vars
-- --     pure a
--       subscript :: Int -> Text -> Text
--       subscript 0 doc = doc
--       subscript n doc =
--           -- The unicode subscript characters ₀...₉ occupy codepoints 8320...8329
--           let (d, r) = n `divMod` 10 in
--           subscript d doc <> (pretty $ chr $ 8320 + r)

-- index :: (PrimMonad m) => Int -> DisplayEnv (PrimState m) ann -> m (Doc ann)
-- index ix env =
--     _ <$> Tbl.index ix env.vars

--------------------------------------------------------------------------------
-- Debug

-- | Ugly-printed output, used for dumping core terms when we may
-- not have a way of generating good names.
class Debug a where
    dump :: a -> Doc ann

--------------------------------------------------------------------------------
-- Deriving-Via Helpers
