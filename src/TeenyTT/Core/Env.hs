module TeenyTT.Core.Env
  ( Env
  , size
  , empty
  , extend
  , push
  , drop
  , Index
  , unIndex
  , index
  , findIndex
  , top
  , Level
  , unLevel
  , level
  , findLevel
  , last
  -- * Potentially Unsafe Operations
  , unsafeIndex
  , unsafeLevel
  ) where

import GHC.Generics
import Control.DeepSeq

import Prelude hiding (last, drop)

import Data.Foldable
import Data.Sequence (Seq(..))
import Data.Sequence qualified as S

import TeenyTT.Core.Pretty

--------------------------------------------------------------------------------
-- Environments
--
-- [FIXME: Reed M, 03/11/2021] Is 'Seq' the right data structure?
-- [FIXME: Reed M, 03/11/2021] Describe the invariants
-- [FIXME: Reed M, 05/11/2021] Rename to 'Telescope'
-- [FIXME: Reed M, 06/11/2021] Remove sanity checks

-- | An Environment.
-- Invariant: @length bindings == size@.
data Env a = Env
    { bindings :: Seq a
    , size :: Int
    }
    deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (Env a)

instance Semigroup (Env a) where
    env0 <> env1 = Env { bindings = bindings env0 <> bindings env1, size = size env0 + size env1 }

instance Monoid (Env a) where
    mempty = Env { bindings = mempty, size = 0 }

empty :: Env a
empty = Env Empty 0

-- | Extend an environment with a new binding.
extend :: Env a -> a -> Env a
extend (Env {..}) x = Env { bindings = bindings :|> x, size = size + 1 }
-- (xs :|> x)

-- | Extend an environment with a new binding that may depend on the current
-- length.
push :: Env a -> (Level -> a) -> Env a
push (Env {..}) f = Env { bindings = bindings :|> (f $ Level size), size = size + 1 }

-- | Get the top variable off an environment.
-- Invariant: The environment must be non-empty.
drop :: Env a -> Env a
drop (Env { bindings = xs :|> _, .. }) = Env { bindings = xs, size = size - 1 }
drop (Env { bindings = Empty}) = error "Invariant Violated: tried to drop a binding off an empty environment."

--------------------------------------------------------------------------------
-- DeBrujin Indexes

-- | DeBruijin Indexes
newtype Index = Index { unIndex :: Int }
    deriving newtype (Eq, Show, NFData)

-- | FIXME: This could be more efficient
index :: Index -> Env a -> a
index (Index ix) (Env {..}) | size > 0 = S.index bindings (size - 1 - ix)
                            | otherwise = error $ "Invalid Index: " <> show ix <> " into environment of size " <> show (length bindings)

findIndex :: (a -> Bool) -> Env a -> Maybe Index
findIndex p (Env {..}) = do
    lvl <- S.findIndexR p bindings
    pure $ Index (size - 1 - lvl)

-- | 'unsafeIndex' has the potential to break the invariant that each index points
-- to some valid place inside of an environment. To use this safely, ensure that
-- you don't mess up your index arithmetic.
unsafeIndex :: Int -> Index
unsafeIndex n | n >= 0 = Index n
              | otherwise = error $ "Invariant Violated: unsafeIndex created an invalid index: " <> show n <> "."

-- | Get the top variable off an environment.
-- Invariant: The environment must be non-empty.
top :: Env a -> a
top (Env { bindings = (_ :|> x) }) = x
top _               = error "Invariant Violated: tried to take the top variable off of an empty environment."


--------------------------------------------------------------------------------
-- DeBrujin Levels

-- | DeBruijin Levels
newtype Level = Level { unLevel :: Int }
    deriving newtype (Eq, Show, NFData)

level :: Level -> Env a -> a
level (Level lvl) (Env {..}) = S.index bindings lvl

findLevel :: (a -> Bool) -> Env a -> Maybe Level
findLevel p (Env {..}) = Level <$> S.findIndexR p bindings

-- | Get the level of the last thing bound.
last :: Env a -> Level
last Env{..} | size > 0 = Level (size - 1)
             | otherwise = error $ "Invariant Violated: last created an invalid level"

-- | 'unsafeLevel' has the potential to break the invariant that each level points
-- to some valid place inside of an environment. To use this safely, ensure that
-- you don't mess up your level arithmetic.
unsafeLevel :: Int -> Level
unsafeLevel n | n >= 0 = Level n
              | otherwise = error $ "Invariant Violated: unsafeLevel created an invalid level: " <> show n <> "."

--------------------------------------------------------------------------------
-- Pretty Printing

instance (Debug a) => Debug (Env a) where
    dump (Env {..}) = hsep $ punctuate ", " (toList $ fmap dump bindings)

instance Debug Index where
    dump (Index ix) = pretty ix

instance Debug Level where
    dump (Level l) = pretty l
