module TeenyTT.Core.Env
  ( Env
  , size
  , empty
  , extend
  , push
  , Index
  , index
  , findIndex
  , top
  , Level
  , level
  , findLevel
  -- * Potentially Unsafe Operations
  , unsafeLevel
  ) where

import Data.Sequence (Seq(..))
import Data.Sequence qualified as S

--------------------------------------------------------------------------------
-- Environments
--
-- FIXME: Is 'Seq' the right data structure?
-- FIXME: Describe the invariants

data Env a = Env
    { bindings :: Seq a
    , size :: Int
    }
    deriving (Show, Functor)

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

--------------------------------------------------------------------------------
-- DeBrujin Indexes

-- | DeBruijin Indexes
newtype Index = Index { unIndex :: Int }
    deriving (Eq, Show)

-- | FIXME: This could be more efficient
index :: Index -> Env a -> a
index (Index ix) (Env {..}) = S.index bindings (size - 1 - ix)

findIndex :: (a -> Bool) -> Env a -> Maybe Index
findIndex p (Env {..}) = do
    lvl <- S.findIndexR p bindings
    pure $ Index (size - 1 - lvl)

-- | Get the top variable off an environment.
-- Invariant: The environment must be non-empty.
top :: Env a -> a
top (Env { bindings = (_ :|> x) }) = x
top _               = error "Invariant Violated: tried to take the top variable off of an empty environment."

--------------------------------------------------------------------------------
-- DeBrujin Levels

-- | DeBruijin Levels
newtype Level = Level { unLevel :: Int }
    deriving (Eq, Show)

level :: Level -> Env a -> a
level (Level lvl) (Env {..}) = S.index bindings lvl

findLevel :: (a -> Bool) -> Env a -> Maybe Level
findLevel p (Env {..}) = Level <$> S.findIndexR p bindings

-- | 'unsafeLevel' has the potential to break the invariant that each level points
-- to some valid place inside of an environment. To use this safely, ensure that
-- you don't mess up your level arithmetic.
unsafeLevel :: Int -> Level
unsafeLevel = Level
