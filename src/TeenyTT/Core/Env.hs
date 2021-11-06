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

import Prelude hiding (last, drop)

import Data.Sequence (Seq(..))
import Data.Sequence qualified as S


--------------------------------------------------------------------------------
-- Environments
--
-- [FIXME: Reed M, 03/11/2021] Is 'Seq' the right data structure?
-- [FIXME: Reed M, 03/11/2021] Describe the invariants
-- [FIXME: Reed M, 05/11/2021] Rename to 'Telescope'

-- | An Environment.
-- Invariant: @length bindings == size@.
data Env a = Env
    { bindings :: Seq a
    , size :: Int
    }
    deriving (Show, Functor)


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
    deriving (Eq, Show)

-- | FIXME: This could be more efficient
index :: Index -> Env a -> a
index (Index ix) (Env {..}) = S.index bindings (size - 1 - ix)

findIndex :: (a -> Bool) -> Env a -> Maybe Index
findIndex p (Env {..}) = do
    lvl <- S.findIndexR p bindings
    pure $ Index (size - 1 - lvl)

-- | 'unsafeIndex' has the potential to break the invariant that each index points
-- to some valid place inside of an environment. To use this safely, ensure that
-- you don't mess up your index arithmetic.
unsafeIndex :: Int -> Index
unsafeIndex = Index

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

-- | Get the level of the last thing bound.
last :: Env a -> Level
last env = Level (size env - 1)

-- | 'unsafeLevel' has the potential to break the invariant that each level points
-- to some valid place inside of an environment. To use this safely, ensure that
-- you don't mess up your level arithmetic.
unsafeLevel :: Int -> Level
unsafeLevel = Level
