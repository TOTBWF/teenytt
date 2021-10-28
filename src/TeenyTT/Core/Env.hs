module TeenyTT.Core.Env
  ( Env
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
  ) where

import Data.Sequence (Seq(..))
import Data.Sequence qualified as S

--------------------------------------------------------------------------------
-- Environments
--
-- FIXME: Should we cache the length in these? Probably
-- FIXME: Is 'Seq' the right data structure?

newtype Env a = Env { unEnv :: Seq a }
    deriving newtype (Show, Functor)

empty :: Env a
empty = Env Empty

-- | Extend an environment with a new binding.
extend :: Env a -> a -> Env a
extend (Env xs) x = Env (xs :|> x)

-- | Extend an environment with a new binding that may depend on the current
-- length.
push :: Env a -> (Level -> a) -> Env a
push (Env xs) f = Env (xs :|> (f (Level $ length xs)))

--------------------------------------------------------------------------------
-- DeBrujin Indexes

-- | DeBruijin Indexes
newtype Index = Index { unIndex :: Int }
    deriving (Eq, Show)

-- | FIXME: This could be more efficient
index :: Index -> Env a -> a
index (Index ix) (Env xs) = S.index xs (length xs - 1 - ix)

findIndex :: (a -> Bool) -> Env a -> Maybe Index
findIndex p (Env xs) = do
    lvl <- S.findIndexR p xs
    pure $ Index (length xs - 1 - lvl)

-- | Get the top variable off an environment.
-- Invariant: The environment must be non-empty.
top :: Env a -> a
top (Env (_ :|> x)) = x
top _               = error "Invariant Violated: tried to take the top variable off of an empty environment."

--------------------------------------------------------------------------------
-- DeBrujin Levels

-- | DeBruijin Levels
newtype Level = Level { unLevel :: Int }
    deriving (Eq, Show)

level :: Level -> Env a -> a
level (Level lvl) (Env xs) = S.index xs lvl

findLevel :: (a -> Bool) -> Env a -> Maybe Level
findLevel p (Env a) = Level <$> S.findIndexR p a

