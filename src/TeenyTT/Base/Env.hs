-- | Fast, mutable environments.
module TeenyTT.Base.Env
  ( MutableEnv
  , Env
  , new
  , size
  , sizeM
  -- * DeBruijin Operations
  , index
  , level
  -- * Modification
  , push
  , pop_
  -- * Cloning
  , freeze
  , thaw
  ) where

import Control.Monad.Primitive

import Data.Primitive.Array
import Data.Primitive.MutVar

data MutableEnv s a =
    MutableEnv
    { items     :: MutVar s (MutableArray s a)
    , capacity  :: MutVar s Int
    , used      :: MutVar s Int
    }

data Env a =
    Env
    { items    :: Array a
    , used     :: Int
    }
    deriving stock (Show)

new :: (PrimMonad m) => Int -> m (MutableEnv (PrimState m) a)
new n = do
    array <- newArray n undefined
    items <- newMutVar array
    capacity <- newMutVar n
    used <- newMutVar 0
    pure $ MutableEnv {items, capacity, used}

size :: Env a -> Int
size env = env.used

sizeM :: (PrimMonad m) => MutableEnv (PrimState m) a -> m Int
sizeM env = readMutVar env.used

--------------------------------------------------------------------------------
-- Resizing

resize :: (PrimMonad m) => MutableEnv (PrimState m) a -> m ()
resize env = do
    used <- readMutVar env.used
    capacity <- readMutVar env.capacity
    -- [NOTE: Array Resize Factors]
    -- It's generally a good idea to use √2 as your resize factor as it
    -- leads to a lot less wasted memory, but here we optimize for speed
    -- instead, as using 2 as your resize factor means you only have
    -- one (amortized) copy per insertion.
    let !newCapacity = 2 * capacity

    array <- readMutVar env.items
    resized <- newArray newCapacity undefined
    copyMutableArray resized 0 array 0 used

    writeMutVar env.capacity newCapacity
    writeMutVar env.items resized

--------------------------------------------------------------------------------
-- DeBruijin Operations

{-# INLINE index #-}
index :: (PrimMonad m) => Int -> MutableEnv (PrimState m) a -> m a
index ix env = do
    used <- readMutVar env.used
    items <- readMutVar env.items
    -- [TODO: Reed M, 02/06/2022] Assertions for bounds checks
    if (ix < 0 || ix >= used) then
      error "index: out of bounds index"
    else
      readArray items (used - ix - 1)

{-# INLINE level #-}
level :: (PrimMonad m) => Int -> MutableEnv (PrimState m) a -> m a
level lvl env = do
    used <- readMutVar env.capacity
    items <- readMutVar env.items
    -- [TODO: Reed M, 02/06/2022] Assertions for bounds checks
    if (lvl < 0 || lvl >= used) then
      error "level: out of bounds level"
    else
      readArray items lvl

--------------------------------------------------------------------------------
-- Modification

push :: (PrimMonad m) => a -> MutableEnv (PrimState m) a -> m ()
push a env = do
    items <- readMutVar env.items
    used <- readMutVar env.used
    capacity <- readMutVar env.capacity

    writeArray items used a
    writeMutVar env.used (used + 1)
    if (used + 1 == capacity) then
       resize env
    else
      pure ()

pop_ :: (PrimMonad m) => MutableEnv (PrimState m) a -> m ()
pop_ env = do
    items <- readMutVar env.items
    used <- readMutVar env.used

    -- Don't retain a reference to the item so it can get GC'd.
    writeArray items (used - 1) undefined
    writeMutVar env.used (used - 1)

--------------------------------------------------------------------------------
-- Freezing/Thawing

freeze :: (PrimMonad m) => MutableEnv (PrimState m) a -> m (Env a)
freeze env = do
    used <- readMutVar env.used
    array <- readMutVar env.items
    items <- freezeArray array 0 used
    pure $ Env { items, used }

thaw :: (PrimMonad m) => Env a -> m (MutableEnv (PrimState m) a)
thaw env = do
    used <- newMutVar env.used
    capacity <- newMutVar env.used
    items <- newMutVar =<< thawArray env.items 0 env.used
    pure $ MutableEnv {..}
