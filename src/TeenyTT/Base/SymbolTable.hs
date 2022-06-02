{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A fast, mutable symbol table.
module TeenyTT.Base.SymbolTable
  ( SymbolTable
  , new
  , push
  , pop_
  , pop
  , lookup
  , indexOf
  , nth
  ) where


import Prelude hiding (lookup)

import GHC.Exts
import GHC.Integer.Logarithms

import Control.Monad.Primitive

import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Hashable

import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.MutVar
import Data.Primitive.Types

--------------------------------------------------------------------------------
-- Dynamic Arrays
-- 
-- Because all of our dynamic arrays share a size, we don't want to store
-- the size of the dynamic array alongside the array itself. This is a minor
-- optimization, but we may as well use it.

type DynamicPrimArray s a = MutVar s (MutablePrimArray s a)
type DynamicArray s a = MutVar s (MutableArray s a)

-- | Create a new 'DynamicPrimArray', populating it with the provided initial value.
newDynamicPrimArray :: (PrimMonad m, Prim a) => Int -> a -> m (DynamicPrimArray (PrimState m) a)
newDynamicPrimArray size a = do
    array <- newPrimArray size
    setPrimArray array 0 size a
    newMutVar array

-- | Create a new 'DynamicArray', populating it with the provided initial value.
newDynamicArray :: PrimMonad m => Int -> a -> m (DynamicArray (PrimState m) a)
newDynamicArray size a = do
    array <- newArray size a
    newMutVar array

-- | Index into a 'DynamicPrimArray'.
readDynamicPrimArray :: (PrimMonad m, Prim a) => DynamicPrimArray (PrimState m) a -> Int -> m a
readDynamicPrimArray dyn ix = do
    array <- readMutVar dyn 
    readPrimArray array ix

-- | Index into a 'DynamicArray'.
readDynamicArray :: (PrimMonad m) => DynamicArray (PrimState m) a -> Int -> m a
readDynamicArray dyn ix = do
    array <- readMutVar dyn 
    readArray array ix

-- | Write to a 'DynamicPrimArray'.
writeDynamicPrimArray :: (PrimMonad m, Prim a) => DynamicPrimArray (PrimState m) a -> Int -> a -> m ()
writeDynamicPrimArray dyn ix a = do
    array <- readMutVar dyn
    writePrimArray array ix a

-- | Write to a 'DynamicArray'.
writeDynamicArray :: (PrimMonad m) => DynamicArray (PrimState m) a -> Int -> a -> m ()
writeDynamicArray dyn ix a = do
    array <- readMutVar dyn
    writeArray array ix a

-- | Resize a 'DynamicPrimArray'.
resizeDynamicPrimArray :: (PrimMonad m, Prim a) => DynamicPrimArray (PrimState m) a -> Int -> m ()
resizeDynamicPrimArray dyn size = do
    array <- readMutVar dyn
    resized <- resizeMutablePrimArray array size
    writeMutVar dyn resized

-- | Resize a 'DynamicArray'.
resizeDynamicArray :: (PrimMonad m) => DynamicArray (PrimState m) a -> Int -> Int -> m ()
resizeDynamicArray dyn used capacity = do
    array <- readMutVar dyn
    resized <- newArray capacity undefined
    copyMutableArray resized 0 array 0 used
    writeMutVar dyn resized

--------------------------------------------------------------------------------
-- Symbol Tables
--
-- It's worth exploring our access patterns into the metadata section
-- of the table.

-- | A mutable symbol table.
-- Note that all of the operations on the table are /not/ thread-safe.
data SymbolTable s k a =
    SymbolTable
    { indicies   :: DynamicPrimArray s Int
    , hashes     :: DynamicPrimArray s Int
    , keys       :: DynamicArray s k
    , values     :: DynamicArray s a
    , shadows    :: DynamicPrimArray s Int
    , used       :: MutVar s Int
    , capacity   :: MutVar s Int
    -- ^ The size of the index array.
    -- [INVARIANT]: Must be a power of two.
    }

--------------------------------------------------------------------------------
-- Internal Table Operations

freeSlot :: Int
freeSlot = -1

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n | n <= 0 = 1
nextPowerOfTwo (I# n) = I# (word2Int# (shiftL# 1## (wordLog2# (int2Word# n))))

data Index = Index { hashIndex :: Int, entryIndex :: Int }

pattern Free :: Int -> Index
pattern Free hashIndex <- Index {hashIndex, entryIndex = -1}
    where
      Free hashIndex = Index { entryIndex = -1, .. }

findHashIndex :: forall m k a. (PrimMonad m, Eq k) => k -> Int -> SymbolTable (PrimState m) k a -> m Index
findHashIndex k khash tbl = do
    capacity <- readMutVar tbl.capacity
    let mask = capacity - 1
    let go :: Int -> Int -> m Index
        go i perturb = do
          -- Use a Linear Congruential Generator to probe the hash table
          -- upon a hash collision. To increase the randomness of the probe,
          -- we mix in a decaying number of bits from the key's hash into
          -- the PRNG. This is why we have the invariant that the size must
          -- be a power of two: this specific LCG will visit every single entry
          -- in the index array.
          let loop = go (5 * i + perturb + 1) (shiftR perturb 5)
          let hashIndex = i .&. mask

          entryIndex <- readDynamicPrimArray tbl.indicies hashIndex
          if entryIndex == freeSlot then
            pure (Free hashIndex)
          else do
            ihash <- readDynamicPrimArray tbl.hashes entryIndex
            if (ihash == khash) then do
              ikey <- readDynamicArray tbl.keys entryIndex
              if k == ikey then
                pure (Index {hashIndex, entryIndex})
              else
                loop
            else
              loop
    go (abs khash .&. mask) khash

resize :: (PrimMonad m, Eq k) => SymbolTable (PrimState m) k a -> m ()
resize tbl = do
    used <- readMutVar tbl.used
    let newCapacity = nextPowerOfTwo (3 * used)

    newIndicies <- newPrimArray newCapacity
    setPrimArray newIndicies 0 newCapacity freeSlot
    writeMutVar tbl.indicies newIndicies

    hashes <- resizeDynamicPrimArray tbl.hashes newCapacity
    keys <- resizeDynamicArray tbl.keys used newCapacity
    values <- resizeDynamicArray tbl.values used newCapacity
    shadows <- resizeDynamicPrimArray tbl.shadows newCapacity

    writeMutVar tbl.capacity newCapacity

    -- Recompute all of the hash indicies.
    -- FIXME: Using 'for_' here is probably bad.
    for_ [0..used - 1] $ \i -> do
        ihash <- readDynamicPrimArray tbl.hashes i
        ikey <- readDynamicArray tbl.keys i
        ix <- findHashIndex ikey ihash tbl
        writePrimArray newIndicies ix.hashIndex i

--------------------------------------------------------------------------------
-- Table Operations

-- | Create a new symbol table with a specified starting size.
new :: PrimMonad m => Int -> m (SymbolTable (PrimState m) k a)
new n = do
    let size = nextPowerOfTwo n
    indicies <- newDynamicPrimArray size freeSlot
    hashes <- newDynamicPrimArray size freeSlot
    keys <- newDynamicArray size undefined
    values <- newDynamicArray size undefined
    shadows <- newDynamicPrimArray size freeSlot

    used <- newMutVar 0
    capacity <- newMutVar size

    pure $ SymbolTable {..}

push :: (PrimMonad m, Hashable k, Eq k) => k -> a -> SymbolTable (PrimState m) k a -> m ()
push key value tbl = do
    let khash = hash key
    used <- readMutVar tbl.used
    capacity <- readMutVar tbl.capacity

    Index{..} <- findHashIndex key khash tbl
    writeDynamicPrimArray tbl.indicies hashIndex used
    writeDynamicPrimArray tbl.hashes used khash
    writeDynamicArray tbl.keys used key
    writeDynamicArray tbl.values used value
    writeDynamicPrimArray tbl.shadows used entryIndex
    writeMutVar tbl.used (used + 1)

    if (3 * (used + 1) > 2 * capacity) then
      resize tbl
    else
      pure ()

pop_ :: (PrimMonad m, Eq k) => SymbolTable (PrimState m) k a -> m ()
pop_ tbl = do
    used <- readMutVar tbl.used
    let lastEntry = used - 1
    if lastEntry >= 0 then do
      key <- readDynamicArray tbl.keys lastEntry
      khash <- readDynamicPrimArray tbl.hashes lastEntry

      Index{..} <- findHashIndex key khash tbl
      -- Unshadow the previously bound symbol.
      shadowed <- readDynamicPrimArray tbl.shadows entryIndex
      writeDynamicPrimArray tbl.indicies hashIndex shadowed

      -- Makes sure we don't retain a reference to the key or the value.
      writeDynamicArray tbl.keys lastEntry undefined
      writeDynamicArray tbl.values lastEntry undefined
    else
      pure ()

pop :: (PrimMonad m, Eq k) => SymbolTable (PrimState m) k a -> m (Maybe (k, a))
pop tbl = do
    used <- readMutVar tbl.used
    let lastEntry = used - 1
    if lastEntry >= 0 then do
      key <- readDynamicArray tbl.keys lastEntry
      value <- readDynamicArray tbl.values lastEntry
      khash <- readDynamicPrimArray tbl.hashes lastEntry

      Index{..} <- findHashIndex key khash tbl
      -- Unshadow the previously bound symbol.
      shadowed <- readDynamicPrimArray tbl.shadows entryIndex
      writeDynamicPrimArray tbl.indicies hashIndex shadowed

      -- Makes sure we don't retain a reference to the key or the value.
      writeDynamicArray tbl.keys lastEntry undefined
      writeDynamicArray tbl.values lastEntry undefined
      pure $ Just (key, value)
    else
      pure Nothing

lookup :: (PrimMonad m, Hashable k, Eq k) => k -> SymbolTable (PrimState m) k a -> m (Maybe a)
lookup key tbl =
    findHashIndex key (hash key) tbl >>= \case
      Free _ ->
          pure Nothing
      Index {entryIndex} ->
          Just <$> readDynamicArray tbl.values entryIndex

indexOf :: (PrimMonad m, Hashable k, Eq k) => k -> SymbolTable (PrimState m) k a -> m (Maybe Int)
indexOf key tbl =
    findHashIndex key (hash key) tbl <&> \case
    Free _ ->
        Nothing
    Index {entryIndex} ->
        Just entryIndex

nth :: (PrimMonad m) => Int -> SymbolTable (PrimState m) k a -> m (k, a)
nth ix tbl = do
    key <- readDynamicArray tbl.keys ix
    value <- readDynamicArray tbl.values ix
    pure (key, value)