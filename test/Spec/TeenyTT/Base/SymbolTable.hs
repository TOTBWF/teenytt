module Spec.TeenyTT.Base.SymbolTable
  ( specs
  , properties
  ) where

import Control.Monad
import Control.Monad.Primitive

import Data.Foldable
import Data.Hashable

import TeenyTT.Base.SymbolTable (SymbolTable)
import TeenyTT.Base.SymbolTable qualified as Tbl

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog.Internal.Property (forAllWithT, forAllT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

withSingleTable :: (PrimMonad m) => (SymbolTable (PrimState m) String Int -> m a) -> m a
withSingleTable k = do
    -- NOTE: Single enough to avoid a resize.
    tbl <- Tbl.new 256
    Tbl.push "0" 0 tbl
    k tbl

withSmallTable :: (PrimMonad m) => (SymbolTable (PrimState m) String Int -> m a) -> m a
withSmallTable k = do
    -- NOTE: Big enough to avoid a resize.
    tbl <- Tbl.new 256
    for_ [0..127] $ \ix ->
      Tbl.push (show ix) ix tbl
    k tbl

withBigTable :: (PrimMonad m) => (SymbolTable (PrimState m) String Int -> m a) -> m a
withBigTable k = do
    -- NOTE: Small enough to trigger (multiple) resizes
    tbl <- Tbl.new 256
    for_ [0..65536] $ \ix ->
      Tbl.push (show ix) ix tbl
    k tbl


specs :: Spec
specs = do
    describe "Symbol Table" $ do
        it "Pops the last element of a single element table" $ (withSingleTable Tbl.pop) `shouldReturn` (Just ("0", 0))
        it "Pops the last element of a small table" $ (withSmallTable Tbl.pop) `shouldReturn` (Just ("127", 127))
        it "Pops the last element of a big table" $ (withBigTable Tbl.pop) `shouldReturn` (Just ("65536", 65536))
        it "Looks up an element in a single element table" $ (withSingleTable (Tbl.lookup "0")) `shouldReturn` (Just 0)
        it "Looks up an element of a small table" $ (withSmallTable (Tbl.lookup "57")) `shouldReturn` (Just 57)
        it "Looks up an element of a big table" $ (withBigTable (Tbl.lookup "1035")) `shouldReturn` (Just 1035)
        it "Index of in a single element table" $ (withSingleTable (Tbl.indexOf "0")) `shouldReturn` (Just 0)
        it "Index of in a small table" $ (withSmallTable (Tbl.indexOf "57")) `shouldReturn` (Just 57)
        it "Index of in a big table" $ (withBigTable (Tbl.indexOf "1035")) `shouldReturn` (Just 1035)

genTbl :: (MonadGen m, PrimMonad m, Hashable k, Eq k) => m k -> m v -> Range Int -> m (SymbolTable (PrimState m) k v)
genTbl keyGen valueGen range = do
    capacity <- Gen.int range
    tbl <- Tbl.new capacity
    used <- Gen.int range
    replicateM_ used $ do
      key <- keyGen
      value <- valueGen
      Tbl.push key value tbl
    pure tbl

properties :: Spec
properties = do
    describe "Symbol Table Property Tests" $ do
        it "push/pop" $ hedgehog $ do
            tbl <- forAllWithT (\_ -> "<table>") table
            k <- forAllT $ key
            v <- forAllT $ value
            Tbl.push k v tbl
            r <- Tbl.pop tbl
            Just (k, v) === r
        it "push/lookup" $ hedgehog $ do
            tbl <- forAllWithT (\_ -> "<table>") table
            k <- forAllT $ key
            v <- forAllT $ value
            Tbl.push k v tbl
            r <- Tbl.lookup k tbl
            Just v === r
      it ""
    where
      key = Gen.text (Range.constant 0 100) Gen.unicode
      value = Gen.int (Range.constant 0 100) 
      table = genTbl key value (Range.linear 0 128)
