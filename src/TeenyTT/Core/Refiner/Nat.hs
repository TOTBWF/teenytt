module TeenyTT.Core.Refiner.Nat
  ( formation
  , zero
  , suc
  , literal
  ) where

import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Error qualified as Err
import TeenyTT.Core.Tactic qualified as T

formation :: T.Tp
formation = T.Tp $ pure S.Nat

zero :: T.Chk
zero = T.Chk $ \case
    D.Nat -> pure $ S.Zero
    tp    -> goalMismatch Err.Nat tp

suc :: T.Chk -> T.Chk
suc tac = T.Chk $ \case
    D.Nat -> do
        n <- T.runChk tac D.Nat
        pure $ S.Suc n
    tp    -> goalMismatch Err.Nat tp


litTerm :: Int -> S.Term
litTerm 0 = S.Zero
litTerm n = S.Suc (litTerm (n - 1))


literal :: Int -> T.Chk
literal n = T.Chk $ \case
    D.Nat | n >= 0    -> pure $ litTerm n
          | otherwise -> invalidLiteral (Err.NatLit n) D.Nat
    tp                -> goalMismatch Err.Nat tp
