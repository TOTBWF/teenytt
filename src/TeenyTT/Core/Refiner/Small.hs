module TeenyTT.Core.Refiner.Small
  ( formation
  , nat
  , pi
  ) where

import Prelude hiding (pi)

import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Eval

import TeenyTT.Core.Error qualified as Err
import TeenyTT.Core.Splice qualified as Splice
import TeenyTT.Core.TermBuilder qualified as TB
import TeenyTT.Core.Tactic qualified as T

formation :: T.Tp -> T.Tp -> T.Tp
formation ta tuniv = T.Tp $ do
    a <- T.runTp ta
    univ <- T.runTp tuniv
    pure $ S.Small a univ

smallTac :: ((D.Type, D.Type) -> RM S.Term) -> T.Chk
smallTac k = T.Chk $ \case
    D.Small tp univ -> k (tp, univ)
    tp              -> goalMismatch Err.Small tp

nat :: T.Chk
nat = smallTac $ \case
    (D.Nat, D.Univ 0) -> pure S.NatSmall
    (D.Nat, univ)     -> goalMismatch (Err.Univ 0) univ
    (a, _)            -> goalMismatch Err.Nat a

pi :: T.Chk -> T.Chk -> T.Chk
pi tbase tfam = smallTac $ \case
    (D.Pi ident base fam, univ) -> do
        baseSmall <- T.runChk tbase (D.Small base univ)
        famtp <- spliceTp $
            Splice.tp base $ \base ->
            Splice.tp univ $ \univ ->
            Splice.tpclo fam $ \fam ->
            Splice.term $ TB.pi ident base (\x -> TB.small (fam x) univ)
        famSmall <- T.runChk tfam famtp
        pure $ S.PiSmall baseSmall famSmall
    (tp, _) -> goalMismatch Err.Pi tp
