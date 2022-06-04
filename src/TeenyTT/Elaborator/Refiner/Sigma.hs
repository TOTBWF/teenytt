-- | Refiner rules for Sigma-Types.
module TeenyTT.Elaborator.Refiner.Sigma
  ( formation
  , intro
  , fst
  , snd
  ) where

import Prelude hiding (fst, snd)

import TeenyTT.Base.Ident

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Tactic qualified as T

formation :: Ident -> T.Tp -> (D.Term -> D.Type -> T.Tp) -> T.Tp
formation x baseTac famTac = T.Tp do
    base <- T.runTp baseTac
    vbase <- evalTp base
    fam <- abstract x vbase \var -> T.runTp (famTac var vbase)
    pure $ S.Sigma x base fam

intro :: T.Chk -> T.Chk -> T.Chk
intro fstTac sndTac = T.Chk \case
    D.VSigma _ base fam -> do
        l <- T.runChk fstTac base
        vl <- eval l
        let fib = instTpClo fam vl
        r <- T.runChk sndTac fib
        pure $ S.Pair l r
    tp ->
        expectedConnective "Σ" tp

fst :: T.Syn -> T.Syn
fst tac = T.Syn do
    (tm, vtp) <- T.runSyn tac
    case vtp of
      D.VSigma _ base _ ->
          pure (S.Fst tm, base)
      tp -> expectedConnective "Σ" tp

snd :: T.Syn -> T.Syn
snd tac = T.Syn do
    (tm, vtp) <- T.runSyn tac
    case vtp of
      D.VSigma _ base fam -> do
          vfst <- eval (S.Fst tm)
          let fib = instTpClo fam vfst
          pure (S.Snd tm, fib)
      tp -> expectedConnective "Σ" tp
