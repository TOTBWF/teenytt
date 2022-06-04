-- | Refiner tactics for Pi-Types.
module TeenyTT.Elaborator.Refiner.Pi
  ( formation
  , intro
  , elim
  ) where

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
    pure $ S.Pi x base fam

intro :: Ident -> (D.Term -> T.Chk) -> T.Chk
intro x bodyTac = T.Chk \case
    D.VPi _ base fam ->
        abstract x base \var -> do
          let fib = instTpClo fam var
          body <- T.runChk (bodyTac var) fib
          pure $ S.Lam x body
    tp -> expectedConnective "Π" tp

elim :: T.Syn -> T.Chk -> T.Syn
elim fnTac argTac = T.Syn do
    (fn, vfnTp) <- T.runSyn fnTac
    case vfnTp of
      D.VPi _ base fam -> do
          arg <- T.runChk argTac base
          varg <- eval arg
          let fib = instTpClo fam varg
          pure (S.Ap fn arg, fib)
      tp -> expectedConnective "Π" tp 
