module TeenyTT.Core.Refiner.Pi
  ( formation
  , intro
  , apply
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Refiner.Monad
import TeenyTT.Core.Eval

import TeenyTT.Core.Error qualified as Err

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

formation :: Ident -> T.Tp -> (T.Var -> T.Tp) -> T.Tp
formation nm tbase tfam = T.Tp $ do
    base <- T.runTp tbase
    vbase <- liftEval $ evalTp base
    fam <- T.abstract nm vbase $ \var -> T.runTp (tfam var)
    pure $ S.Pi nm base fam

intro :: Ident -> (T.Var -> T.Chk) -> T.Chk
intro nm tbody = T.Chk $ \case
    D.Pi _ base fam -> T.abstract nm base $ \var -> do
        fib <- liftEval $ instTpClo fam (T.vtm var)
        body <- T.runChk (tbody var) fib
        pure $ S.Lam nm body
    tp -> goalMismatch Err.Pi tp

apply ::T.Syn -> T.Chk -> T.Syn
apply tfun targ = T.Syn $ do
    (fun, tp) <- T.runSyn tfun
    case tp of
      (D.Pi _ base fam) -> do
          arg <- T.runChk targ base
          varg <- liftEval $ eval arg
          fib <- liftEval $ instTpClo fam varg
          pure (S.App fun arg, fib)
      _ -> goalMismatch Err.Pi tp
