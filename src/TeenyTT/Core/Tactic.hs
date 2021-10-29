module TeenyTT.Core.Tactic
  ( Chk(..)
  , Syn(..)
  , Tp(..)
  , chk
  , ann
  -- * Variable Tactics
  , Var(..)
  , abstract
  ) where

import TeenyTT.Core.Ident

import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Conversion
import TeenyTT.Core.Eval

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype Chk = Chk { runChk :: D.Type -> RM S.Term }

newtype Syn = Syn { runSyn :: RM (S.Term, D.Type) }

newtype Tp = Tp { runTp :: RM S.Type }

--------------------------------------------------------------------------------
-- Tactic Conversion

chk :: Syn -> Chk
chk t = Chk $ \goal -> do
    (tm, tp) <- runSyn t
    liftConv $ equateTp goal tp
    pure tm

ann :: Chk -> Tp -> Syn
ann tac tpTac = Syn $ do
    tp <- runTp tpTac
    vtp <- liftEval $ evalTp tp
    tm <- runChk tac vtp
    pure (tm, vtp)

--------------------------------------------------------------------------------
-- Variable Tactics

data Var = Var { vtp :: D.Type, vtm :: D.Value }

abstract :: Ident -> D.Type -> (Var -> RM a) -> RM a
abstract nm tp k =
    scope nm tp $ \v -> k (Var tp v)
