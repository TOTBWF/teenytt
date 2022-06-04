-- | Bidirectional Tactic for Elaboration
module TeenyTT.Elaborator.Tactic (
    -- * Type Formation Tactics
      Tp(..)
    , runTp
    , failTp
    -- * Check Tactics
    , Chk(..)
    , runChk
    , chk
    , failChk
    , match
    -- * Synthesis Tactics
    , Syn(..)
    , runSyn
    , ann
    , failSyn
    , observe
    ) where

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.Monad

--------------------------------------------------------------------------------
-- Type Tactics

newtype Tp = Tp { unTp :: ElabM S.Type }

{-# INLINE runTp #-}
runTp :: Tp -> ElabM S.Type
runTp tac = tac.unTp

failTp :: (forall a. ElabM a) -> Tp
failTp m = Tp m

--------------------------------------------------------------------------------
-- Check Tactics

newtype Chk = Chk { unChk :: D.Type -> ElabM S.Term }

{-# INLINE runChk #-}
runChk :: Chk -> D.Type -> ElabM S.Term
runChk tac = tac.unChk

{-# INLINE chk #-}
chk :: Syn -> Chk
chk tac = Chk \goal -> do
  (tm, vtp) <- runSyn tac
  equateTp goal vtp
  pure tm

failChk :: (forall a. ElabM a) -> Chk
failChk m = Chk \_ -> m

match :: (D.Type -> ElabM Chk) -> Chk
match k = Chk \goal -> do
  tac <- k goal
  runChk tac goal

--------------------------------------------------------------------------------
-- Synthesis Tactics

newtype Syn = Syn { unSyn :: ElabM (S.Term, D.Type) }

{-# INLINE runSyn #-}
runSyn :: Syn -> ElabM (S.Term, D.Type)
runSyn tac = tac.unSyn

ann :: Chk -> Tp -> Syn
ann tac tpTac = Syn do
    tp <- runTp tpTac
    vtp <- evalTp tp
    tm <- runChk tac vtp
    pure (tm, vtp)

failSyn :: (forall a. ElabM a) -> Syn
failSyn m = Syn m

observe :: Syn -> (S.Term -> D.Type -> ElabM Syn) -> Syn
observe synTac k = Syn do
    (tm, vtp) <- runSyn synTac
    tac <- k tm vtp
    runSyn tac
