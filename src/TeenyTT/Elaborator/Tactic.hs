{-# LANGUAGE DataKinds #-}
-- | Bidirectional Tactics for Elaboration
module TeenyTT.Elaborator.Tactic
  ( Chk(..)
  , runChk
  , Syn(..)
  , runSyn
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

--------------------------------------------------------------------------------
-- Check Tactics

newtype Chk = Chk { unChk :: D.Type -> ElabM S.Term }

{-# INLINE runChk #-}
runChk :: Chk -> D.Type -> ElabM S.Term
runChk tac = tac.unChk

{-# INLINE syn #-}
syn :: Syn -> Chk
syn tac = Chk \goal -> do
  (tm, vtp) <- runSyn tac
  equateTp goal vtp
  pure tm

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
