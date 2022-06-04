-- | Bidirectional Tactic for Elaboration
module TeenyTT.Elaborator.Tactic (
    -- * Type Formation Tactics
      Tp(..)
    , runTp
    -- * Check Tactics
    , Chk(..)
    , runChk
    , chk
    , match
    -- * Synthesis Tactics
    , Syn(..)
    , runSyn
    , ann
    , observe
    -- * Tactic Class
    , Tactic(..)
    ) where

import TeenyTT.Base.Location

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.Monad

class Tactic tac where
    failure :: (forall a. ElabM a) -> tac
    updateSpan :: Span -> tac -> tac

--------------------------------------------------------------------------------
-- Type Tactics

newtype Tp = Tp { unTp :: ElabM S.Type }

{-# INLINE runTp #-}
runTp :: Tp -> ElabM S.Type
runTp tac = tac.unTp

instance Tactic Tp where
    failure err = Tp err
    updateSpan sp (Tp m) = Tp $ withSpan sp m

--------------------------------------------------------------------------------
-- Check Tactics

newtype Chk = Chk { unChk :: D.Type -> ElabM S.Term }

{-# INLINE runChk #-}
runChk :: Chk -> D.Type -> ElabM S.Term
runChk tac = tac.unChk

instance Tactic Chk where
    failure err = Chk \_ -> err
    updateSpan sp (Chk k) = Chk \goal -> withSpan sp (k goal)

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

instance Tactic Syn where
    failure err = Syn err
    updateSpan sp (Syn m) = Syn (withSpan sp m)

ann :: Chk -> Tp -> Syn
ann tac tpTac = Syn do
    tp <- runTp tpTac
    vtp <- evalTp tp
    tm <- runChk tac vtp
    pure (tm, vtp)


observe :: Syn -> (S.Term -> D.Type -> ElabM Syn) -> Syn
observe synTac k = Syn do
    (tm, vtp) <- runSyn synTac
    tac <- k tm vtp
    runSyn tac
