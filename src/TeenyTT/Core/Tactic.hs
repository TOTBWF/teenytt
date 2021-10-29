module TeenyTT.Core.Tactic
  ( Chk(..)
  , Syn(..)
  , Tp(..)
  -- * Tactic Conversion
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

-- | A @Chk@ tactic can be thought of as a tactic that does nothing
-- more than mechanical decomposition.
newtype Chk = Chk { runChk :: D.Type -> RM S.Term }

-- | A @Syn@ tactic can be thought of as 2 classes of tactics:
-- * Things like variable lookup that already "know" their type
-- * Things like function application that need to "invent" information.
newtype Syn = Syn { runSyn :: RM (S.Term, D.Type) }

-- | A @Tp@ tactic is used to synthesize types, rather than any sort of term.
-- These correspond to the type formation rules.
newtype Tp = Tp { runTp :: RM S.Type }

--------------------------------------------------------------------------------
-- Tactic Conversion

-- | @chk tac@ will convert a 'Syn' tactic into a 'Chk' tactic by
-- synthesizing something, and checking that the synthesized type
-- is the same as the one we are checking against.
chk :: Syn -> Chk
chk t = Chk $ \goal -> do
    (tm, tp) <- runSyn t
    liftConv $ equateTp goal tp
    pure tm

-- | @ann tac tp@ takes a 'Chk' tactic and transforms it into a 'Syn' one
-- by providing it with a sort of "type annotation".
ann :: Chk -> Tp -> Syn
ann tac tpTac = Syn $ do
    tp <- runTp tpTac
    vtp <- liftEval $ evalTp tp
    tm <- runChk tac vtp
    pure (tm, vtp)

--------------------------------------------------------------------------------
-- Variables

-- | A 'Var' is used to ensure that we handle binding structures properly.
data Var = Var { vtp :: D.Type, vtm :: D.Value }

abstract :: Ident -> D.Type -> (Var -> RM a) -> RM a
abstract nm tp k =
    scope nm tp $ \v -> k (Var tp v)
