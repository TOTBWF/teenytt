module TeenyTT.Core.Tactic
  ( Chk(..)
  , Syn(..)
  , Tp(..)
  , Var(..)
  , abstract
  ) where

import TeenyTT.Core.Ident

import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype Chk = Chk { runChk :: D.Type -> RM S.Term }

newtype Syn = Syn { runSyn :: RM (S.Term, D.Type) }

newtype Tp = Tp { runTp :: RM S.Type }

data Var = Var { vtp :: D.Type, vtm :: D.Value }

abstract :: Ident -> D.Type -> (Var -> RM a) -> RM a
abstract nm tp k =
    scope nm tp $ \v -> k (Var tp v)
