module TeenyTT.Frontend.Elaborator
  (
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict

import TeenyTT.Frontend.ConcreteSyntax qualified as CS

import TeenyTT.Core.Ident

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Tactic as T

import TeenyTT.Core.Refiner.Pi qualified as Pi
import TeenyTT.Core.Refiner.Univ qualified as Univ

-- tpQuantifiers :: (Ident -> T.Tp -> (T.Var -> T.Tp) -> T.Tp) -> T.Tp -> [Cell CS.Expr] -> T.Tp
-- tpQuantifiers quant tac cells = foldr (\(Cell x expr) tfam -> quant x (chkTp expr) (\_ -> tfam)) tac cells

-- chkTp :: CS.Expr -> T.Tp
-- chkTp CS.Univ            = Univ.formation
-- chkTp (CS.Pi cells body) = tpQuantifiers Pi.formation (chkTp body) cells
-- chkTp _ = _

-- chkTm :: CS.Expr -> T.Chk
-- chkTm = _

-- synTm :: CS.Expr -> T.Syn
-- synTm = _
