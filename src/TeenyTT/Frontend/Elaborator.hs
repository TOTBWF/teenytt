module TeenyTT.Frontend.Elaborator
  ( chkTp
  , chkTm
  , synTm
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.List (foldl')

import TeenyTT.Frontend.ConcreteSyntax qualified as CS

import TeenyTT.Core.Ident

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Tactic as T

import TeenyTT.Core.Refiner.El qualified as El
import TeenyTT.Core.Refiner.Nat qualified as Nat
import TeenyTT.Core.Refiner.Pi qualified as Pi
import TeenyTT.Core.Refiner.Structural qualified as Structural
import TeenyTT.Core.Refiner.Univ qualified as Univ


chkTp :: CS.Expr -> T.Tp
chkTp (CS.Univ i) = Univ.formation  i
chkTp CS.Nat = Nat.formation
chkTp (CS.Pi cells body) = foldr (\(Cell x e) tac -> Pi.formation x (chkTp e) (\_ -> tac)) (chkTp body) cells
chkTp e = El.formation (chkTm e)

chkTm :: CS.Expr -> T.Chk
chkTm (CS.Lam xs body) = foldr (\x tac -> Pi.intro x (\_ -> tac)) (chkTm body) xs
chkTm (CS.Zero) = Nat.zero
chkTm (CS.Suc e) = Nat.suc (chkTm e)
chkTm (CS.NatLit n) = Nat.literal n
chkTm e = T.chk (synTm e)

synTm :: CS.Expr -> T.Syn
synTm (CS.App f args) = foldl' (\tac arg -> Pi.apply tac (chkTm arg)) (synTm f) args
synTm (CS.Var ident) = Structural.var ident
synTm e = error $ show e
