module TeenyTT.Frontend.Elaborator
  ( chkTp
  , chkTm
  , synTm
  ) where

import Data.List (foldl')

import TeenyTT.Frontend.ConcreteSyntax qualified as CS

import TeenyTT.Core.Ident
import TeenyTT.Core.Position

import TeenyTT.Core.Tactic as T

import TeenyTT.Core.Refiner.El qualified as El
import TeenyTT.Core.Refiner.Hole qualified as Hole
import TeenyTT.Core.Refiner.Nat qualified as Nat
import TeenyTT.Core.Refiner.Pi qualified as Pi
import TeenyTT.Core.Refiner.Structural qualified as Structural
import TeenyTT.Core.Refiner.Univ qualified as Univ


chkTp :: Loc CS.Expr -> T.Tp
chkTp e =
    case (unlocate e) of
      (CS.Univ i)        -> Univ.formation i
      CS.Nat             -> Nat.formation
      (CS.Pi cells body) -> foldr (\(Cell x e) tac -> Pi.formation x (chkTp e) (\_ -> tac)) (chkTp body) cells
      _                  -> El.formation (chkTm e)

-- chkTp (CS.Univ i) = Univ.formation  i
-- chkTp CS.Nat = Nat.formation
-- chkTp (CS.Pi cells body) = foldr (\(Cell x e) tac -> Pi.formation x (chkTp e) (\_ -> tac)) (chkTp body) cells
-- chkTp e = El.formation (chkTm e)

chkTm :: Loc CS.Expr -> T.Chk
chkTm e =
    case (unlocate e) of
      (CS.Lam xs body) -> foldr (\x tac -> Pi.intro x (\_ -> tac)) (chkTm body) xs
      CS.Zero          -> Nat.zero
      (CS.Suc e)       -> Nat.suc (chkTm e)
      (CS.NatLit n)    -> Nat.literal n
      CS.Hole          -> Hole.unleash
      _                -> T.chk (synTm e)

synTm :: Loc CS.Expr -> T.Syn
synTm e =
    case (unlocate e) of
      (CS.App f args) -> foldl' (\tac arg -> Pi.apply tac (chkTm arg)) (synTm f) args
      (CS.Var ident)  -> Structural.var ident
      _               -> error $ show e
      
