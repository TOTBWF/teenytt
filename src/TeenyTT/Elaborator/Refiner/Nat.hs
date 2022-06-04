-- | Refiner rules for Nat.
module TeenyTT.Elaborator.Refiner.Nat
  ( formation
  , zero
  , suc
  , literal
  , elim
  ) where

import TeenyTT.Base.Ident

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Splice qualified as Splice
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.TermBuilder qualified as TB

import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Tactic qualified as T

formation :: T.Tp
formation = T.Tp $ pure S.Nat

assertNat :: D.Type -> ElabM ()
assertNat D.VNat = pure ()
assertNat tp = expectedConnective "ℕ" tp

zero :: T.Chk
zero = T.Chk \tp -> do
    assertNat tp
    pure S.Zero

suc :: T.Chk -> T.Chk
suc tac = T.Chk \tp -> do
    assertNat tp
    tm <- T.runChk tac tp
    pure $ S.Suc tm

literal :: Integer -> T.Chk
literal n = 
    case compare n 0 of
      LT -> T.failChk $ outOfBoundsLiteral n D.VNat
      EQ -> zero 
      GT -> suc (literal $ n - 1)

elim :: T.Chk -> T.Chk -> T.Chk -> T.Syn -> T.Syn
elim motTac zTac sTac scrutTac = T.Syn do
    (tscrut, vscrutTp) <- T.runSyn scrutTac
    assertNat vscrutTp
    vscrut <- eval tscrut

    tmot <- T.runChk motTac $ spliceTp $
        Splice.term $
        TB.pi Anon TB.nat \_ -> TB.univ
    vmot <- eval tmot

    tz <- T.runChk zTac $ spliceTp $
        Splice.val vmot \mot ->
        Splice.term $
        TB.el (TB.ap mot TB.zero)

    ts <- T.runChk sTac $ spliceTp $
          Splice.val vmot \mot ->
          Splice.term $
          TB.pi Anon TB.nat \x ->
          TB.pi Anon (TB.el $ TB.ap mot x) \ih ->
          TB.el $ TB.ap mot (TB.suc x)

    let fib = spliceTp $
              Splice.val vscrut \scrut ->
              Splice.val vmot \mot ->
              Splice.term $
              TB.el $ TB.ap mot scrut

    pure (S.NatElim tmot tz ts tscrut, fib)
