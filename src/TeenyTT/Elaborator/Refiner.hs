-- | The core of the elaboration algorithm.
module TeenyTT.Elaborator.Refiner
  ( typ
  , chk
  , syn
  ) where

import Data.Foldable
import Data.Functor

import TeenyTT.Base.Ident
import TeenyTT.Base.Location

import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.ConcreteSyntax qualified as CS
import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Macros qualified as Macros
import TeenyTT.Elaborator.Tactic qualified as T

import TeenyTT.Elaborator.Refiner.Nat qualified as Nat
import TeenyTT.Elaborator.Refiner.Pi qualified as Pi
import TeenyTT.Elaborator.Refiner.Sigma qualified as Sigma
import TeenyTT.Elaborator.Refiner.Structural qualified as Structural
import TeenyTT.Elaborator.Refiner.Univ qualified as Univ

typCell :: (Ident -> T.Tp -> (D.Term -> D.Type -> T.Tp) -> T.Tp) -> CS.Cell -> T.Tp -> T.Tp
typCell k cell baseTac =
    let tpTac = typ cell.tp
    in foldr (\ident tac -> k ident tpTac (\_ _ -> tac)) baseTac cell.names

typ :: CS.Term -> T.Tp
typ locTp =
    case unlocate locTp of
      CS.Pi cells body ->
          foldr (typCell Pi.formation) (typ body) cells
      CS.Sigma cells body ->
          foldr (typCell Sigma.formation) (typ body) cells
      CS.Univ ->
          Univ.formation
      CS.Nat ->
          Nat.formation
      _ ->
          T.failTp $ notAType locTp

chk :: CS.Term -> T.Chk
chk locTm =
    case unlocate locTm of
      CS.Let x tm body ->
          Structural.letChk x (syn tm) (\_ _ -> chk body)
      CS.Hole ->
          Structural.hole
      CS.Incomplete tm ->
          Structural.incomplete (syn tm)

      CS.Pi cells body ->
          T.failChk $ notImplemented locTm
      CS.Lam args body ->
          foldr (\arg tac -> Pi.intro arg \_ -> tac) (chk body) (toList args)

      CS.Sigma cells body ->
          T.failChk $ notImplemented locTm
      CS.Pair l r ->
          Sigma.intro (chk l) (chk r)

      CS.Univ ->
          T.failChk $ notImplemented locTm

      CS.Nat ->
          T.failChk $ notImplemented locTm
      CS.Lit n ->
          Nat.literal n
      CS.Suc tm ->
          Nat.suc (chk tm)

      CS.LamElim cases ->
          T.failChk $ notImplemented locTm
      _ -> T.chk (syn locTm)


syn :: CS.Term -> T.Syn
syn locTm =
    case unlocate locTm of
      CS.Var x ->
          Structural.var (User x)
      CS.Let x tm body ->
          Structural.letSyn x (syn tm) (\_ _ -> syn body)
      CS.Ann tm tp ->
          T.ann (chk tm) (typ tp)

      (CS.Ap fn args) ->
          foldl' Pi.elim (syn fn) $ fmap chk args

      (CS.Fst tm) ->
          Sigma.fst (syn tm)
      (CS.Snd tm) ->
          Sigma.snd (syn tm)

      (CS.Elim mot cases scrut) ->
          let caseTacs = cases <&> \case_ ->
                (case_.pattern, chk case_.body)
          in Macros.elim (chk mot) caseTacs (syn scrut)
      _ ->
          T.failSyn $ cannotSynth locTm
