-- | Refiner "Macros".
-- These are elaborator tactics that are constructed out of the core refiner rules.
module TeenyTT.Elaborator.Macros
  ( elim
  ) where

import Data.Foldable
import Data.Functor
import Data.Text (Text)

import TeenyTT.Base.Ident

import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.ConcreteSyntax qualified as CS
import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Tactic qualified as T

import TeenyTT.Elaborator.Refiner.Nat qualified as Nat
import TeenyTT.Elaborator.Refiner.Pi qualified as Pi
import TeenyTT.Elaborator.Refiner.Structural qualified as Structural

findCase :: Text -> [(CS.Pattern, T.Chk)] -> Maybe ([CS.PatternArg], T.Chk)
findCase lbl cases =
    find (\(pat, _) -> pat.lbl == lbl) cases <&> \(pat, tac) -> (pat.args, tac)

elim :: T.Chk -> [(CS.Pattern, T.Chk)] -> T.Syn -> T.Syn
elim motTac caseTacs scrutTac =
    T.observe scrutTac \tscrut scrutTp ->
      case scrutTp of
        D.VNat -> do
            zTac <- case findCase "zero" caseTacs of
                Just ([], tac) ->
                    pure tac
                Just _ ->
                    malformedPattern D.VNat
                Nothing ->
                    pure Structural.hole

            sTac <- case findCase "suc" caseTacs of
                Just ([CS.Simple name], tac) ->
                    pure $ Pi.intro name \_ -> Pi.intro Anon \_ -> tac
                Just ([CS.Inductive name ih], tac) -> 
                    pure $ Pi.intro name \_ -> Pi.intro ih \_ -> tac
                Just _ ->
                    malformedPattern D.VNat
                Nothing ->
                    pure Structural.hole

            -- This only makes sense as 'scrut' won't be used under binders.
            let scrut = T.Syn $ pure (tscrut, scrutTp)
            pure $ Nat.elim motTac zTac sTac scrut
        tp ->
            cannotEliminate tp
