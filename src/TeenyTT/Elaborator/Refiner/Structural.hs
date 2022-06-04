-- | Structural Refiner Tactics.
module TeenyTT.Elaborator.Refiner.Structural
  ( var
  , letChk
  , letSyn
  , hole
  , incomplete
  ) where

import TeenyTT.Base.Ident

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Tactic qualified as T

var :: Ident -> T.Syn
var name = T.Syn $
    resolve name >>= \case
      Local ix -> do
          (_, vtp) <- getLocal ix
          pure (S.Local ix, vtp)
      Global lvl -> do
          (~v, vtp) <- getGlobal lvl
          pure (S.Global name v, vtp)
      Unbound ->
          unboundVariable name

letChk :: Ident -> T.Syn -> (D.Term -> D.Type -> T.Chk) -> T.Chk
letChk x tmTac bodyTac = T.Chk \goal -> do
  (tm, vtp) <- T.runSyn tmTac
  body <- abstract x vtp \v ->
    T.runChk (bodyTac v vtp) goal
  pure $ S.Let x tm body

letSyn :: Ident -> T.Syn -> (D.Term -> D.Type -> T.Syn) -> T.Syn
letSyn x tmTac bodyTac = T.Syn do
  (tm, vtp) <- T.runSyn tmTac
  (body, vbodyTp) <- abstract x vtp \v ->
    T.runSyn (bodyTac v vtp)
  pure (S.Let x tm body, vbodyTp)

hole :: T.Chk
hole = T.Chk \goal ->
  -- [FIXME: Reed M, 03/06/2022] Print out the hole info. This requires passing in the source buffer to the elaborator.
  pure S.Hole

incomplete :: T.Syn -> T.Chk
incomplete _ = T.Chk \goal ->
    -- [FIXME: Reed M, 03/06/2022] Print out the hole info. This requires passing in the source buffer to the elaborator.
    pure S.Hole
