module TeenyTT.Core.Refiner.Hole
  ( unleash
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Quote qualified as Quote

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

import TeenyTT.Core.Refiner.Probe qualified as Probe

unleash :: T.Chk
unleash = Probe.probe $ T.Chk $ \goal -> do
    qgoal <- liftQuote $ Quote.quoteTp goal
    pure $ S.Hole Anon qgoal
