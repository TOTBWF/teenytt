module TeenyTT.Core.Refiner.Univ
  ( formation
  ) where

import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

formation :: Int -> T.Tp
formation i = T.Tp $ pure $ S.Univ i
