module TeenyTT.Core.Refiner.Univ
  ( formation
  ) where

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

formation :: T.Tp
formation = T.Tp $ pure S.Univ
