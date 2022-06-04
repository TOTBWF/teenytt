-- | Refiner rules for the universe.
module TeenyTT.Elaborator.Refiner.Univ
  ( formation
  ) where


import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Elaborator.Tactic qualified as T

formation :: T.Tp
formation = T.Tp $ pure S.Univ
