-- | Refiner rules for the universe.
module TeenyTT.Elaborator.Refiner.Univ
  ( formation
  , el
  ) where


import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Elaborator.Tactic qualified as T

formation :: T.Tp
formation = T.Tp $ pure S.Univ

el :: T.Chk -> T.Tp
el tac = T.Tp do
    tm <- T.runChk tac D.VUniv
    pure $ S.El tm
