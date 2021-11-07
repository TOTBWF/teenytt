module TeenyTT.Core.Refiner.El
  ( formation
  ) where

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

-- [FIXME: Reed M, 06/11/2021] Universe levels
formation :: T.Chk -> T.Tp
formation tac = T.Tp $ do
    tm <- T.runChk tac (D.Univ 0)
    pure $ S.El (S.Univ 0) tm
