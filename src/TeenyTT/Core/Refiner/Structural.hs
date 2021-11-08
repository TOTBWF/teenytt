module TeenyTT.Core.Refiner.Structural
  ( var
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

var :: Ident -> T.Syn
var x = T.Syn $ resolve x >>= \case
    Local ix -> do
        (_, vtp) <- getLocal ix
        pure (S.Local ix, vtp)
    Global lvl -> do
        (_, vtp) <- getGlobal lvl
        pure (S.Global lvl, vtp)
    Unbound -> unboundVariable x

