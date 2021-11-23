module TeenyTT.Core.Refiner.Probe
  ( probe
  ) where

import Data.Foldable
import Data.Traversable

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty
import TeenyTT.Core.Pretty.Unicode qualified as Pp
import TeenyTT.Core.Refiner.Monad

import TeenyTT.Core.Env (Env)

import TeenyTT.Core.Compute
import TeenyTT.Core.Quote qualified as Quote

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Tactic qualified as T

renderGoal :: Env (Cell S.Type) -> S.Type -> Doc ann
renderGoal ctx tp =
    vsep [ vsep (toList $ fmap (dump NoPrec) ctx)
         , Pp.turnstile <+> dump NoPrec tp
         ]

destructCtx :: Env (Cell (a, D.Type)) -> RM (Env (Cell S.Type))
destructCtx env = for env $ \Cell{..} -> do
  qtp <- liftQuote $ Quote.quoteTp $ snd contents
  pure $ Cell { contents = qtp, .. }

printGoal :: D.Type -> RM ()
printGoal goal = do
    qgoal <- liftQuote $ Quote.quoteTp goal
    locals <- getContext
    ctx <- destructCtx locals
    emit $ renderGoal ctx qgoal

probe :: T.Chk -> T.Chk
probe tac = T.Chk $ \goal -> do
    printGoal goal
    T.runChk tac goal
