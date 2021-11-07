module TeenyTT.Core.Conversion
  ( Convert
  , Env(..)
  , runConv
  , equate
  , equateTp
  ) where

import Control.Monad.Reader

import TeenyTT.Core.Quote (Quote)
import TeenyTT.Core.Quote qualified as Q

import TeenyTT.Core.Error (Error(..))

import TeenyTT.Core.Compute

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype Convert a = Convert { unConvert :: ReaderT Env Compute a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadCompute)

runConv :: Env -> Convert a -> Compute a
runConv env m = runReaderT (unConvert m) env

liftQuote :: Quote a -> Convert a
liftQuote m = do
    locals <- asks locals
    liftCompute $ Q.runQuote (Q.Env { locals = locals, unfold = Q.UnfoldAll }) m

--------------------------------------------------------------------------------
-- Conversion Environments

data Env = Env { locals :: Int }

--------------------------------------------------------------------------------
-- Conversion Checking

equate :: D.Type -> D.Value -> D.Value -> Convert ()
equate tp tm0 tm1 = do
    qtm0 <- liftQuote $ Q.quote tp tm0
    qtm1 <- liftQuote $ Q.quote tp tm1
    tmEquals qtm0 qtm1

equateTp :: D.Type -> D.Type -> Convert ()
equateTp tp0 tp1 = do
    qtp0 <- liftQuote $ Q.quoteTp tp0
    qtp1 <- liftQuote $ Q.quoteTp tp1
    tpEquals qtp0 qtp1

-- [FIXME: Reed M, 07/11/2021] This approach isn't great, as we unfold globals too far
-- NOTE: We don't check for global equality here, as they should unfold during quotation.
tmEquals :: S.Term -> S.Term -> Convert ()
tmEquals (S.Lam _ tm0) (S.Lam _ tm1) =
    tmEquals tm0 tm1
tmEquals (S.App fn0 arg0) (S.App fn1 arg1) = do
    tmEquals fn0 fn1
    tmEquals arg0 arg1
tmEquals S.Zero S.Zero =
    pure ()
tmEquals (S.Suc tm0) (S.Suc tm1) =
    tmEquals tm0 tm1
tmEquals (S.Rel tp0 small0) (S.Rel tp1 small1) = do
    tpEquals tp0 tp1
    tmEquals small0 small1
tmEquals S.NatSmall S.NatSmall =
    pure ()
tmEquals (S.PiSmall base0 fam0) (S.PiSmall base1 fam1) = do
    tmEquals base0 base1
    tmEquals fam0 fam1
tmEquals (S.Local ix0) (S.Local ix1) | ix0 == ix1 =
    pure ()
tmEquals tm0 tm1 =
    failure $ ExpectedEqual tm0 tm1
   


-- NOTE: 'S.TpSubst' and 'S.TpVar' don't show up in the image
-- of 'Q.quote', so we don't check for their equality here.
tpEquals :: S.Type -> S.Type -> Convert ()
tpEquals (S.Univ i) (S.Univ j) | i == j =
    pure ()
tpEquals S.Nat S.Nat =
    pure ()
tpEquals (S.Pi _ base0 fam0) (S.Pi _ base1 fam1) = do
    tpEquals base0 fam0
    tpEquals base1 fam1
tpEquals (S.El univ0 tm0) (S.El univ1 tm1) = do
    tpEquals univ0 univ1
    tmEquals tm0 tm1
tpEquals (S.Small tp0 univ0) (S.Small tp1 univ1) = do
    tpEquals tp0 tp1
    tpEquals univ0 univ1
tpEquals tp0 tp1 =
    failure $ ExpectedTpEqual tp0 tp1
