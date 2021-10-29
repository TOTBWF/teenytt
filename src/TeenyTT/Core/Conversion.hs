module TeenyTT.Core.Conversion
  ( ConvM
  , ConvEnv(..)
  , runConv
  , equateTp
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Eval

import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error(..))

import TeenyTT.Core.Compute

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype ConvM a = ConvM { unConvM :: ReaderT ConvEnv CmpM a }
    deriving (Functor, Applicative, Monad, MonadReader ConvEnv, MonadCmp)

runConv :: ConvEnv -> ConvM a -> CmpM a
runConv env (ConvM m) = runReaderT m env

--------------------------------------------------------------------------------
-- Conversion Environments

data ConvEnv = ConvEnv
    { conv_locals :: Int
    }

--------------------------------------------------------------------------------
-- Errors

--------------------------------------------------------------------------------
-- Variables

binders :: Int -> ConvEnv -> ConvEnv
binders n ConvEnv{..} = ConvEnv { conv_locals = n + conv_locals }

bindVar :: (D.Value -> ConvM a) -> ConvM a
bindVar k = local (binders 1) $ do
    n <- asks conv_locals
    k $ D.var (Env.unsafeLevel $ n - 1)

equateTp :: D.Type -> D.Type -> ConvM ()
equateTp D.Univ D.Univ = pure ()
equateTp D.Nat D.Nat = pure ()
equateTp (D.Pi _ base0 fam0) (D.Pi _ base1 fam1) = do
    equateTp base0 base1
    bindVar $ \x -> do
        fib0 <- instTpClo fam0 x
        fib1 <- instTpClo fam1 x
        equateTp fib0 fib1
equateTp tp0 tp1 = failure $ ExpectedEqualTp tp0 tp1
