module TeenyTT.Core.Quote
  ( QuM
  , QuoteEnv(..)
  , runQuote
  , quote
  , quoteTp
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Env, Index, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error(..))

import TeenyTT.Core.Eval

import TeenyTT.Core.Compute

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype QuM a = QuM { unQuM :: ReaderT QuoteEnv CmpM a }
    deriving (Functor, Applicative, Monad, MonadCmp, MonadReader QuoteEnv)

runQuote :: QuoteEnv -> QuM a -> CmpM a
runQuote env (QuM m) = runReaderT m env

--------------------------------------------------------------------------------
-- Environments

data QuoteEnv = QuoteEnv { qu_locals :: Int }

binders :: Int -> QuoteEnv -> QuoteEnv
binders n QuoteEnv{..} = QuoteEnv { qu_locals = n + qu_locals }

bindVar :: (D.Value -> QuM a) -> QuM a
bindVar k = local (binders 1) $ do
    n <- asks qu_locals
    k $ D.var (Env.unsafeLevel $ n - 1)

--------------------------------------------------------------------------------
-- Quoting

quote :: D.Type -> D.Value -> QuM S.Term
quote D.Nat (D.Zero) = pure S.Zero
quote D.Nat (D.Suc n) = do
    qn <- quote D.Nat n
    pure $ S.Suc qn
quote (D.Pi _ base fam) (D.Lam x clo) =
    quoteLam x $ \arg -> do
    fib <- instTpClo fam arg
    ret <- instTmClo clo arg
    quote fib ret
quote (D.Pi x base fam) v =
    quoteLam x $ \arg -> do
    fib <- instTpClo fam arg
    ret <- app v arg
    quote fib ret
quote tp v = failure $ QuotationMismatch tp v

quoteLam :: Ident -> (D.Value -> QuM S.Term) -> QuM S.Term
quoteLam x k = do
    body <- bindVar k
    pure $ S.Lam x body

quoteTpClo :: D.Type -> D.Clo S.Type -> QuM S.Type
quoteTpClo base fam =
    bindVar $ \v -> do
      tp <- instTpClo fam v
      quoteTp tp

quoteTp :: D.Type -> QuM S.Type
quoteTp D.Univ = pure S.Univ
quoteTp D.Nat = pure S.Nat
quoteTp (D.Pi x base fam) = do
    qbase <- quoteTp base
    qfam <- quoteTpClo base fam
    pure $ S.Pi x qbase qfam

