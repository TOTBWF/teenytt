module TeenyTT.Core.Quote
  ( QuM
  , Unfold(..)
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

data Unfold
    = UnfoldNone
    | UnfoldAll

data QuoteEnv = QuoteEnv
    { qu_locals :: Int
    , qu_unfold :: Unfold
    }

binders :: Int -> QuoteEnv -> QuoteEnv
binders n QuoteEnv{..} = QuoteEnv { qu_locals = n + qu_locals, .. }

bindVar :: D.Type -> (D.Value -> QuM a) -> QuM a
bindVar tp k = local (binders 1) $ do
    n <- asks qu_locals
    k $ D.var (Env.unsafeLevel $ n - 1) tp

quoteVar :: Level -> QuM Index
quoteVar lvl = do
    n <- asks qu_locals
    pure $ Env.unsafeIndex $ n - (Env.unLevel lvl + 1)

shouldUnfold :: QuM Unfold
shouldUnfold = asks qu_unfold

--------------------------------------------------------------------------------
-- Quoting

quote :: D.Type -> D.Value -> QuM S.Term
quote D.Nat (D.Zero) = pure S.Zero
quote D.Nat (D.Suc n) = do
    qn <- quote D.Nat n
    pure $ S.Suc qn
quote (D.Pi _ base fam) (D.Lam x clo) =
    quoteLam x base $ \arg -> do
    fib <- instTpClo fam arg
    ret <- instTmClo clo arg
    quote fib ret
quote (D.Pi x base fam) v =
    quoteLam x base $ \arg -> do
    fib <- instTpClo fam arg
    ret <- app v arg
    quote fib ret
quote _ (D.Cut neu tp) = quoteNeu tp neu
quote tp v = failure $ QuotationMismatch tp v

quoteNeu :: D.Type -> D.Neutral -> QuM S.Term
quoteNeu _ (D.Neutral {hd = D.Local lvl, frames}) = do
    ix <- quoteVar lvl
    quoteSpine (S.Local ix) frames
quoteNeu tp (D.Neutral {hd = D.Global lvl u, frames}) =
    shouldUnfold >>= \case
      UnfoldNone -> quoteSpine (S.Global lvl) frames
      UnfoldAll -> quote tp u


quoteSpine :: S.Term -> [D.Frame] -> QuM S.Term
quoteSpine tm [] = pure tm
quoteSpine tm (frm : frms) = do
    tm' <- quoteFrame tm frm
    quoteSpine tm' frms

quoteFrame :: S.Term -> D.Frame -> QuM S.Term
quoteFrame tm (D.App tp arg) = do
    qarg <- quote tp arg
    pure $ S.App tm qarg

quoteLam :: Ident -> D.Type -> (D.Value -> QuM S.Term) -> QuM S.Term
quoteLam x tp k = do
    body <- bindVar tp k
    pure $ S.Lam x body

quoteTpClo :: D.Type -> D.Clo S.Type -> QuM S.Type
quoteTpClo base fam =
    bindVar base $ \v -> do
      tp <- instTpClo fam v
      quoteTp tp

quoteTp :: D.Type -> QuM S.Type
quoteTp D.Univ = pure S.Univ
quoteTp D.Nat = pure S.Nat
quoteTp (D.Pi x base fam) = do
    qbase <- quoteTp base
    qfam <- quoteTpClo base fam
    pure $ S.Pi x qbase qfam

