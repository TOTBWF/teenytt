module TeenyTT.Core.Quote
  ( Quote
  , Unfold(..)
  , Env(..)
  , runQuote
  , quote
  , quoteNeu
  , quoteTp
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Index, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error(..))

import TeenyTT.Core.Eval

import TeenyTT.Core.Compute

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype Quote a = Quote { unQuote :: ReaderT Env Compute a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadCompute)

runQuote :: Env -> Quote a -> Compute a
runQuote env (Quote m) = runReaderT m env

--------------------------------------------------------------------------------
-- Environments

data Unfold
    = UnfoldNone
    | UnfoldAll

data Env = Env
    { locals :: Int
    , unfold :: Unfold
    }

binders :: Int -> Env -> Env
binders n Env{..} = Env { locals = n + locals, .. }

bindVar :: D.Type -> (D.Value -> Quote a) -> Quote a
bindVar tp k = local (binders 1) $ do
    n <- asks locals
    k $ D.var (Env.unsafeLevel $ n - 1) tp

quoteVar :: Level -> Quote Index
quoteVar lvl = do
    n <- asks locals
    pure $ Env.unsafeIndex $ n - (Env.unLevel lvl + 1)

shouldUnfold :: Quote Unfold
shouldUnfold = asks unfold

--------------------------------------------------------------------------------
-- Quoting

quote :: D.Type -> D.Value -> Quote S.Term
quote D.Nat (D.Zero) =
    pure S.Zero
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
quote _ (D.Cut neu tp) =
    quoteNeu tp neu
quote univ (D.Rel tp small) = do
    qtp <- quoteTp tp
    qsmall <- quote (D.Small tp univ) small
    pure $ S.Rel qtp qsmall
quote tp v =
    failure $ QuotationMismatch tp v

quoteNeu :: D.Type -> D.Neutral -> Quote S.Term
quoteNeu tp (D.Neutral {..}) = do
    unf <- shouldUnfold
    case (unf, hd) of
      (_, D.Local lvl) -> do
          ix <- quoteVar lvl
          quoteSpine (S.Local ix) frames
      (UnfoldNone, D.Global lvl _) ->
          quoteSpine (S.Global lvl) frames
      (UnfoldAll, D.Global _ u) ->
          quote tp u
      (_, D.Hole nm) -> do
          qtp <- quoteTp tp
          quoteSpine (S.Hole nm qtp) frames

quoteSpine :: S.Term -> [D.Frame] -> Quote S.Term
quoteSpine tm [] = pure tm
quoteSpine tm (frm : frms) = do
    tm' <- quoteFrame tm frm
    quoteSpine tm' frms

quoteFrame :: S.Term -> D.Frame -> Quote S.Term
quoteFrame tm (D.App tp arg) = do
    qarg <- quote tp arg
    pure $ S.App tm qarg

quoteLam :: Ident -> D.Type -> (D.Value -> Quote S.Term) -> Quote S.Term
quoteLam x tp k = do
    body <- bindVar tp k
    pure $ S.Lam x body

quoteTpClo :: D.Type -> D.Clo S.Type -> Quote S.Type
quoteTpClo base fam =
    bindVar base $ \v -> do
      tp <- instTpClo fam v
      quoteTp tp

quoteTp :: D.Type -> Quote S.Type
quoteTp (D.Univ l) =
    pure $ S.Univ l
quoteTp D.Nat =
    pure S.Nat
quoteTp (D.Pi x base fam) = do
    qbase <- quoteTp base
    qfam <- quoteTpClo base fam
    pure $ S.Pi x qbase qfam
quoteTp (D.El univ code) = do
    quniv <- quoteTp univ
    qcode <- quote univ code
    pure $ S.El quniv qcode
quoteTp (D.ElCut univ neu) = do
    quniv <- quoteTp univ
    qneu <- quoteNeu univ neu
    pure $ S.El quniv qneu
quoteTp (D.Small tp univ) = do
    qtp <- quoteTp tp
    quniv <- quoteTp univ
    pure $ S.Small qtp quniv
