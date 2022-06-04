{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
module TeenyTT.Core.Eval
  ( EvalM
  , runEvalM
  , eval
  , evalTp
  -- * Eliminators
  , doAp
  , doFst
  , doSnd
  , doNatElim
  , doEl
  -- * Closures
  , instTmClo
  , instTpClo
  -- * Unfolding
  , unfold
  -- * Splicing
  , spliceTm
  , spliceTp
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST.Strict

import TeenyTT.Base.Diagnostic
import TeenyTT.Base.Env
import TeenyTT.Base.Env qualified as Env

import TeenyTT.Core.Splice (Splice)
import TeenyTT.Core.Splice qualified as Splice
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

--------------------------------------------------------------------------------
-- The Evaluation Monad

newtype EvalM s a = EvalM { unEvalM :: ReaderT (D.MutableEnv s) (ST s) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (D.MutableEnv s), PrimMonad)

runEvalM :: Env D.Term -> (forall s. EvalM s a) -> a
runEvalM env m =
    runST $ do
      values <- Env.thaw env
      types <- Env.new 32
      -- NOTE: For very annoying reasons, we can't pattern match on 'EvalM',
      -- so we use an accessor instead.
      runReaderT m.unEvalM (D.MutableEnv { values, types })

var :: Int -> EvalM s D.Term
var ix = do
    env <- ask
    Env.index ix env.values

tpvar :: Int -> EvalM s D.Type
tpvar ix = do
    env <- ask
    Env.index ix env.types

clo :: a -> EvalM s (D.Clo a)
clo a = do
    mutEnv <- ask
    env <- D.freeze mutEnv
    pure $ D.Clo env a

extend :: D.Term -> EvalM s a -> EvalM s a
extend tm m = do
    env <- ask
    Env.push tm env.values
    a <- m
    Env.pop_ env.values
    pure a

eval :: S.Term -> EvalM s D.Term
eval (S.Local ix) =
    var ix
eval (S.Global name ~val) =
    pure $ D.Global name val []
eval (S.Let x tm body) = do
    vtm <- eval tm
    extend vtm $ eval body
eval (S.Lam x body) =
    D.VLam x <$> clo body
eval (S.Ap fn arg) =
    doAp <$> eval fn <*> eval arg
eval S.Hole =
    pure $ D.Hole []
eval (S.Pair l r) =
    D.VPair <$> eval l <*> eval r
eval (S.Fst tm) =
    doFst <$> eval tm
eval (S.Snd tm) =
    doSnd <$> eval tm
eval S.Zero =
    pure $ D.VZero
eval (S.Suc tm) =
    D.VSuc <$> eval tm
eval (S.NatElim mot z s scrut) =
    doNatElim <$> eval mot <*> eval z <*> eval s <*> eval scrut
eval (S.CodePi x base fam) =
    D.VCodePi x <$> eval base <*> clo fam
eval (S.CodeSigma x base fam) =
    D.VCodeSigma x <$> eval base <*> clo fam
eval S.CodeUniv =
    pure $ D.VCodeUniv
eval S.CodeNat =
    pure $ D.VCodeNat

evalTp :: S.Type -> EvalM s D.Type
evalTp (S.TpVar ix) =
    tpvar ix
evalTp (S.Pi x base fam) =
    D.VPi x <$> evalTp base <*> clo fam
evalTp (S.Sigma x base fam) =
    D.VSigma x <$> evalTp base <*> clo fam
evalTp (S.El tm) =
    doEl <$> eval tm
evalTp S.Univ =
    pure D.VUniv
evalTp S.Nat =
    pure D.VNat

--------------------------------------------------------------------------------
-- Eliminators

doAp :: D.Term -> D.Term -> D.Term
doAp (D.VLam x vclo) varg = instTmClo vclo varg
doAp (D.VNeu neu) varg = D.VNeu $ D.pushFrame neu (D.KAp varg) (\vfn -> doAp vfn varg)
doAp _ varg = impossible "bad doAp"

doFst :: D.Term -> D.Term
doFst (D.VPair l _) = l
doFst (D.VNeu neu) = D.VNeu $ D.pushFrame neu D.KFst doFst
doFst _ = impossible "bad doFst"

doSnd :: D.Term -> D.Term
doSnd (D.VPair _ r) = r
doSnd (D.VNeu neu) = D.VNeu $ D.pushFrame neu D.KSnd doSnd
doSnd _ = impossible "bad doSnd"

doNatElim :: D.Term -> D.Term -> D.Term -> D.Term -> D.Term
doNatElim mot z s D.VZero = z
doNatElim mot z s (D.VSuc tm) = doAp s (doNatElim mot z s tm)
doNatElim mot z s (D.VNeu neu) = D.VNeu $ D.pushFrame neu (D.KNatElim mot z s) (doNatElim mot z s)
doNatElim _ _ _ _ = impossible "bad doNatElim"

doEl :: D.Term -> D.Type
doEl (D.VCodePi x base fam) = D.VPi x (doEl base) (elClo fam)
doEl (D.VCodeSigma x base fam) = D.VSigma x (doEl base) (elClo fam)
doEl D.VCodeUniv = D.VUniv
doEl D.VCodeNat = D.VNat
doEl (D.VNeu neu) = D.VElNeu neu
doEl _ = impossible $ "bad doEl"

elClo :: D.Clo S.Term -> D.Clo S.Type
elClo (D.Clo env tm) = D.Clo env (S.El tm)

instTmClo :: D.Clo S.Term -> D.Term -> D.Term
instTmClo (D.Clo env tm) val = runST do
    mutEnv <- D.thaw env
    Env.push val mutEnv.values
    runReaderT (eval tm).unEvalM mutEnv

instTpClo :: D.Clo S.Type -> D.Term -> D.Type
instTpClo (D.Clo env tp) val = runST do
    mutEnv <- D.thaw env
    Env.push val mutEnv.values
    runReaderT (evalTp tp).unEvalM mutEnv

--------------------------------------------------------------------------------
-- Unfolding

unfold :: D.Term -> D.Term
unfold (D.VNeu (D.Neu { hd = D.KGlobal _ v })) = v
unfold v = v

--------------------------------------------------------------------------------
-- Splicing

spliceTm :: (forall s. Splice (ST s) S.Term) -> D.Term
spliceTm splice = runST do
    (env, tm) <- Splice.compile splice
    runReaderT (eval tm).unEvalM env

spliceTp :: (forall s. Splice (ST s) S.Type) -> D.Type
spliceTp splice = runST do
    (env, tp) <- Splice.compile splice
    runReaderT (evalTp tp).unEvalM env
