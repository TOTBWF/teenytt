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
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST.Strict

import TeenyTT.Base.Diagnostic
import TeenyTT.Base.Env (MutableEnv, Env)
import TeenyTT.Base.Env qualified as Env

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

--------------------------------------------------------------------------------
-- The Evaluation Monad

newtype EvalM s a = EvalM { unEvalM :: ReaderT (MutableEnv s D.Term) (ST s) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (MutableEnv s D.Term), PrimMonad)

runEvalM :: Env D.Term -> (forall s. EvalM s a) -> a
runEvalM env m =
    runST $ do
      mutEnv <- Env.thaw env
      -- NOTE: For very annoying reasons, we can't pattern match on 'EvalM',
      -- so we use an accessor instead.
      runReaderT m.unEvalM mutEnv

var :: Int -> EvalM s D.Term
var ix = do
    env <- ask
    Env.index ix env

clo :: a -> EvalM s (D.Clo a)
clo a = do
    mutEnv <- ask
    env <- Env.freeze mutEnv
    pure $ D.Clo env a

extend :: D.Term -> EvalM s a -> EvalM s a
extend tm m = do
    env <- ask
    Env.push tm env
    a <- m
    Env.pop_ env
    pure a

eval :: S.Term -> EvalM s D.Term
eval (S.Local ix) =
    var ix
eval (S.Global name ~val) =
    pure $ D.global name val
eval (S.Let tm x body) = do
    vtm <- eval tm
    extend vtm $ eval body
eval (S.Lam x body) =
    D.VLam x <$> clo body
eval (S.Ap fn arg) =
    doAp <$> eval fn <*> eval arg
eval S.Hole =
    pure D.hole
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
doAp (D.VNeu hd frms) varg = D.pushFrame hd frms (D.KAp varg) (\vfn -> doAp vfn varg)
doAp _ varg = impossible "bad doAp"

doFst :: D.Term -> D.Term
doFst (D.VPair l _) = l
doFst (D.VNeu hd frms) = D.pushFrame hd frms D.KFst doFst
doFst _ = impossible "bad doFst"

doSnd :: D.Term -> D.Term
doSnd (D.VPair _ r) = r
doSnd (D.VNeu hd frms) = D.pushFrame hd frms D.KSnd doSnd
doSnd _ = impossible "bad doSnd"

doNatElim :: D.Term -> D.Term -> D.Term -> D.Term -> D.Term
doNatElim mot z s D.VZero = z
doNatElim mot z s (D.VSuc tm) = doAp s (doNatElim mot z s tm)
doNatElim mot z s (D.VNeu hd frms) = D.pushFrame hd frms (D.KNatElim mot z s) (doNatElim mot z s)
doNatElim _ _ _ _ = impossible "bad doNatElim"

doEl :: D.Term -> D.Type
doEl (D.VCodePi x base fam) = D.VPi x (doEl base) (elClo fam)
doEl (D.VCodeSigma x base fam) = D.VSigma x (doEl base) (elClo fam)
doEl D.VCodeUniv = D.VUniv
doEl D.VCodeNat = D.VNat
doEl _ = impossible "bad doEl"

elClo :: D.Clo S.Term -> D.Clo S.Type
elClo (D.Clo env tm) = D.Clo env (S.El tm)

instTmClo :: D.Clo S.Term -> D.Term -> D.Term
instTmClo (D.Clo env tm) val = runST do
    mutEnv <- Env.thaw env
    Env.push val mutEnv
    runReaderT (eval tm).unEvalM mutEnv

instTpClo :: D.Clo S.Type -> D.Term -> D.Type
instTpClo (D.Clo env tp) val = runST do
    mutEnv <- Env.thaw env
    Env.push val mutEnv
    runReaderT (evalTp tp).unEvalM mutEnv
