module TeenyTT.Core.Eval
  ( Eval
  , runEval
  , withLocals
  -- * Closures
  , instTmClo
  , instTpClo
  -- * Splicing
  , splice
  , spliceTp
  , eval
  , evalTp
  -- * Semantic Operations
  , app
  -- , el
  ) where

import Control.Monad.Reader

import TeenyTT.Core.Env (Index)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error as Err

import TeenyTT.Core.Compute
import TeenyTT.Core.Splice (Splice)
import TeenyTT.Core.Splice qualified as Splice

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

-- | The Evaluation Monad.
--
-- All we need access to here are environments, and the ability to throw errors.
newtype Eval a = Eval { unEval :: ReaderT D.Env Compute a }
    deriving newtype (Functor, Applicative, Monad, MonadReader D.Env, MonadCompute)

runEval :: D.Env -> Eval a -> Compute a
runEval env (Eval m) = runReaderT m env

--------------------------------------------------------------------------------
-- Variable + Environment Management
--
-- TODO: Write something about Levels/Indexes
-- TODO: Link to a note about axioms

-- | Lookup a local variable.
getLocal :: Index -> Eval D.Value
getLocal ix = asks (Env.index ix . D.vals)

-- | Lookup a type variable.
getLocalTp :: Index -> Eval D.Type
getLocalTp ix = asks (Env.index ix . D.tps)

-- | Capture the current environment into a closure.
capture :: a -> Eval (D.Clo a)
capture a = do
    locals <- ask
    pure $ D.Clo locals a

--------------------------------------------------------------------------------
-- Closures

-- | Use a provided local environment for evaluation.
withLocals :: (MonadCompute m) => D.Env -> Eval a -> m a
withLocals locals (Eval m) = liftCompute $ runReaderT m locals

-- | Instantiate an 'S.Term' closure by providing a value for the additional variable binding.
instTmClo :: (MonadCompute m) => (D.Clo S.Term) -> D.Value -> m D.Value
instTmClo (D.Clo env tm) v = withLocals (D.bindVal v env) $ eval tm

-- | Instantiate an 'S.Type' closure by providing a value for the additional variable binding.
instTpClo :: (MonadCompute m) => (D.Clo S.Type) -> D.Value -> m D.Type
instTpClo (D.Clo env tp) v = withLocals (D.bindVal v env) $ evalTp tp

--------------------------------------------------------------------------------
-- Splicing

splice :: (MonadCompute m) => Splice S.Term -> m D.Value
splice sp =
    let (env, tm) = Splice.compile sp
    in withLocals env $ eval tm

spliceTp :: (MonadCompute m) => Splice S.Type -> m D.Type
spliceTp sp =
    let (env, tp) = Splice.compile sp
    in withLocals env $ evalTp tp

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a term into a value.
eval :: S.Term -> Eval D.Value
eval (S.Local ix)   = getLocal ix
eval (S.Global lvl) = do
    -- See [NOTE: Global Variable Unfolding]
    (~u, tp) <- getGlobal lvl
    pure $ D.global lvl u tp
eval (S.Lam x body) = do
    clo <- capture body
    pure $ D.Lam x clo
eval (S.App f a) = do
    vf <- eval f
    va <- eval a
    app vf va
eval S.Zero = pure D.Zero
eval (S.Suc n) = do
    vn <- eval n
    pure $ D.Suc vn
eval (S.Rel tp small) = do
    vtp <- evalTp tp
    vsmall <- eval small
    pure $ D.Rel vtp vsmall
eval S.NatSmall = pure D.NatSmall
eval (S.PiSmall base fam) = do
    vbase <- eval base
    vfam <- eval fam
    pure $ D.PiSmall vbase vfam
eval (S.Subst sub tm) = evalSubst sub $ eval tm
eval (S.Hole nm tp) = do
    vtp <- evalTp tp
    pure (D.hole nm vtp)

evalTp :: S.Type -> Eval D.Type
evalTp (S.Univ l) = pure $ D.Univ l
evalTp S.Nat = pure D.Nat
evalTp (S.Pi x base fam) = do
    vbase <- evalTp base
    clo <- capture fam
    pure $ D.Pi x vbase clo
evalTp (S.El univ code) = do
    vuniv <- evalTp univ
    vcode <- eval code
    el vuniv vcode
evalTp (S.Small tp univ) = do
    vtp <- evalTp tp
    vuniv <- evalTp univ
    pure $ D.Small vtp vuniv
evalTp (S.TpVar ix) =
    getLocalTp ix
evalTp (S.TpSubst sub tp) = evalSubst sub $ evalTp tp

evalSubst :: S.Subst -> Eval a -> Eval a
evalSubst S.Id m = m
evalSubst (S.Comp sub0 sub1) m = evalSubst sub0 $ evalSubst sub1 m
evalSubst S.Emp m = local (\env -> env { D.vals = mempty }) m
evalSubst S.Weak m = local (\env -> env { D.vals = Env.drop (D.vals env) }) m
evalSubst (S.Extend sub tm) m = do
    vtm <- eval tm
    local (D.bindVal vtm) $ evalSubst sub m

--------------------------------------------------------------------------------
-- [NOTE: Global Variable Unfolding]
-- Inside of a proof assistant, we often want to use different evaluation strategies
-- in different places. For instance, when we are performing conversion checking, we
-- want to unfold everything. However, when we are displaying terms, we want to
-- keep things as small as possible. Furthermore, we want to be able to share
-- as much of the computation as possible!
--
-- This can be accomplished by adding a bit of non-determinism and laziness to our
-- semantic domain. Specifically, when we encounter some global variable,
-- we keep track of both a stack of values, /and/ a lazily computed
-- version of the same neutral, but with the global unfolded.
--
-- This idea is courtesy of Andras Kovacs + Olle Fredriksson
--
-- We will generally prefix anything value that represents the "unfolded" version
-- with a @u@.


-- | Apply a value to another value.
app :: (MonadCompute m) => D.Value -> D.Value -> m D.Value
app (D.Lam _ clo)       ~a = instTmClo clo a
app (D.Cut neu (D.Pi _ base fam)) ~a = do
    fib <- instTpClo fam a
    cut neu fib (D.App base a) (\f -> app f a)
app f                   ~_ = failure $ Err.ValMismatch Err.Pi f

-- [FIXME: Reed M, 05/11/2021] Is 'el' on a cut handled properly?
el :: (MonadCompute m) => D.Type -> D.Value -> m D.Type
el univ (D.Rel tp small) = pure tp
el univ (D.Cut neu _) = pure $ D.ElCut univ neu
el univ v = failure $ Err.ValMismatch (Err.El univ) v

-- | Push a new 'D.Frame' onto a 'D.Neutral' value, potentially updating the global's unfolding.
cut :: (MonadCompute m) => D.Neutral -> D.Type -> D.Frame -> (D.Value -> m D.Value) -> m D.Value
cut (D.Neutral (D.Global lvl ~u) frms) tp frm ufold = do
    ~uf <- ufold u
    pure $ D.Cut (D.Neutral (D.Global lvl uf) (frm : frms)) tp
cut (D.Neutral hd frms) tp frm _ =
    pure $ D.Cut (D.Neutral hd (frm : frms)) tp
