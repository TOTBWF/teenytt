module TeenyTT.Core.Eval
  ( EvM
  , runEval
  , EvalEnv(..)
  , EvalErr(..)
  , instTmClo
  , instTpClo
  , eval
  , evalTp
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Env, Index, Level)
import TeenyTT.Core.Env qualified as Env

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

-- | The Evaluation Monad.
--
-- All we need access to here are environments, and the ability to throw errors.
newtype EvM a = EvM { unEvM :: ReaderT EvalEnv (Except EvalErr) a }
    deriving (Functor, Applicative, Monad, MonadReader EvalEnv, MonadError EvalErr)

runEval :: EvalEnv -> EvM a -> Either EvalErr a
runEval env (EvM m) = runExcept $ runReaderT m env

--------------------------------------------------------------------------------
-- Error Handling
--
-- TODO: Capture the context an error occured in.

data EvalErr
    = NotAFunction D.Value
    deriving (Show)

evalError :: EvalErr -> EvM a
evalError = throwError

--------------------------------------------------------------------------------
-- Variable + Environment Management
--
-- TODO: Write something about Levels/Indexes
-- TODO: Link to a note about axioms

-- | An Evaluation Environment consists of environments for both local + global bindings.
data EvalEnv = EvalEnv
    { env_globals :: Env (Maybe D.Value)
    , env_locals :: Env D.Value
    }

-- | Lookup a local variable.
getLocal :: Index -> EvM D.Value
getLocal ix = asks (Env.index ix . env_locals)

-- | Lookup a global variable.
getGlobal :: Level -> EvM (Maybe D.Value)
getGlobal lvl = asks (Env.level lvl . env_globals)

-- | Capture the current environment into a closure.
capture :: a -> EvM (D.Clo a)
capture a = do
    locals <- asks env_locals
    pure $ D.Clo locals a

-- | Use a provided local environment for evaluation.
withLocals :: Env D.Value -> EvM a -> EvM a
withLocals locals m = local (\env -> env { env_locals = locals }) m

--------------------------------------------------------------------------------
-- Closures

-- | Instantiate an 'S.Term' closure by providing a value for the additional variable binding.
instTmClo :: (D.Clo S.Term) -> D.Value -> EvM D.Value
instTmClo (D.Clo env tm) v = withLocals env $ eval tm

-- | Instantiate an 'S.Type' closure by providing a value for the additional variable binding.
instTpClo :: (D.Clo S.Type) -> D.Value -> EvM D.Type
instTpClo (D.Clo env tp) v = withLocals env $ evalTp tp

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a term into a value.
eval :: S.Term -> EvM D.Value
eval (S.Local ix)   = getLocal ix
eval (S.Global lvl) = do
    -- See [NOTE: Global Variable Unfolding]
    u <- getGlobal lvl
    pure $ D.Global lvl D.Nil u
eval (S.Lam x body) = do
    clo <- capture body
    pure $ D.Lam x clo
eval (S.App f a)    = do
    vf <- eval f
    va <- eval a
    app vf va
eval S.Zero = pure D.Zero
eval (S.Suc n) = do
    vn <- eval n
    pure $ D.Suc vn

evalTp :: S.Type -> EvM D.Type
evalTp S.Univ = pure D.Univ
evalTp S.Nat = pure D.Nat
evalTp (S.Pi x base fam) = do
    vbase <- evalTp base
    clo <- capture fam
    pure $ D.Pi x vbase clo

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
app :: D.Value -> D.Value -> EvM D.Value
app (D.Lam x clo)       ~a = instTmClo clo a
app (D.Local lvl sp)    ~a = pure $ D.Local lvl (D.App sp a)
app (D.Global lvl sp uf) ~a = do
    -- See [NOTE: Global Variable Unfolding]
    ufa <- traverse (\f -> app f a) uf
    pure $ D.Global lvl (D.App sp a) ufa
app f                   ~_ = evalError (NotAFunction f)
