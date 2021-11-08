module TeenyTT.Core.Refiner.Monad
  ( RM
  , runRM
  , liftEval
  , liftQuote
  , liftConv
  -- * Errors
  , unboundVariable
  , goalMismatch
  , invalidLiteral
  -- * Variable
  , scope
  , Resolved(..)
  , resolve
  , getLocal
  , getContext
  , getGlobal
  ) where


import Control.Monad.Except
import Control.Monad.Reader

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Env, Index, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error(..), Literal, Connective)


import TeenyTT.Core.Eval (Eval)
import TeenyTT.Core.Quote (Quote)
import TeenyTT.Core.Conversion (Convert)

import TeenyTT.Core.Eval qualified as Eval
import TeenyTT.Core.Quote qualified as Quote
import TeenyTT.Core.Conversion qualified as Convert

import TeenyTT.Core.Compute (MonadCompute(..), runCompute)

import TeenyTT.Core.Domain qualified as D

-- | The Refiner Monad.
newtype RM a = RM { unRM :: ReaderT RefineEnv (ExceptT Error IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader RefineEnv)

instance MonadCompute RM where
    liftCompute m = RM $ ReaderT $ \RefineEnv{..} -> ExceptT $ runCompute (fmap contents rm_globals) m
    failure err = RM $ throwError err

runRM :: Env (Cell (D.Value, D.Type)) -> RM a -> IO (Either Error a)
runRM globals (RM m) =
    let env = RefineEnv { rm_locals = Env.empty
                        , rm_globals = globals
                        }
    in runExceptT $ runReaderT m env

--------------------------------------------------------------------------------
-- The Refiner Environment.

data RefineEnv = RefineEnv
    { rm_locals :: Env (Cell (D.Value, D.Type))
    , rm_globals :: Env (Cell (D.Value, D.Type))
    }

-- | Construct an evaluation environment from a refiner environment.
evalEnv :: RefineEnv -> D.Env
evalEnv RefineEnv{..} =
    D.Env { D.vals = fmap (fst . contents) rm_locals
          , D.tps = Env.empty
          }
          

quoteEnv :: RefineEnv -> Quote.Env
quoteEnv RefineEnv{..} =
    Quote.Env { locals = Env.size rm_locals, unfold = Quote.UnfoldNone }

convEnv :: RefineEnv -> Convert.Env
convEnv RefineEnv{..} =
    Convert.Env { locals = Env.size rm_locals }

-- | Pushes a fresh variable to the locals.
fresh :: Ident -> D.Type -> RefineEnv -> RefineEnv
fresh x tp env =
    -- This variable will refer to the /top/ of the environment,
    -- which is what we would expect for closures.
    let mkCell lvl = Cell x (D.var lvl tp, tp)
    in env { rm_locals = Env.push (rm_locals env) mkCell }

--------------------------------------------------------------------------------
-- Errors

unboundVariable :: Ident -> RM a
unboundVariable x = failure $ UnboundVariable x

goalMismatch :: Connective -> D.Type -> RM a
goalMismatch expected actual = do
    qtp <- liftQuote $ Quote.quoteTp actual
    failure $ GoalMismatch expected qtp

invalidLiteral :: Literal -> D.Type -> RM a
invalidLiteral lit tp = do
    qtp <- liftQuote $ Quote.quoteTp tp
    failure $ InvalidLiteral lit qtp

hoistErr :: Either Error a -> RM a
hoistErr (Left err) = failure err
hoistErr (Right a) = pure a

--------------------------------------------------------------------------------
-- Lifting
-- [FIXME: Reed M, 07/11/2021] I should just expose some common primitives

-- | Lift an 'EvM'
liftEval :: Eval a -> RM a
liftEval m = do
    ev_env <- asks evalEnv
    liftCompute $ Eval.runEval ev_env m

liftQuote :: Quote a -> RM a
liftQuote m = do
    qu_env <- asks quoteEnv
    liftCompute $ Quote.runQuote qu_env m

liftConv :: Convert a -> RM a
liftConv m = do
    conv_env <- asks convEnv
    liftCompute $ Convert.runConv conv_env m

--------------------------------------------------------------------------------
-- Variables

scope :: Ident -> D.Type -> (D.Value -> RM a) -> RM a
scope x tp k =
    local (fresh x tp) $ do
    (v, _) <- asks (contents . Env.top . rm_locals)
    k v

data Resolved
    = Local Index
    | Global Level
    | Unbound

resolve :: Ident -> RM Resolved
resolve x = do
    env <- ask
    case Env.findIndex hasName (rm_locals env) of
      Just ix -> pure $ Local ix
      Nothing -> case Env.findLevel hasName (rm_globals env) of
        Just lvl -> pure $ Global lvl
        Nothing  -> pure $ Unbound
    where
      hasName :: Cell a -> Bool
      hasName (Cell {..}) = ident == x

getLocal :: Index -> RM (D.Value, D.Type)
getLocal ix = asks (contents . Env.index ix . rm_locals)

getContext :: RM (Env (Cell (D.Value, D.Type)))
getContext = asks rm_locals

getGlobal :: Level -> RM (D.Value, D.Type)
getGlobal lvl = asks (contents . Env.level lvl . rm_globals)
