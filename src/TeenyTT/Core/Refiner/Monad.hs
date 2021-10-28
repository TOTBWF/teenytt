module TeenyTT.Core.Refiner.Monad
  ( RM
  , runRM
  , liftEval
  -- * Errors
  , RefineErr(..)
  , refineErr
  -- * Variable
  , scope
  , Resolved(..)
  , resolve
  , getLocal
  , getGlobal
  ) where

import Data.Text (Text)

import Control.Monad.Except
import Control.Monad.Reader

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Env, Index, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Eval

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

data Cell a = Cell { ident :: Ident, contents :: a }

-- | The Refiner Monad.
newtype RM a = RM { unRM :: ReaderT RefineEnv (Except RefineErr) a }
    deriving (Functor, Applicative, Monad, MonadReader RefineEnv)

runRM :: Env (Cell (Maybe D.Value, D.Type)) -> RM a -> Either RefineErr a
runRM globals (RM m) =
    let env = RefineEnv { rm_locals = Env.empty
                        , rm_globals = globals
                        }
    in runExcept $ runReaderT m env

--------------------------------------------------------------------------------
-- The Refiner Environment.

data RefineEnv = RefineEnv
    { rm_locals :: Env (Cell (D.Value, D.Type))
    , rm_globals :: Env (Cell (Maybe D.Value, D.Type))
    }

-- | Construct an evaluation environment from a refiner environment.
evalEnv :: RefineEnv -> EvalEnv
evalEnv RefineEnv{..} =
    EvalEnv { env_locals = fmap (fst . contents) rm_locals
            , env_globals = fmap (fst . contents) rm_globals
            }

pushLocal :: Ident -> D.Type -> (Level -> D.Value) -> RefineEnv -> RefineEnv
pushLocal x tp k env =
    let mkCell lvl = Cell x (k lvl, tp)
    in env { rm_locals = Env.push (rm_locals env) mkCell }

--------------------------------------------------------------------------------
-- Refiner Errors

data RefineErr
    = EvalFailed EvalErr
    | UnboundVariable Ident
    | GoalMismatch Text D.Type
    deriving (Show)

hoistErr :: (err -> RefineErr) -> Either err a -> RM a
hoistErr f (Left err) = RM $ throwError (f err)
hoistErr f (Right a) = pure a

refineErr :: RefineErr -> RM a
refineErr err = RM $ throwError err

--------------------------------------------------------------------------------
-- Lifting

-- | Lift an 'EvM'
liftEval :: EvM a -> RM a
liftEval m = do
    ev_env <- asks evalEnv
    hoistErr EvalFailed $ runEval ev_env m

--------------------------------------------------------------------------------
-- Variables

scope :: Ident -> D.Type -> (D.Value -> RM a) -> RM a
scope x tp k =
    local (pushLocal x tp D.var) $ do
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

getGlobal :: Level -> RM (Maybe D.Value, D.Type)
getGlobal lvl = asks (contents . Env.level lvl . rm_globals)
