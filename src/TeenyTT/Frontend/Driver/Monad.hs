module TeenyTT.Frontend.Driver.Monad
  ( Driver
  , runDriver
  , liftRM
  , hoistError
  -- * State Management
  , sandbox
  -- * Annotations
  , annotateTp
  , getAnnotation
  -- * Globals
  , getGlobals
  , getGlobal
  , bindGlobal
  -- * Debug
  , setDebugMode
  , getDebugMode
  -- * Output
  , LogLevel(..)
  , message
  , divider
  ) where

import Control.Monad.State.Strict

import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty
import TeenyTT.Core.Env (Env)
import TeenyTT.Core.Env qualified as Env

import TeenyTT.Core.Refiner.Monad (RM)
import TeenyTT.Core.Refiner.Monad qualified as RM

import TeenyTT.Core.Domain qualified as D

newtype Driver a = Driver { unDriver :: StateT DriverState IO a }
    deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadIO)

-- [TODO: Reed M, 07/11/2021] Thread the handle down into the refiner.

-- [FIXME: Reed M, 07/11/2021] The globals uses a bad data structure. I should
-- split the bindings + name resolution up
data DriverState = DriverState
    { globals   :: Env (Cell (D.Value, D.Type))
    , typeAnns  :: Map Ident D.Type
    , debugMode :: Bool
    }

initState :: DriverState
initState = DriverState
    { globals = mempty
    , typeAnns = mempty
    , debugMode = False
    }

runDriver :: Driver a -> IO a
runDriver m = evalStateT (unDriver m) initState

-- [FIXME: Reed M, 06/11/2021] This should use proper pretty printing
hoistError :: (Show err) => Either err a -> Driver a
hoistError (Left err) = liftIO $ fail $ show err
hoistError (Right a) = pure a

liftRM :: RM a -> Driver a
liftRM m = do
    glbs <- getGlobals
    res <- liftIO $ RM.runRM glbs m
    hoistError res

--------------------------------------------------------------------------------
-- State Mangement

-- | 'sandbox' takes a 'Driver' action, and captures it's state.
-- This is particularly useful for benchmarking.
sandbox :: Driver a -> Driver (() -> IO a)
sandbox m = do
    s <- get
    pure $ \_ -> evalStateT (unDriver m) s

--------------------------------------------------------------------------------
-- Type Annotations

-- | Add a type annotation to a top-level identifier.
annotateTp :: Ident -> D.Type -> Driver ()
annotateTp x tp =
    modify' (\st -> st { typeAnns = Map.insert x tp (typeAnns st) })

getAnnotation :: Ident -> Driver (Maybe D.Type)
getAnnotation x =
    gets (Map.lookup x . typeAnns)

--------------------------------------------------------------------------------
-- Globals

getGlobals :: Driver (Env (Cell (D.Value, D.Type)))
getGlobals = gets globals

getGlobal :: Ident -> Driver (Maybe (D.Value, D.Type))
getGlobal x =
    gets (fmap contents . find (\Cell{..} -> ident == x) . globals)

bindGlobal :: Ident -> D.Value -> D.Type -> Driver ()
bindGlobal x val tp =
    modify' (\s -> s { globals = Env.extend (globals s) (Cell x (val, tp)) })

--------------------------------------------------------------------------------
-- Debugging

setDebugMode :: Bool -> Driver ()
setDebugMode b = modify' (\s -> s { debugMode = b })

getDebugMode :: Driver Bool
getDebugMode = gets debugMode


--------------------------------------------------------------------------------
-- Output
-- [TODO: Reed M, 07/11/2021] Make messages/errors prettier

data LogLevel
    = Debug
    | Info
    | Warning
    | Error

divider :: Driver ()
divider = liftIO $ putStrLn (replicate 80 '-')

message :: LogLevel -> Doc ann -> Driver ()
message Debug msg = do
    dbg <- getDebugMode
    when dbg $ putDocLn ("Debug:" <+> msg)
message Info msg =
    putDocLn ("Info:" <+> msg)
message Warning msg =
    putDocLn ("Warning:" <+> msg)
message Error msg =
    putDocLn ("Error:" <+> msg)
