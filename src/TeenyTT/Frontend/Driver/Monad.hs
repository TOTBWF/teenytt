-- | The monad for the outer driver of @teenytt@.
module TeenyTT.Frontend.Driver.Monad
  ( Driver
  , runDriver
  -- * State
  , sandbox
  -- * Errors
  , hoistError
  -- * Debugging
  , setDebugMode
  , getDebugMode
  ) where

import Control.Monad.Reader

import Data.IORef

--------------------------------------------------------------------------------
-- [NOTE: Exception Safety + Global State]
-- We have two options here for our state: 'StateT' or 'ReaderT' + 'IORef'.
-- We prefer to use the latter as it hass better semantics when it comes to catching exceptions,
-- which is our preferred mechanism for throwing errors.
--
-- For various reasons, 'ExceptT' is a non-starter. It doesn't compose well, and requires
-- either a mega-error type, or a bunch of nested error types, neither of which is particularly
-- ergonomic. Furthermore, it's rather slow, and requires wrapping/unwrapping eithers /constantly/.

newtype Driver a = Driver { unDriver :: ReaderT (IORef DriverState) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (IORef DriverState), MonadIO)


runDriver :: Driver a -> IO a
runDriver (Driver m) = do
    ref <- newIORef initState
    runReaderT m ref

--------------------------------------------------------------------------------
-- Driver State

data DriverState = DriverState
    { debugMode :: Bool
    }

initState :: DriverState
initState =
    DriverState { debugMode = False
                }

get :: Driver DriverState
get = do
    ref <- ask
    liftIO $ readIORef ref

gets :: (DriverState -> a) -> Driver a
gets k = do
    ref <- ask
    st <- liftIO $ readIORef ref
    pure $ k st

modify :: (DriverState -> DriverState) -> Driver ()
modify k = do
    ref <- ask
    liftIO $ modifyIORef' ref k

-- | 'sandbox' takes a 'Driver' aciton, and clones it's state.
-- This is particularly useful for benchmarking.
sandbox :: Driver a -> Driver (() -> IO a)
sandbox (Driver m) = do
    st <- get
    ref <- liftIO $ newIORef st
    pure $ \_ -> runReaderT m ref

--------------------------------------------------------------------------------
-- Errors

-- [FIXME: Reed M, 01/06/2022] This should use pretty-printing!
{-# INLINE hoistError #-}
hoistError :: (Show e) => Either e a -> Driver a
hoistError (Left err) = liftIO $ fail $ show err
hoistError (Right a) = pure a

--------------------------------------------------------------------------------
-- Debugging

{-# INLINE setDebugMode #-}
setDebugMode :: Bool -> Driver ()
setDebugMode b = modify \st -> st { debugMode = b }

{-# INLINE getDebugMode #-}
getDebugMode :: Driver Bool
getDebugMode = gets \st -> st.debugMode
