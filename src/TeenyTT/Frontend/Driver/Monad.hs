-- | The monad for the outer driver of @teenytt@.
module TeenyTT.Frontend.Driver.Monad
  ( Driver
  , runDriver
  -- * State
  , sandbox
  -- * Debugging
  , setDebugMode
  , getDebugMode
  ) where

import Control.Exception
import Control.Monad.Reader

import Data.ByteString (ByteString)

import Data.IORef

import Prettyprinter.Render.Text

import System.Exit

import TeenyTT.Base.Diagnostic (Diagnostic(..))
import TeenyTT.Base.Diagnostic qualified as Diagnostic

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


runDriver :: ByteString -> Driver a -> IO a
runDriver buffer (Driver m) = do
    ref <- newIORef initState
    catch (runReaderT m ref) handler
    where
      handler :: Diagnostic -> IO a
      handler diag = do
          putDoc (Diagnostic.render buffer diag)
          exitWith (ExitFailure 1)

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
-- Debugging

{-# INLINE setDebugMode #-}
setDebugMode :: Bool -> Driver ()
setDebugMode b = modify \st -> st { debugMode = b }

{-# INLINE getDebugMode #-}
getDebugMode :: Driver Bool
getDebugMode = gets \st -> st.debugMode
