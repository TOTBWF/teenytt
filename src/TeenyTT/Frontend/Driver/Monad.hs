{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
-- | The monad for the outer driver of @teenytt@.
module TeenyTT.Frontend.Driver.Monad
  ( Driver
  , runDriver
  -- * State
  , sandbox
  -- * Debugging
  , setDebugMode
  , getDebugMode
  -- * Elaboration
  , elab
  , annotateTp
  , getAnnotation
  , define
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Reader

import Data.ByteString (ByteString)

import Data.IORef

import Prettyprinter.Render.Text

import System.Exit

import TeenyTT.Base.Diagnostic (Diagnostic(..))
import TeenyTT.Base.Diagnostic qualified as Diagnostic
import TeenyTT.Base.Ident
import TeenyTT.Base.Location qualified as Loc
import TeenyTT.Base.SymbolTable (SymbolTable)
import TeenyTT.Base.SymbolTable qualified as Tbl

import TeenyTT.Core.Domain qualified as D

import TeenyTT.Elaborator.Monad

--------------------------------------------------------------------------------
-- [NOTE: Exception Safety + Global State]
-- We have two options here for our state: 'StateT' or 'ReaderT' + 'IORef'.
-- We prefer to use the latter as it hass better semantics when it comes to catching exceptions,
-- which is our preferred mechanism for throwing errors.
--
-- For various reasons, 'ExceptT' is a non-starter. It doesn't compose well, and requires
-- either a mega-error type, or a bunch of nested error types, neither of which is particularly
-- ergonomic. Furthermore, it's rather slow, and requires wrapping/unwrapping eithers /constantly/.

newtype Driver a = Driver { unDriver :: ReaderT DriverState IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader DriverState, MonadIO, PrimMonad)


runDriver :: FilePath -> ByteString -> Driver a -> IO a
runDriver path bytes (Driver m) = do
    st <- initState path bytes
    catch (runReaderT m st) handler
    where
      -- [TODO: Reed M, 04/06/2022] This should probably be handled inside of the elaborator...
      handler :: Diagnostic -> IO a
      handler diag = do
          putDoc (Diagnostic.render bytes diag)
          exitWith (ExitFailure 1)

--------------------------------------------------------------------------------
-- Driver State

data DriverState = DriverState
    { debugMode :: IORef Bool
    , filename :: IORef FilePath
    , buffer :: IORef ByteString
    , globals :: SymbolTable RealWorld Ident (D.Term, D.Type)
    , annotations :: SymbolTable RealWorld Ident D.Type
    }


initState :: FilePath -> ByteString -> IO DriverState
initState path bytes = do
    debugMode <- newIORef False
    filename <- newIORef path
    buffer <- newIORef bytes
    globals <- Tbl.new 128
    annotations <- Tbl.new 128
    pure $ DriverState {..}

clone :: DriverState -> Driver DriverState
clone st = do
    debugMode <- liftIO $ newIORef =<< readIORef st.debugMode
    filename <- liftIO $ newIORef =<< readIORef st.filename
    buffer <- liftIO $ newIORef =<< readIORef st.buffer
    globals <- Tbl.clone st.globals
    annotations <- Tbl.clone st.annotations
    pure $ DriverState {..}

-- | 'sandbox' takes a 'Driver' action, and clones it's state.
-- This is particularly useful for benchmarking.
sandbox :: Driver a -> Driver (() -> IO a)
sandbox (Driver m) = do
    st <- ask
    cloned <- clone st
    pure $ \_ -> runReaderT m st

--------------------------------------------------------------------------------
-- Elaboration

elab :: ElabM a -> Driver a
elab m = do
    st <- ask
    path <- liftIO $ readIORef st.filename
    bytes <- liftIO $ readIORef st.buffer
    liftIO $ runElabM st.globals bytes (Loc.spanStart path) m

annotateTp :: Ident -> D.Type -> Driver ()
annotateTp ident tp = do
    st <- ask
    -- [TODO: Reed M, 04/06/2022] Should probably warn if I shadow something.
    liftIO $ Tbl.push ident tp st.annotations

getAnnotation :: Ident -> Driver (Maybe D.Type)
getAnnotation ident = do
    st <- ask
    liftIO $ Tbl.lookup ident st.annotations

define :: Ident -> D.Term -> D.Type -> Driver ()
define ident ~tm tp = do
    st <- ask
    liftIO $ Tbl.push ident (tm, tp) st.globals

--------------------------------------------------------------------------------
-- Debugging

{-# INLINE setDebugMode #-}
setDebugMode :: Bool -> Driver ()
setDebugMode b = do
    st <- ask
    liftIO $ writeIORef st.debugMode b

{-# INLINE getDebugMode #-}
getDebugMode :: Driver Bool
getDebugMode = do
    st <- ask
    liftIO $ readIORef st.debugMode
