{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
-- | The Elaborator Monad.
module TeenyTT.Elaborator.Monad
  ( ElabM
  , runElabM
  -- * Errors
  , expectedConnective
  , unboundVariable
  , outOfBoundsLiteral
  , malformedPattern
  , cannotEliminate
  , cannotSynth
  , notAType
  , notImplemented
  -- * Environments
  , abstract
  , Resolved(..)
  , resolve
  , getLocal
  , getGlobal
  -- * NbE Wrappers
  , eval 
  , evalTp
  , quote
  , quoteTp
  , equate
  , equateTp
  -- * Eliminators
  , NbE.doEl
  -- * Closures
  , NbE.instTmClo
  , NbE.instTpClo
  -- * Splicing
  , NbE.spliceTm
  , NbE.spliceTp
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Reader

import Data.Functor

import TeenyTT.Base.Diagnostic
import TeenyTT.Base.Env (MutableEnv)
import TeenyTT.Base.Env qualified as Env
import TeenyTT.Base.Ident
import TeenyTT.Base.Location
import TeenyTT.Base.Pretty (Doc, Display, DisplayEnv, (<+>))
import TeenyTT.Base.Pretty qualified as Pp
import TeenyTT.Base.SymbolTable (SymbolTable)
import TeenyTT.Base.SymbolTable qualified as Tbl

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.NbE qualified as NbE

import TeenyTT.Elaborator.ConcreteSyntax qualified as CS

--------------------------------------------------------------------------------
-- Elaboration Monad

newtype ElabM a = ElabM { unElabM :: ReaderT (ElabEnv RealWorld) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, PrimMonad)

data ElabEnv s
    = ElabEnv
    { locals     :: MutableEnv s D.Term
    -- ^ We store the local bindings separately from their
    -- types so that we can evaluate easier.
    , localTps   :: SymbolTable s Ident D.Type
    , globals    :: SymbolTable s Ident (D.Term, D.Type) 
    , location   :: Span
    , displayEnv :: DisplayEnv RealWorld
    }

runElabM :: SymbolTable RealWorld Ident (D.Term, D.Type) -> Span -> ElabM a -> IO a
runElabM globals location m = do
    locals <- Env.new 128
    localTps <- Tbl.new 128
    displayEnv <- Pp.initEnv
    runReaderT m.unElabM (ElabEnv {locals, localTps, globals, location, displayEnv})

envSize :: ElabM Int
envSize = ElabM do
    env <- ask
    Env.sizeM env.locals

currentSpan :: ElabM Span
currentSpan = ElabM $ asks (\env -> env.location)

--------------------------------------------------------------------------------
-- Environments

abstract :: Ident -> D.Type -> (D.Term -> ElabM a) -> ElabM a
abstract x tp k = ElabM do
    env <- ask
    lvl <- Env.sizeM env.locals
    let v = D.Local lvl []
    Env.push v env.locals
    Tbl.push x tp env.localTps
    a <- (k v).unElabM
    Env.pop_ env.locals
    Tbl.pop_ env.localTps
    pure a

data Resolved
    = Local Int
    | Global Int
    | Unbound

resolve :: Ident -> ElabM Resolved
resolve name = ElabM do
    env <- ask
    Tbl.indexOf name env.localTps >>= \case
      Just ix -> pure $ Local ix
      Nothing -> Tbl.levelOf name env.globals <&> \case
          Just lvl -> Global lvl
          Nothing -> Unbound


getLocal :: Int -> ElabM (D.Term, D.Type)
getLocal ix = ElabM do
    env <- ask
    v <- Env.index ix env.locals
    vtp <- Tbl.index ix env.localTps
    pure (v, vtp)

getGlobal :: Int -> ElabM (D.Term, D.Type)
getGlobal ix = ElabM do
    env <- ask
    Tbl.level ix env.globals

--------------------------------------------------------------------------------
-- Errors

fatal :: Diagnostic -> ElabM a
fatal diag = liftIO $ throwIO diag

display :: (Display a) => a -> ElabM (Doc ())
display a = ElabM do
    displayEnv <- asks (\env -> env.displayEnv)
    Pp.display displayEnv a

tmConvError :: D.Term -> D.Term -> ElabM a
tmConvError v0 v1 = do
      location <- currentSpan
      tm0 <- quote v0
      tm1 <- quote v1
      ptm0 <- display tm0
      ptm1 <- display tm1
      let snippet = Snippet { location, message = "Couldn't equate terms:" <+> ptm0 <+> "and" <+>  ptm1 }
      fatal $ Diagnostic { severity = Error, code = ConversionError, snippets = [snippet] }

tpConvError :: D.Type -> D.Type -> ElabM a
tpConvError v0 v1 = do
      location <- currentSpan
      tm0 <- quoteTp v0
      tm1 <- quoteTp v1
      ptm0 <- display tm0
      ptm1 <- display tm1
      let snippet = Snippet { location, message = "Couldn't equate types:" <+> ptm0 <+> "and" <+>  ptm1 }
      fatal $ Diagnostic { severity = Error, code = ConversionError, snippets = [snippet] }

expectedConnective :: Doc () -> D.Type -> ElabM a
expectedConnective conn vtp = do
    location <- currentSpan
    tp <- quoteTp vtp
    ptp <- display tp
    let snippet = Snippet { location, message = "Expected connective" <+> conn <+> "but got" <+> ptp }
    fatal $ Diagnostic { severity = Error, code = ExpectedConnective, snippets = [snippet] }

unboundVariable :: Ident -> ElabM a
unboundVariable name = do
    location <- currentSpan
    let snippet = Snippet { location, message = "Variable" <+> Pp.pretty name <+> "is unbound." }
    fatal $ Diagnostic { severity = Error, code = UnboundVariable, snippets = [snippet] }

outOfBoundsLiteral :: Integer -> D.Type -> ElabM a
outOfBoundsLiteral lit vtp = do
    location <- currentSpan
    tp <- quoteTp vtp
    ptp <- display tp
    let snippet = Snippet { location, message = "Literal" <+> Pp.pretty lit <+> "is out of bounds for type" <+> ptp }
    fatal $ Diagnostic { severity = Error, code = UnboundVariable, snippets = [snippet] }

malformedPattern :: D.Type -> ElabM a
malformedPattern vtp = do
    location <- currentSpan
    tp <- quoteTp vtp
    ptp <- display tp
    let snippet = Snippet { location, message = "Malformed pattern for type"  <+> ptp }
    fatal $ Diagnostic { severity = Error, code = MalformedCase, snippets = [snippet] }

cannotEliminate :: D.Type -> ElabM a
cannotEliminate vtp = do
    location <- currentSpan
    tp <- quoteTp vtp
    ptp <- display tp
    let snippet = Snippet { location, message = "Cannot eliminate type"  <+> ptp }
    fatal $ Diagnostic { severity = Error, code = CannotEliminate, snippets = [snippet] }

cannotSynth :: CS.Term -> ElabM a
cannotSynth ctm = do
    location <- currentSpan
    ptm <- display ctm
    let snippet = Snippet { location, message = "Cannot synthesize term"  <+> ptm }
    fatal $ Diagnostic { severity = Error, code = CannotSynth, snippets = [snippet] }

notAType :: CS.Term -> ElabM a
notAType ctm = do
    location <- currentSpan
    ptm <- display ctm
    let snippet = Snippet { location, message = ptm <+> "is not a type" }
    fatal $ Diagnostic { severity = Error, code = NotAType, snippets = [snippet] }

notImplemented :: CS.Term -> ElabM a
notImplemented ctm = do
    location <- currentSpan
    ptm <- display ctm
    let snippet = Snippet { location, message = "Cannot elaborate term" <+> ptm }
    fatal $ Diagnostic { severity = Panic, code = NotImplemented, snippets = [snippet] }

--------------------------------------------------------------------------------
-- NbE Wrappers

eval :: S.Term -> ElabM D.Term
eval tm = ElabM do
    locals <- Env.freeze =<< asks (\env -> env.locals)
    pure $ NbE.eval locals tm

evalTp :: S.Type -> ElabM D.Type
evalTp tp = ElabM do
    locals <- Env.freeze =<< asks (\env -> env.locals)
    pure $ NbE.evalTp locals tp

quote :: D.Term -> ElabM S.Term
quote v = do
    size <- envSize
    pure $ NbE.quote size v

quoteTp :: D.Type -> ElabM S.Type
quoteTp vtp = do
    size <- envSize
    pure $ NbE.quoteTp size vtp

equate :: D.Term -> D.Term -> ElabM ()
equate tm0 tm1 = do
    size <- envSize
    if NbE.equate size tm0 tm1 then
      pure ()
    else
      tmConvError tm0 tm1

equateTp :: D.Type -> D.Type -> ElabM ()
equateTp tp0 tp1 = do
    size <- envSize
    if NbE.equateTp size tp0 tp1 then
      pure ()
    else
      tpConvError tp0 tp1
