{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
-- | The Elaborator Monad.
module TeenyTT.Elaborator.Monad
  ( ElabM
  , runElabM
  -- * NbE Wrappers
  , eval 
  , evalTp
  , quote
  , quoteTp
  , equate
  , equateTp
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Reader

import TeenyTT.Base.Diagnostic
import TeenyTT.Base.Env (MutableEnv)
import TeenyTT.Base.Env qualified as Env
import TeenyTT.Base.Ident
import TeenyTT.Base.Location
import TeenyTT.Base.Pretty (Doc, Display, DisplayEnv, (<+>))
import TeenyTT.Base.Pretty qualified as Pp
import TeenyTT.Base.SymbolTable (SymbolTable)

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.NbE qualified as NbE

--------------------------------------------------------------------------------
-- Elaboration Monad

newtype ElabM a = ElabM { unElabM :: ReaderT (ElabEnv RealWorld) IO a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, PrimMonad)

data ElabEnv s
    = ElabEnv
    { locals     :: MutableEnv s D.Term
    , globals    :: SymbolTable s Ident (D.Type, D.Term) 
    , location   :: Span
    , displayEnv :: DisplayEnv RealWorld
    }

runElabM :: SymbolTable RealWorld Ident (D.Type, D.Term) -> Span -> ElabM a -> IO a
runElabM globals location m = do
    locals <- Env.new 128
    displayEnv <- Pp.initEnv
    runReaderT m.unElabM (ElabEnv {locals, globals, location, displayEnv})

envSize :: ElabM Int
envSize = ElabM do
    env <- ask
    Env.size env.locals

currentSpan :: ElabM Span
currentSpan = ElabM $ asks (\env -> env.location)

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
