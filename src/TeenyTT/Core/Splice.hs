-- | Splicing of Values.
module TeenyTT.Core.Splice
  ( Splice
  , val
  , tp
  , term
  , compile
  ) where

import Control.Monad.Primitive

import TeenyTT.Base.Env qualified as Env

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.TermBuilder (TB)
import TeenyTT.Core.TermBuilder qualified as TB

-- | A 'Splice' gives us a means of building terms
-- in the semantic domain, while avoiding having to do
-- lots of DeBruijin Arithmetic.
--
-- We do so by building terms that contain only variables,
-- and then tracking all the terms/types we've "spliced"
-- in an environment.
newtype Splice m a = Splice { unSplice :: D.MutableEnv (PrimState m) -> m (TB a) }

-- | Splice a 'D.Term'.
val :: (PrimMonad m) => D.Term -> (TB S.Term -> Splice m a) -> Splice m a
val v k = Splice \env -> do
    Env.push v env.values
    var <- TB.var <$> Env.sizeM env.values
    (k var).unSplice env

-- | Splice a 'D.Type'.
tp :: (PrimMonad m) => D.Type -> (TB S.Type -> Splice m a) -> Splice m a
tp vtp k = Splice \env -> do
    Env.push vtp env.types
    tpvar <- TB.tpvar <$> Env.sizeM env.values
    (k tpvar).unSplice env

term :: (PrimMonad m) => TB a -> Splice m a
term tb = Splice \_ -> pure tb

compile :: (PrimMonad m) => Splice m a -> m (D.MutableEnv (PrimState m), a)
compile (Splice sp) = do
    values <- Env.new 32
    types <- Env.new 32
    let env = D.MutableEnv {values, types}
    tb <- sp env
    nvalues <- Env.sizeM values
    ntypes <- Env.sizeM types
    pure (env, TB.runTB nvalues ntypes tb)
