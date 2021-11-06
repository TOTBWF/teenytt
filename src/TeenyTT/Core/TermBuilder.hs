module TeenyTT.Core.TermBuilder
  ( TB
  , runTB
  -- * Variables
  , lvl
  , tplvl
  , scope
  , var
 , tpvar
  -- * Terms
  , pi
  , small
  ) where

import Prelude hiding (pi)

import Control.Monad.Reader

import TeenyTT.Core.Ident

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Env (Index, Level)
import TeenyTT.Core.Env qualified as Env

newtype TB a = TB { unTB :: Reader Env a }
    deriving (Functor, Applicative, Monad, MonadReader Env)

data Env = Env { types :: Int, terms :: Int }

runTB :: D.Env -> TB a -> a
runTB env (TB tb) =
    let tbenv = Env { types = Env.size (D.tps env), terms = Env.size (D.vals env) }
    in runReader tb tbenv

--------------------------------------------------------------------------------
-- Level Arithmetic

lvl :: D.Env -> Level
lvl env = Env.last $ D.vals env

tplvl :: D.Env -> Level
tplvl env = Env.last $ D.tps env

var :: Level -> TB S.Term
var l = do
    n <- asks terms
    pure $ S.Local (Env.unsafeIndex (n - Env.unLevel l - 1))

tpvar :: Level -> TB S.Type
tpvar l = do
    n <- asks types
    pure $ S.TpVar (Env.unsafeIndex (n - Env.unLevel l - 1))

bindVal :: (Level -> TB a) -> TB a
bindVal k = do
    l <- asks (Env.unsafeLevel . terms)
    local (\env -> env { terms = terms env + 1 }) (k l)

bindTp :: (Level -> TB a) -> TB a
bindTp k = do
    l <- asks (Env.unsafeLevel . types)
    local (\env -> env { types = types env + 1 }) (k l)

scope :: (TB S.Term -> TB a) -> TB a
scope k = bindVal (k . var)

--------------------------------------------------------------------------------
-- Term Builders

pi :: Ident -> TB S.Type -> (TB S.Term -> TB S.Type) -> TB S.Type
pi ident tbase tfam = S.Pi ident <$> tbase <*> scope tfam

small :: TB S.Type -> TB S.Type -> TB S.Type
small ta tuniv = S.Small <$> ta <*> tuniv
