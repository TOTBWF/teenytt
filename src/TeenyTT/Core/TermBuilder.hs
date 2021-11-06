module TeenyTT.Core.TermBuilder
  ( TB
  , runTB
  -- * Variables
  , lvl
  , tplvl
  , scope
  , var
 , tpvar
 , clo
 , tpclo
  -- * Terms
  , pi
  , lam
  , app
  , small
  ) where

import Prelude hiding (pi)
import Data.List (foldl')

import Control.Monad.Reader

import TeenyTT.Core.Ident

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

import TeenyTT.Core.Env (Index, Level)
import TeenyTT.Core.Env qualified as Env

newtype TB a = TB { unTB :: Reader Env a }
    deriving (Functor, Applicative, Monad, MonadReader Env)

-- | A Term Builder 'Env' keeps track of the /number/ of types
-- and terms that have been bound. This allows us to convert
-- between DeBruijin indicies and levels. See 'var' and 'tpvar'.
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

-- | Convert a DeBruijin 'Level' into DeBruijin indexed variable.
var :: Level -> TB S.Term
var l = do
    n <- asks terms
    pure $ S.Local (Env.unsafeIndex (n - Env.unLevel l - 1))

-- | Convert a DeBruijin 'Level' into DeBruijin indexed type variable.
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

-- | @subst cloenv env tm@ will construct a 'S.Subst'
-- for a given closure environment @cloenv@,
-- relative to some existing environment @env@.
--
-- To do so, all the variables bound in @cloenv@
-- will be substituted by variables generated via
-- 'var', and the additional variable is substituted
-- for @tm@.
subst :: D.Env -> D.Env -> S.Term -> TB S.Subst
subst cloenv env tm = go vals
  where
    go 0 = pure $ S.Extend S.Id tm
    go n = S.Extend <$> go (n - 1) <*> var (Env.unsafeLevel (allvals - n))

    clovals = Env.size $ D.vals cloenv
    vals    = Env.size $ D.vals env
    allvals = clovals + vals

clo :: D.Clo S.Term -> D.Env -> TB S.Term -> TB S.Term
clo (D.Clo cloenv tm) env ta = do
    a <- ta
    sub <- subst cloenv env a
    pure $ S.Subst sub tm

tpclo :: D.Clo S.Type -> D.Env -> TB S.Term -> TB S.Type
tpclo (D.Clo cloenv tp) env ta = do
    a <- ta
    sub <- subst cloenv env a
    pure $ S.TpSubst sub tp

--------------------------------------------------------------------------------
-- Term Builders

pi :: Ident -> TB S.Type -> (TB S.Term -> TB S.Type) -> TB S.Type
pi ident tbase tfam = S.Pi ident <$> tbase <*> scope tfam

lam :: Ident -> (TB S.Term -> TB S.Term) -> TB S.Term
lam ident k = S.Lam ident <$> scope k

app :: TB S.Term -> [TB S.Term] -> TB S.Term
app f args = foldl' (\fn arg -> S.App <$> fn <*> arg) f args

small :: TB S.Type -> TB S.Type -> TB S.Type
small ta tuniv = S.Small <$> ta <*> tuniv
