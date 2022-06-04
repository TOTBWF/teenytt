-- | Helpers for constructing terms that involve DeBruijin Arithmetic.
module TeenyTT.Core.TermBuilder
  ( TB
  , runTB
  , var
  , tpvar
  -- * Term Builders
  -- ** Pi Types
  , pi
  , lam
  , ap
  , aps
  -- ** Sigma Types
  , sigma
  , pair
  , fst
  , snd
  -- ** Nats
  , nat
  , zero
  , suc
  -- ** Universes
  , univ
  , el
  ) where

import Prelude hiding (pi, fst, snd)

import Control.Monad.Reader

import Data.List (foldl')

import TeenyTT.Base.Ident

import TeenyTT.Core.Syntax qualified as S

newtype TB a = TB { unTB :: Reader TBEnv a }
    deriving newtype (Functor, Applicative, Monad, MonadReader TBEnv)

-- | A Term Builder 'Env' keeps track of the /number/ of types
-- and terms that have been bound. This allows us to convert
-- between DeBruijin indicies and levels. See 'var' and 'tpvar'.
data TBEnv = TBEnv { types :: Int, values :: Int }

runTB :: Int -> Int -> TB a -> a
runTB values types (TB m) =
    let tbenv = TBEnv { values, types }
    in runReader m tbenv

--------------------------------------------------------------------------------
-- Level Arithmetiic

var :: Int -> TB S.Term
var lvl = do
    size <- asks (\env -> env.values)
    pure $ S.Local (size - lvl - 1)

tpvar :: Int -> TB S.Type
tpvar lvl = do
    size <- asks (\env -> env.types)
    pure $ S.TpVar (size - lvl - 1)

extend :: (Int -> TB a) -> TB a
extend k = do
    size <- asks (\env -> env.values)
    local (\env -> env { values = env.values + 1 }) (k size)

extendTp :: (Int -> TB a) -> TB a
extendTp k = do
    size <- asks (\env -> env.types)
    local (\env -> env { values = env.values + 1 }) (k size)

scope :: (TB S.Term -> TB a) -> TB a
scope k = extend (k . var)

--------------------------------------------------------------------------------
-- Term Builders

-- Pi Types

pi :: Ident -> TB S.Type -> (TB S.Term -> TB S.Type) -> TB S.Type
pi x tbase tfam = S.Pi x <$> tbase <*> scope tfam

lam :: Ident -> (TB S.Term -> TB S.Term) -> TB S.Term
lam x k = S.Lam x <$> scope k

ap :: TB S.Term -> TB S.Term -> TB S.Term
ap tfn targ = S.Ap <$> tfn <*> targ

aps :: TB S.Term -> [TB S.Term] -> TB S.Term
aps tfn targs = foldl' ap tfn targs

-- Sigma Types

sigma :: Ident -> TB S.Type -> (TB S.Term -> TB S.Type) -> TB S.Type
sigma x tbase tfam = S.Sigma x <$> tbase <*> scope tfam

pair :: TB S.Term -> TB S.Term -> TB S.Term
pair tl tr = S.Pair <$> tl <*> tr

fst :: TB S.Term -> TB S.Term
fst tm = S.Fst <$> tm

snd :: TB S.Term -> TB S.Term
snd tm = S.Snd <$> tm

-- Nats

nat :: TB S.Type
nat = pure S.Nat

zero :: TB S.Term
zero = pure S.Zero

suc :: TB S.Term -> TB S.Term
suc tm = S.Suc <$> tm

-- Universes

univ :: TB S.Type
univ = pure S.Univ

el :: TB S.Term -> TB S.Type
el tm = S.El <$> tm
