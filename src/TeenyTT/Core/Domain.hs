module TeenyTT.Core.Domain
  ( Type(..)
  , Head(..)
  , Frame(..)
  , Neutral(..)
  , Clo(..)
  , Value(..)
  , Env(..)
  -- * Environments
  , bindVal
  , bindTp
  -- * Smart Constructors
  , var
  , global
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Level, Index)
import TeenyTT.Core.Env qualified as Env

import TeenyTT.Core.Syntax qualified as S

-- [FIXME: Reed M, 05/11/2021] I should add a monoid instance here
data Env = Env { tps :: Env.Env Type, vals :: Env.Env Value }
    deriving (Show)

instance Semigroup Env where
    env0 <> env1 = Env { tps = tps env0 <> tps env1, vals = vals env0 <> vals env1 }

instance Monoid Env where
    mempty = Env { tps = mempty, vals = mempty }

-- | A @Clo@ represents some environment, along with a piece of the syntax (IE: an 'S.Term' or 'S.Type')
-- that binds an additional variable.
data Clo a = Clo Env a
    deriving (Show)

data Value
    = Lam Ident (Clo S.Term)
    | Zero
    | Suc Value
    | Cut Neutral Type
    deriving (Show)

-- | A 'Neutral' value consists of some variable that evaluation is stuck on
-- along with a stack of eliminators that are blocked on that variable.
-- We call that variable the 'Head', and those eliminators 'Frame's.
data Neutral = Neutral { hd :: Head, frames :: [Frame] }
    deriving (Show)

-- | A 'Head' is some sort of variable that evaluation got stuck on.
data Head
    = Local Level
    | Global Level ~Value
    deriving (Show)

-- | A 'Frame' is some elimination form that is blocked on a 'Head'.
data Frame = App Type ~Value
    deriving (Show)

data Type
    = Univ
    | Nat
    | Pi Ident Type (Clo S.Type)
    deriving (Show)

var :: Level -> Type -> Value
var lvl tp = Cut (Neutral (Local lvl) []) tp

global :: Level -> Value -> Type -> Value
global lvl ~u tp = Cut (Neutral (Global lvl u) []) tp
