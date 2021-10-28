module TeenyTT.Core.Domain
  ( Type(..)
  , Spine(..)
  , Clo(..)
  , Value(..)
  , var
  ) where

import TeenyTT.Core.Ident
import TeenyTT.Core.Env

import TeenyTT.Core.Syntax qualified as S


-- | A @Clo@ represents some environment, along with a piece of the syntax (IE: an 'S.Term' or 'S.Type')
-- that binds an additional variable.
data Clo a = Clo (Env Value) a
    deriving (Show)

data Spine = Nil | App Spine ~Value
    deriving (Show)

data Value
    = Lam Ident (Clo S.Term)
    | Zero
    | Suc Value
    | Local Level Spine
    | Global Level Spine ~(Maybe Value)
    deriving (Show)

data Type
    = Univ
    | Nat
    | Pi Ident Type (Clo S.Type)
    deriving (Show)

var :: Level -> Value
var lvl = Local lvl Nil
