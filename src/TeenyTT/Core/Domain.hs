{-# LANGUAGE PatternSynonyms #-}
-- | Re-Exports of domain types, intended to be used with a qualified import.
module TeenyTT.Core.Domain
  ( Term
  , Type
  , module Types
  -- * Pattern Synonyms
  , pattern Local
  , pattern Global
  , pattern Hole
  -- * Neutrals
  , pushFrame
  -- * Environments
  , thaw
  , freeze
  ) where

import Control.Monad.Primitive

import TeenyTT.Base.Env qualified as Env
import TeenyTT.Base.Ident

import TeenyTT.Core.Types as Types
    ( Value(..)
    , ValueType(..)
    , Neu(..)
    , Head(..)
    , Frame(..)
    , Env(..)
    , MutableEnv(..)
    , Clo(..)
    )


type Term = Value
type Type = ValueType

pattern Local :: Int -> [Frame] -> Value
pattern Local lvl spine = VNeu (Neu (KLocal lvl) spine)

pattern Global :: Ident -> Value -> [Frame] -> Value
pattern Global name val spine = VNeu (Neu (KGlobal name val) spine)

pattern Hole :: [Frame] -> Value
pattern Hole spine = VNeu (Neu KHole spine)

pushFrame :: Neu -> Frame -> (Value -> Value) -> Neu
pushFrame neu frm unf = case neu.hd of
  KGlobal name ~v ->
      neu { hd = KGlobal name (unf v), spine = frm : neu.spine }
  _ ->
      neu { spine = frm : neu.spine }

thaw :: (PrimMonad m) => Env -> m (MutableEnv (PrimState m))
thaw env = MutableEnv <$> Env.thaw env.values <*> Env.thaw env.types

freeze :: (PrimMonad m) => MutableEnv (PrimState m) -> m Env
freeze env = Env <$> Env.freeze env.values <*> Env.freeze env.types
