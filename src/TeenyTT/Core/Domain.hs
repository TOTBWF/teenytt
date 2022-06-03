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
  ) where

import Data.Text (Text)

import TeenyTT.Core.Types as Types
    ( Value(..)
    , ValueType(..)
    , Neu(..)
    , Head(..)
    , Frame(..)
    , Clo(..)
    )


type Term = Value
type Type = ValueType

pattern Local :: Int -> [Frame] -> Value
pattern Local lvl spine = VNeu (Neu (KLocal lvl) spine)

pattern Global :: Text -> Value -> [Frame] -> Value
pattern Global name val spine = VNeu (Neu (KGlobal name val) spine)

pattern Hole :: [Frame] -> Value
pattern Hole spine = VNeu (Neu KHole spine)

pushFrame :: Neu -> Frame -> (Value -> Value) -> Neu
pushFrame neu frm unf = case neu.hd of
  KGlobal name ~v ->
      neu { hd = KGlobal name (unf v), spine = frm : neu.spine }
  _ ->
      neu { spine = frm : neu.spine }
