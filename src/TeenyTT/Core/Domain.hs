-- | Re-Exports of domain types, intended to be used with a qualified import.
module TeenyTT.Core.Domain
  ( Term
  , Type
  , Value(..)
  , ValueType(..)
  , Head(..)
  , Frame(..)
  , Clo(..)
  -- * Smart Constructors
  , local
  , global
  , hole
  , pushFrame
  ) where

import Data.Text (Text)

import TeenyTT.Core.Types

type Term = Value
type Type = ValueType

local :: Int -> Value
local lvl = VNeu (KLocal lvl) []

global :: Text -> Value -> Value
global name ~val = VNeu (KGlobal name val) []

hole :: Value
hole = VNeu KHole []

pushFrame :: Head -> [Frame] -> Frame -> (Value -> Value) -> Value
pushFrame (KGlobal name ~v) spine frm unfold = VNeu (KGlobal name (unfold v)) (frm : spine)
pushFrame hd spine frm _ = VNeu hd (frm : spine)
