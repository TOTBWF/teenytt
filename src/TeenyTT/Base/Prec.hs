{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- | Precedence Levels
module TeenyTT.Base.Prec where

import TeenyTT.Base.Pretty

passed = nonassoc 8
atom = nonassoc 7
delimited = nonassoc 6
juxtaposition = nonassoc 5
comma = nonassoc 3
colon = nonassoc 3
times = right 2
arrow = right 2
lambda = right 1
in_ = nonassoc 0
