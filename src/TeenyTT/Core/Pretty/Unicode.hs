module TeenyTT.Core.Pretty.Unicode
  ( arrow
  , compose
  , emptySet
  , forall
  , lambda
  , nat
  , turnstile
  ) where

import Prelude hiding (pi)

import TeenyTT.Core.Pretty

arrow :: Doc ann
arrow = pretty '→'

emptySet :: Doc ann
emptySet = pretty '∅'

forall :: Doc ann
forall = pretty '∀'

lambda :: Doc ann
lambda = pretty 'λ'

nat :: Doc ann
nat = pretty 'ℕ'

turnstile :: Doc ann
turnstile = pretty '⊢'

compose :: Doc ann
compose = pretty '∘'
