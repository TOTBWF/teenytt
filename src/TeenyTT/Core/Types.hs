-- | The core data types for @teenytt@.
-- [TODO: Reed M, 02/06/2022] Explore the possibilty of using backpack here.
module TeenyTT.Core.Types (
  -- * Syntax
    Syntax(..)
  , SyntaxType(..)
  -- * Domain
  , Value(..)
  , ValueType(..)
  , Head(..)
  , Frame(..)
  , Clo(..)
  ) where

import Data.Text (Text)

import TeenyTT.Base.Ident
import TeenyTT.Base.Env (Env)

--------------------------------------------------------------------------------
-- Syntax

data Syntax
    = Local Int
    | Global Text ~Value
    | Let Syntax Ident Syntax
    | Lam Ident Syntax
    | Ap Syntax Syntax
    | Hole
    | Pair Syntax Syntax
    | Fst Syntax
    | Snd Syntax
    | Zero
    | Suc Syntax
    | NatElim Syntax Syntax Syntax Syntax
    | CodePi Ident Syntax Syntax
    | CodeSigma Ident Syntax Syntax
    | CodeUniv
    | CodeNat
    deriving stock (Show)

data SyntaxType
    = Pi Ident SyntaxType SyntaxType
    | Sigma Ident SyntaxType SyntaxType
    | El Syntax
    | Nat
    | Univ
    deriving stock (Show)

--------------------------------------------------------------------------------
-- Domain

data Value
    = VNeu Head [Frame]
    | VLam Ident (Clo Syntax)
    | VPair Value Value
    | VZero
    | VSuc Value
    | VCodePi Ident Value (Clo Syntax)
    | VCodeSigma Ident Value (Clo Syntax)
    | VCodeUniv
    | VCodeNat
    deriving stock (Show)

data ValueType
    = VElNeu Head [Frame]
    | VPi Ident ValueType (Clo SyntaxType)
    | VSigma Ident ValueType (Clo SyntaxType)
    | VNat
    | VUniv

data Head
    = KLocal Int
    | KGlobal Text ~Value
    | KHole
    deriving stock (Show)

data Frame
    = KAp Value
    | KFst
    | KSnd
    | KNatElim Value Value Value
    deriving stock (Show)

--------------------------------------------------------------------------------
-- Closures

data Clo a = Clo (Env Value) a
    deriving stock (Show)
