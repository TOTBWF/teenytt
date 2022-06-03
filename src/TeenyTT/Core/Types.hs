-- | The core data types for @teenytt@.
-- [TODO: Reed M, 02/06/2022] Explore the possibilty of using backpack here.
module TeenyTT.Core.Types (
  -- * Syntax
    Syntax(..)
  , SyntaxType(..)
  -- * Domain
  , Value(..)
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
    | Lit Integer
    | Zero
    | Suc Syntax
    | NatElim Syntax Syntax Syntax Syntax
    | CodePi Syntax Syntax
    | CodeSigma Syntax Syntax
    | CodeUniv
    | CodeNat
    deriving stock (Show)

data SyntaxType
    = TpVar Int
    | Pi SyntaxType Ident SyntaxType
    | Sigma SyntaxType Ident SyntaxType
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
    deriving stock (Show)

data ValueType
    = ElNeu Head [Frame]
    | VPi ValueType Ident (Clo SyntaxType)
    | VSigma ValueType Ident (Clo SyntaxType)
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
