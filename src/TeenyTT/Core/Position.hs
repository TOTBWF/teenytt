module TeenyTT.Core.Position
  ( Position(..)
  , fileStart
  -- * Spans
  , Span(..)
  , contains
  -- * Location Annotations
  , Loc(..)
  , unlocate
  , Located(..)
  , locations
  ) where

import GHC.Generics
import Control.DeepSeq

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------
-- Positions

data Position = Pos
    { posLine :: Int
    , posCol  :: Int
    } deriving (Show, Eq, Ord, Generic)

instance NFData Position

fileStart :: Position
fileStart = Pos { posLine = 1, posCol  = 1 }

--------------------------------------------------------------------------------
-- Spans

data Span = Span
    { startLine :: Int
    , startCol  :: Int
    , endLine   :: Int
    , endCol    :: Int
    } deriving (Show, Eq, Generic)

instance NFData Span

instance Semigroup Span where
    sp0 <> sp1 = Span 
      { startLine =
        min (startLine sp0) (startLine sp1)
      , startCol  =
        case compare (startLine sp0) (startLine sp1) of
          LT -> startCol sp0
          GT -> startCol sp1
          EQ -> min (startCol sp0) (startCol sp1)
      , endLine   =
        max (endLine sp0) (endLine sp1)
      , endCol    =
        case compare (endLine sp0) (endLine sp1) of
          LT -> endCol sp1
          GT -> endCol sp0
          EQ -> max (endCol sp0) (endCol sp1)
      }

-- | @big `contains` small@ determines if the
-- span @big@ completely contains @small@.
contains :: Span -> Span -> Bool
contains big small =
    case (compare (startLine small) (startLine big), compare (endLine small) (endLine big)) of
      (LT, GT) -> True
      (GT, _)  -> False
      (_, LT)  -> False
      (EQ, GT) -> endCol small <= endCol big
      (LT, EQ) -> startCol big <= startCol small
      (EQ, EQ) -> (endCol small <= endCol big && startCol big <= startCol small)

--------------------------------------------------------------------------------
-- Location Annotations

data Loc a = Loc Span a
    deriving stock (Show, Generic)
    deriving anyclass NFData

unlocate :: Loc a -> a
unlocate (Loc _ a) = a

class Located a where
    locate :: a -> Span

instance Located Span where
    {-# INLINE locate #-}
    locate x = x

instance Located (Loc a) where
    {-# INLINE locate #-}
    locate (Loc sp _) = sp

locations :: (Located a) => NonEmpty a -> Span
locations (x :| xs) = foldl' (\sp y -> sp <> locate y) (locate x) xs
