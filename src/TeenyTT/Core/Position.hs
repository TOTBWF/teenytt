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
    { startPos :: Position
    , endPos   :: Position
    } deriving (Show, Eq, Generic)

instance NFData Span

instance Semigroup Span where
    sp0 <> sp1 = Span 
      { startPos = min (startPos sp0) (startPos sp1)
      , endPos   = max (endPos sp0) (endPos sp1)
      }

-- | @big `contains` small@ determines if the
-- span @big@ completely contains @small@.
contains :: Span -> Span -> Bool
contains big small =
    (startPos big <= startPos small) && (endPos small <= endPos big)

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
