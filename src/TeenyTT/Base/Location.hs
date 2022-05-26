{-# LANGUAGE DataKinds #-}

-- | Source Spans and Source Locations.
module TeenyTT.Base.Location
  ( Span(..)
  -- * Located Data
  , Loc(..)
  , unlocate
  , Located(..)
  -- * Conversions
  , startPos
  , stopPos
  -- * Slicing
  , slice
  ) where

import TeenyTT.Base.ByteString (ByteString)
import TeenyTT.Base.ByteString qualified as BS

import GHC.Records (HasField(..))

data Pos = Pos
    { pos :: Int
    -- ^ The absolute position in the file, measured in bytes.
    , bol :: Int
    -- ^ The absolute position of the beginning of the line, measured in bytes.
    , lineNum :: Int
    -- ^ The line number of the position.
    , filename :: FilePath
    -- ^ The absolute filepath of the file the position originated from.
    }

-- | A source span.
data Span = Span
    { start :: Int
    -- ^ The absolute position in the file for the start of the span,
    -- in bytes.
    , startBol :: Int
    -- ^ The absolute position of the begining of the line for the start
    -- position.
    , startLine :: Int
    -- ^ The line number of the start of the span.
    , stop :: Int
    -- ^ The absolute position in the file for the end of the span,
    -- in bytes.
    , stopBol :: Int
    -- ^ The absolute position of the begining of the line for the stop
    -- position.
    , stopLine :: Int
    -- ^ The line number of the start of the span.
    , filename :: FilePath
    -- ^ The absolute filepath of the file the span originated from.
    }

-- | The column number of the start of a span.
instance HasField "startCol" Span Int where
    getField sp = sp.start - sp.startBol

-- | The column number of the end of a span.
instance HasField "stopCol" Span Int where
    getField sp = sp.stop - sp.stopBol

--------------------------------------------------------------------------------
-- Locations

-- | Some data, along with a span.
data Loc a = Loc Span a

-- | Drop the span from some located data.
unlocate :: Loc a -> a
unlocate (Loc _ a) = a

-- | We occasionaly have data that isn't located via 'Loc',
-- so it makes sense to create a typeclass for getting the locations.
class Located a where
    locate :: a -> Span

instance Located (Loc a) where
    locate (Loc sp _) = sp

--------------------------------------------------------------------------------
-- Conversions

-- | Get the start position of a span.
startPos :: Span -> Pos
startPos sp =
    Pos { pos = sp.start
        , bol = sp.startBol
        , lineNum = sp.startLine
        , filename = sp.filename
        }

-- | Get the stop position of a span.
stopPos :: Span -> Pos
stopPos sp =
    Pos { pos = sp.stop
        , bol = sp.stopBol
        , lineNum = sp.stopLine
        , filename = sp.filename
        }

--------------------------------------------------------------------------------
-- Slicing

-- | Extract a span out of a 'ByteString'.
slice :: Span -> ByteString -> ByteString
slice sp bs = BS.slice sp.start sp.stop bs

