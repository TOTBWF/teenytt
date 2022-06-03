{-# LANGUAGE DataKinds #-}

-- | Source Spans and Source Locations.
module TeenyTT.Base.Location
  (
  -- * Positions
    Pos(..)
  , posStart
  , nextCol
  , nextLine
  -- * Spans
  , Span(..)
  , spanStart
  -- * Located Data
  , Loc(..)
  , unlocate
  , Located(..)
  , locations
  -- * Conversions
  , startPos
  , stopPos
  , spanning
  -- * Slicing
  , sliceLine
  ) where

import GHC.Generics
import GHC.Records (HasField(..))

import Control.DeepSeq

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))

import TeenyTT.Base.ByteString (ByteString)
import TeenyTT.Base.Pretty
import qualified TeenyTT.Base.ByteString as BS


--------------------------------------------------------------------------------
-- Positions

data Pos = Pos
    { pos :: Int
    -- ^ The absolute position in the file, measured in graphemes.
    , bol :: Int
    -- ^ The absolute position of the beginning of the line, measured in graphemes.
    , lineNum :: Int
    -- ^ The line number of the position.
    , filename :: FilePath
    -- ^ The absolute filepath of the file the position originated from.
    }
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | The column number of the start of a span.
instance HasField "col" Pos Int where
    getField pos = pos.pos - pos.bol

-- | The position at the start of a file.
posStart :: FilePath -> Pos
posStart path =
    Pos 0 0 1 path

-- | Advance a position to the next grapheme.
nextCol :: Pos -> Pos
nextCol p = p { pos = p.pos + 1 }

-- | Advance a position to the start of the next line.
nextLine :: Pos -> Pos
nextLine p = p { pos = p.pos + 1, bol = p.pos + 1, lineNum = p.lineNum + 1 }

--------------------------------------------------------------------------------
-- Spans

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
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

-- | The column number of the start of a span.
instance HasField "startCol" Span Int where
    getField sp = sp.start - sp.startBol

-- | The column number of the end of a span.
instance HasField "stopCol" Span Int where
    getField sp = sp.stop - sp.stopBol

-- | The width of a span.
instance HasField "width" Span Int where
    getField sp = sp.stop - sp.start

instance Semigroup Span where
    sp0 <> sp1 =
        Span { start = min sp0.start sp1.start
             , startBol = min sp0.startBol sp1.startBol
             , startLine = min sp0.startLine sp1.startLine
             , stop = max sp0.stop sp1.stop
             , stopBol = max sp0.stopBol sp1.stopBol
             , stopLine = max sp0.stopLine sp1.stopLine
             , filename = sp0.filename
             }

-- | The empty span at the start of a file
spanStart :: FilePath -> Span
spanStart path = Span 0 0 1 0 0 1 path

--------------------------------------------------------------------------------
-- Locations

-- | Some data, along with a span.
data Loc a = Loc Span a
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance Pretty a => Pretty (Loc a) where
    pretty (Loc _ a) = pretty a

instance Display a => Display (Loc a) where
    classify (Loc _ a) = classify a
    display' env (Loc _ a) = display' env a

-- | Drop the span from some located data.
unlocate :: Loc a -> a
unlocate (Loc _ a) = a

-- | We occasionaly have data that isn't located via 'Loc',
-- so it makes sense to create a typeclass for getting the locations.
class Located a where
    locate :: a -> Span

instance Located (Loc a) where
    locate (Loc sp _) = sp

locations :: (Located a) => NonEmpty a -> Span
locations (x :| xs) = foldl' (\sp a -> sp <> locate a) (locate x) xs

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

spanning :: Pos -> Pos -> Span
spanning start stop =
    Span { start = start.pos
         , startBol = start.bol
         , startLine = start.lineNum
         , stop = stop.pos
         , stopBol = stop.bol
         , stopLine = stop.lineNum
         , filename = start.filename
         }

--------------------------------------------------------------------------------
-- Slicing

sliceLine :: Span -> ByteString -> ByteString
sliceLine sp bs = BS.sliceLine sp.startBol bs

