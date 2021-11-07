module TeenyTT.Frontend.Position
  ( Position(..)
  , fileStart
  ) where

import GHC.Generics

import Control.DeepSeq

data Position = Position
    { posFile :: FilePath
    , posLine :: Int
    , posCol  :: Int
    } deriving (Show, Eq, Ord, Generic)

instance NFData Position

fileStart :: FilePath -> Position
fileStart path =
    Position { posFile = path
             , posLine = 1
             , posCol  = 1
             }
