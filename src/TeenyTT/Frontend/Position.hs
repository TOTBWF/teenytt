-- |
module TeenyTT.Frontend.Position
  ( Position(..)
  , fileStart
  ) where

data Position = Position
    { posFile :: FilePath
    , posLine :: Int
    , posCol  :: Int
    } deriving (Show, Eq, Ord)

fileStart :: FilePath -> Position
fileStart path =
    Position { posFile = path
             , posLine = 1
             , posCol  = 1
             }
