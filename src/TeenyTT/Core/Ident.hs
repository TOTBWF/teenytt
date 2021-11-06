-- | Identifiers, along with DeBruijin Indexes + Levels
module TeenyTT.Core.Ident
  ( Ident(..)
  , Cell(..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.String

data Ident
    = Anon
    | User Text
    deriving (Eq, Show)

data Cell a = Cell { ident :: Ident, contents :: a }
    deriving Show

instance IsString Ident where
    fromString = User . T.pack
