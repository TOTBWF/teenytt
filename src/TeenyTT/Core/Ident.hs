-- | Identifiers, along with DeBruijin Indexes + Levels
module TeenyTT.Core.Ident
  ( Ident(..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.String

data Ident
    = Anon
    | User Text
    deriving (Eq, Show)

instance IsString Ident where
    fromString = User . T.pack
