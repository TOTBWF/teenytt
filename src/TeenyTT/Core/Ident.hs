-- | Identifiers, along with DeBruijin Indexes + Levels
module TeenyTT.Core.Ident
  ( Ident(..)
  , Cell(..)
  ) where

import GHC.Generics

import Control.DeepSeq
import Data.Text (Text)
import Data.Text qualified as T
import Data.String

import TeenyTT.Core.Pretty

data Ident
    = Anon
    | User Text
    deriving (Show, Eq, Ord, Generic)

instance NFData Ident

data Cell a = Cell { ident :: Ident, contents :: a }
    deriving (Show, Generic)

instance (NFData a) => NFData (Cell a)

instance IsString Ident where
    fromString = User . T.pack

instance Debug Ident where
    dump (User txt) = pretty txt
    dump Anon       = "_"

instance Debug a => Debug (Cell a) where
    dump Cell{..} = dump ident <+> ":" <+> dump contents
