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
import TeenyTT.Core.Position

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

instance Pretty Ident where
    pretty (User txt) = pretty txt
    pretty Anon       = "_"

instance Debug Ident where
    dump _ (User txt) = pretty txt
    dump _ Anon       = "_"

instance Debug a => Debug (Cell a) where
    dump prec Cell{..} = parensIf (prec > NoPrec) (pretty ident <+> ":" <+> dump AnnPrec contents)

instance (Located a) => Located (Cell a) where
    locate (Cell _ a) = locate a
