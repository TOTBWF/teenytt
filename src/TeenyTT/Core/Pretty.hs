module TeenyTT.Core.Pretty
  ( module Pp
  , Precedence(..)
  , parensIf
  , Debug(..)
  , Display(..)
  -- * IO
  , putDocLn
  ) where

import Control.Monad.IO.Class

import Prettyprinter as Pp
import Prettyprinter.Render.Text as Render

import TeenyTT.Core.Position

--------------------------------------------------------------------------------
-- Precedence

data Precedence
    = NoPrec
    -- ^ No precedence.
    | AnnPrec
    -- ^ The precedence of a type annotation.
    | AppPrec
    -- ^ The precedence of a function application.
    deriving stock (Show, Eq, Ord)

-- | Optionally wrap a document in parens.
parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  d = parens d
parensIf False d = d

--------------------------------------------------------------------------------
-- Debugging

-- | 'Debug' lives somewhere between 'Show' and 'Display'.
-- We often want to display some term, but we may not have
-- the context for providing good names.
class Debug a where
    dump :: Precedence -> a -> Doc ann

instance Debug a => Debug (Loc a) where
    dump prec (Loc _ a) = dump prec a

--------------------------------------------------------------------------------
-- Display

-- | 'Display' is intended for user facing display of terms.
class Display a where
    display :: Precedence -> a -> Doc ann

--------------------------------------------------------------------------------
-- IO

putDocLn :: (MonadIO m) => Doc ann -> m ()
putDocLn doc = liftIO $ do
    putDoc doc
    putStrLn ""
