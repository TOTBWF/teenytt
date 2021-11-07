module TeenyTT.Core.Pretty
  ( module Pp
  , Debug(..)
  , Display(..)
  -- * IO
  , putDocLn
  ) where

import Control.Monad.IO.Class

import Prettyprinter as Pp
import Prettyprinter.Render.Text as Render

-- | 'Debug' lives somewhere between 'Show' and 'Display'.
-- We often want to display some term, but we may not have
-- the context for providing good names.
class Debug a where
    dump :: a -> Doc ann

instance (Debug a, Debug b) => Debug (a, b) where
    dump (a, b) = parens (dump a <> comma <+> dump b)

-- [FIXME: Reed M, 06/11/2021] Fill in this class
class Display a where
    display :: a -> Doc ann

--------------------------------------------------------------------------------
-- IO

putDocLn :: (MonadIO m) => Doc ann -> m ()
putDocLn doc = liftIO $ do
    putDoc doc
    putStrLn ""
