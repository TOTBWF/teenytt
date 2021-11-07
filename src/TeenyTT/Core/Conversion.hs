module TeenyTT.Core.Conversion
  ( ConvM
  , ConvEnv(..)
  , runConv
  , equate
  , equateTp
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Eval

import TeenyTT.Core.Ident

import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error(..))

import TeenyTT.Core.Compute

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S

newtype ConvM a = ConvM { unConvM :: ReaderT ConvEnv CmpM a }
    deriving (Functor, Applicative, Monad, MonadReader ConvEnv, MonadCmp)

runConv :: ConvEnv -> ConvM a -> CmpM a
runConv env (ConvM m) = runReaderT m env

--------------------------------------------------------------------------------
-- Conversion Environments

data ConvEnv = ConvEnv
    { conv_locals :: Int
    }

--------------------------------------------------------------------------------
-- Errors

--------------------------------------------------------------------------------
-- Variables

binders :: Int -> ConvEnv -> ConvEnv
binders n ConvEnv{..} = ConvEnv { conv_locals = n + conv_locals }

bindVar :: D.Type -> (D.Value -> ConvM a) -> ConvM a
bindVar tp k = local (binders 1) $ do
    n <- asks conv_locals
    k $ D.var (Env.unsafeLevel $ n - 1) tp

--------------------------------------------------------------------------------
-- Conversion Checking

-- | Check to see if two values of the same type are equal.
equate :: D.Type -> D.Value -> D.Value -> ConvM ()
equate (D.Pi _ base fam) tm0 tm1 =
    bindVar base $ \x -> do
      fib <- instTpClo fam x
      ap0 <- app tm0 x
      ap1 <- app tm1 x
      equate fib ap0 ap1
equate _ D.Zero D.Zero =
    pure ()
equate tp (D.Suc tm0) (D.Suc tm1) =
    equate tp tm0 tm1
equate _ (D.Cut neu0 _) (D.Cut neu1 _) =
    equateNeu neu0 neu1
equate univ (D.Rel tp0 sm0) (D.Rel tp1 sm1) = do
    equateTp tp0 tp1
    equate (D.Small tp0 univ) sm0 sm1
equate _ D.NatSmall D.NatSmall =
    pure ()
equate (D.Small (D.Pi ident base fam) univ) (D.PiSmall sbase0 sfam0) (D.PiSmall sbase1 sfam1) = do
    equate (D.Small base univ) sbase0 sbase1
    bindVar base $ \x -> do
      fib <- instTpClo fam x
      ap0 <- app sfam0 x
      ap1 <- app sfam1 x
      equate (D.Small fib univ) ap0 ap1
equate _ tm0 tm1 =
    failure $ ExpectedEqual tm0 tm1

equateNeu :: D.Neutral -> D.Neutral -> ConvM ()
equateNeu (D.Neutral hd0 sp0) (D.Neutral hd1 sp1) =
    equateHead hd0 hd1

-- [FIXME: Reed M, 06/11/2021] Unfold globals
equateHead :: D.Head -> D.Head -> ConvM ()
equateHead (D.Local lvl0) (D.Local lvl1) | lvl0 == lvl1 = pure ()
equateHead (D.Global lvl0 _) (D.Global lvl1 _) | lvl0 == lvl1 = pure ()
equateHead hd0 hd1 = failure $ ExpectedEqualHead hd0 hd1

equateSpine :: [D.Frame] -> [D.Frame] -> ConvM ()
equateSpine full0 full1 = go full0 full1
    where
      go [] [] = pure ()
      go (k0 : sp0) (k1 : sp1) = do
          equateFrame k0 k1
          equateSpine sp0 sp1
      go _ _ = failure $ ExpectedSpineEqual full0 full1

equateFrame :: D.Frame -> D.Frame -> ConvM ()
equateFrame (D.App tp0 val0) (D.App tp1 val1) = do
    equateTp tp0 tp1
    equate tp0 val0 val1

-- | Check to see if two types are equal.
equateTp :: D.Type -> D.Type -> ConvM ()
equateTp (D.Univ i) (D.Univ j) =
    when (i /= j) $
      failure $ ExpectedEqualTp (D.Univ i) (D.Univ j)
equateTp D.Nat D.Nat =
    pure ()
equateTp (D.Pi _ base0 fam0) (D.Pi _ base1 fam1) = do
    equateTp base0 base1
    bindVar base0 $ \x -> do
        fib0 <- instTpClo fam0 x
        fib1 <- instTpClo fam1 x
        equateTp fib0 fib1
equateTp (D.El univ0 code0) (D.El univ1 code1) = do
    equateTp univ0 univ1
    equate univ0 code0 code1
equateTp (D.ElCut tp0 neu0) (D.ElCut tp1 neu1) = do
    equateTp tp0 tp1
    equateNeu neu0 neu1
equateTp (D.Small tp0 univ0) (D.Small tp1 univ1) = do
    equateTp tp0 tp1
    equateTp univ0 univ1
equateTp tp0 tp1 =
    failure $ ExpectedEqualTp tp0 tp1
