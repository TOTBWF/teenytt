-- |
module TeenyTT.Core.Quote
  ( QuoteM
  , runQuoteM
  , quote
  , quoteTp
  ) where

import Control.Monad.Reader

import Data.Foldable

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Eval qualified as Eval

--------------------------------------------------------------------------------
-- The Quotation Monad

newtype QuoteM a = QuoteM { unQuoteM :: Reader Int a }
    deriving newtype (Functor, Applicative, Monad, MonadReader Int)

runQuoteM :: Int -> QuoteM a -> a
runQuoteM size m = runReader m.unQuoteM size

var :: Int -> QuoteM S.Term
var lvl = do
    size <- ask
    pure $ S.Local (size - lvl - 1)

extend :: (Int -> QuoteM a) -> QuoteM a
extend k = local (1 +) (k =<< ask)

quote :: D.Term -> QuoteM S.Term
quote (D.VNeu hd spine) =
    quoteNeu hd spine
quote (D.VLam x clo) =
    S.Lam x <$> quoteTmClo clo
quote (D.VPair l r) =
    S.Pair <$> quote l <*> quote r
quote D.VZero =
    pure S.Zero
quote (D.VSuc v) =
    S.Suc <$> quote v
quote (D.VCodePi x base clo) =
    S.CodePi x <$> quote base <*> quoteTmClo clo
quote (D.VCodeSigma x base clo) =
    S.CodeSigma x <$> quote base <*> quoteTmClo clo
quote D.VCodeUniv =
    pure S.CodeUniv
quote D.VCodeNat =
    pure S.CodeNat

quoteTp :: D.Type -> QuoteM S.Type
quoteTp (D.VElNeu hd frms) =
    S.El <$> quoteNeu hd frms
quoteTp (D.VPi x base clo) =
    S.Pi x <$> quoteTp base <*> quoteTpClo clo
quoteTp (D.VSigma x base clo) =
    S.Sigma x <$> quoteTp base <*> quoteTpClo clo
quoteTp D.VNat =
    pure S.Nat
quoteTp D.VUniv =
    pure S.Univ


--------------------------------------------------------------------------------
-- Quoting Neutrals

quoteHead :: D.Head -> QuoteM S.Term
quoteHead (D.KLocal lvl) = var lvl
quoteHead (D.KGlobal name ~v) = pure $ S.Global name v
quoteHead D.KHole = pure $ S.Hole

quoteFrame :: D.Frame -> S.Term -> QuoteM S.Term
quoteFrame (D.KAp varg) tm =
    S.Ap tm <$> quote varg
quoteFrame D.KFst tm = 
    pure $ S.Fst tm
quoteFrame D.KSnd tm =
    pure $ S.Snd tm
quoteFrame (D.KNatElim vmot vz vs) tm =
    S.NatElim <$> quote vmot <*> quote vz <*> quote vs <*> pure tm

quoteNeu :: D.Head -> [D.Frame] -> QuoteM S.Term
quoteNeu hd spine = do
    tm <- quoteHead hd
    foldrM quoteFrame tm spine

--------------------------------------------------------------------------------
-- Quoting Closures

quoteTmClo :: D.Clo S.Term -> QuoteM S.Term
quoteTmClo clo = extend \lvl -> quote (Eval.instTmClo clo (D.local lvl))

quoteTpClo :: D.Clo S.Type -> QuoteM S.Type
quoteTpClo clo = extend \lvl -> quoteTp (Eval.instTpClo clo (D.local lvl))
