-- | Core NbE Wrappers.
module TeenyTT.Core.NbE (
    -- * Evaluation
      eval
    , evalTp
    -- * Quotation
    , quote
    , quoteTp
    -- * Conversion
    , equate
    , equateTp
    -- * Closures
    , Eval.instTmClo
    , Eval.instTpClo
    -- * Eliminators
    , Eval.doEl
    -- * Splicing
    , Eval.spliceTm
    , Eval.spliceTp
    ) where

import TeenyTT.Core.Eval qualified as Eval
import TeenyTT.Core.Quote qualified as Quote
import TeenyTT.Core.Conversion qualified as Conv

import TeenyTT.Base.Env (Env)

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D

--------------------------------------------------------------------------------
-- Evaluation

eval :: Env D.Term -> S.Term -> D.Term
eval env tm = Eval.runEvalM env (Eval.eval tm)

evalTp :: Env D.Term -> S.Type -> D.Type
evalTp env tm = Eval.runEvalM env (Eval.evalTp tm)

--------------------------------------------------------------------------------
-- Quotation

quote :: Int -> D.Term -> S.Term
quote size v = Quote.runQuoteM size (Quote.quote v)

quoteTp :: Int -> D.Type -> S.Type
quoteTp size v = Quote.runQuoteM size (Quote.quoteTp v)

--------------------------------------------------------------------------------
-- Conversion

equate :: Int -> D.Term -> D.Term -> Bool
equate size tm0 tm1 = Conv.runConvertM size (Conv.equate tm0 tm1)

equateTp :: Int -> D.Type -> D.Type -> Bool
equateTp size tp0 tp1 = Conv.runConvertM size (Conv.equateTp tp0 tp1)
