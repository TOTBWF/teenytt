module TeenyTT.Core.Splice
  ( Splice
  , val
  , tp
  , clo
  -- , tpclo
  , term
  , compile
  ) where

import TeenyTT.Core.Ident

import TeenyTT.Core.Compute
import TeenyTT.Core.TermBuilder (TB)
import TeenyTT.Core.TermBuilder as TB

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import qualified TeenyTT.Core.Env as Env

-- | A 'Splice' gives us a means of building terms
-- in the semantic domain, while avoiding having to do
-- lots of DeBruijin Arithmetic.
--
-- We do so by building terms that contain only variables,
-- and then tracking all the terms/types we've "spliced"
-- in an environment.
newtype Splice a = Splice { unSplice :: D.Env -> (D.Env, TB a) }

-- | Splice a 'D.Value'.
val :: D.Value -> (TB S.Term -> Splice a) -> Splice a
val val k = Splice $ \env ->
  let env' = D.bindVal val env
      v    = TB.var $ TB.lvl env'
  in unSplice (k v) env'

-- | Splice a 'D.Type'
tp :: D.Type -> (TB S.Type -> Splice a) -> Splice a
tp tp k = Splice $ \env ->
  let env' = D.bindTp tp env
      v    = TB.tpvar $ TB.tplvl env'
  in unSplice (k v) env'

clo :: D.Clo S.Term -> (TB S.Term -> Splice a) -> Splice a
clo cl = val (D.Lam Anon cl)

term :: TB a -> Splice a
term tb = Splice $ \env -> (env, tb)

compile :: Splice a -> (D.Env, a)
compile (Splice sp) =
    let (env, tb) = sp mempty
    in (env, runTB env tb)
