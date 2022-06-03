-- |
module TeenyTT.Core.Conversion
  ( ConvertM
  , runConvertM
  , equate
  , equateTp
  ) where

import Control.Exception
import Control.Monad.Reader

import Data.Text (Text)

import System.IO.Unsafe (unsafePerformIO)

import TeenyTT.Base.Diagnostic

import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Eval qualified as Eval

--------------------------------------------------------------------------------
-- The Conversion Monad
--
-- [NOTE: Exceptions + IO]
-- We very explictly do /not/ use 'ExceptT' here, and instead opt to use
-- IO + exceptions. This is for a myriad of reasons, but mostly due to the
-- fact that 'ExceptT' is terrible for performance.

newtype ConvertM a = ConvertM { unConvertM :: ReaderT ConvertEnv IO a }
    deriving newtype (Functor, Applicative, Monad, MonadReader ConvertEnv)

runConvertM :: Int -> ConvertM a -> Maybe a
runConvertM size m =
    unsafePerformIO $
    let env = ConvertEnv { size, mode = Rigid }
    in catch (Just <$> runReaderT m.unConvertM env) \case
      NotConvertible -> pure Nothing

data Mode
    = Full
    | Rigid
    | Flex
    deriving stock (Show, Eq)

data ConvertEnv
    = ConvertEnv
    { size :: Int
    , mode :: Mode
    }

getMode :: ConvertM Mode
getMode = asks (\env -> env.mode)

withMode :: Mode -> ConvertM a -> ConvertM a
withMode mode m = local (\env -> env { mode = mode }) m

unfold :: D.Term -> ConvertM D.Term
unfold tm = do
    mode <- getMode
    case mode of
      Full -> pure $ Eval.unfold tm
      _ -> pure tm

extend :: (D.Term -> ConvertM a) -> ConvertM a
extend k = do
    lvl <- asks (\env -> env.size)
    local (\env -> env { size = env.size + 1 }) (k $ D.Local lvl [])

data NotConvertible = NotConvertible
    deriving stock (Show)
    deriving anyclass (Exception)


notConvertible :: ConvertM a
notConvertible = ConvertM $ liftIO $ throwIO NotConvertible

backtrack :: ConvertM () -> ConvertM () -> ConvertM ()
backtrack (ConvertM m0) (ConvertM m1) = ConvertM $ ReaderT $ \env -> do
    catch (runReaderT m0 env) \case
        NotConvertible -> runReaderT m1 env

--------------------------------------------------------------------------------
-- Equating Terms/Types

equate :: D.Term -> D.Term -> ConvertM ()
equate tm0 tm1 = do
    utm0 <- unfold tm0
    utm1 <- unfold tm1
    mode <- getMode
    case (utm0, utm1) of
      (D.VNeu neu0, D.VNeu neu1) ->
          equateNeu neu0 neu1
      (D.VLam _ clo0, D.VLam _ clo1) ->
          equateTmClo clo0 clo1
      (D.VPair l0 r0, D.VPair l1 r1) -> do
          equate l0 l1
          equate r0 r1
      (D.VZero, D.VZero) ->
          pure ()
      (D.VSuc n0, D.VSuc n1) ->
          equate n0 n1
      (D.VCodePi _ base0 clo0, D.VCodePi _ base1 clo1) -> do
          equate base0 base1
          equateTmClo clo0 clo1
      (D.VCodeUniv, D.VCodeUniv) ->
          pure ()
      (D.VCodeNat, D.VCodeNat) ->
          pure ()
      -- We unfold globals in Rigid Mode before attempting to eta-expand.
      (D.Global _ v0 _, v1) | mode == Rigid ->
          equate v0 v1
      (v0, D.Global _ v1 _) | mode == Rigid ->
          equate v0 v1
      -- When equating against a neutral, it's time to eta-expand.
      (D.VNeu neu, v) ->
          equateEta neu v
      (v, D.VNeu neu) ->
          equateEta neu v
      _ ->
          notConvertible

equateTp :: D.Type -> D.Type -> ConvertM ()
equateTp tp0 tp1 =
    case (tp0, tp1) of
      (D.VElNeu neu0, D.VElNeu neu1) ->
          equateNeu neu0 neu1
      (D.VPi _ base0 clo0, D.VPi _ base1 clo1) -> do
          equateTp base0 base1
          equateTpClo clo0 clo1
      (D.VSigma _ base0 clo0, D.VSigma _ base1 clo1) -> do
          equateTp base0 base1
          equateTpClo clo0 clo1
      (D.VNat, D.VNat) ->
          pure ()
      (D.VUniv, D.VUniv) ->
          pure ()
      _ ->
          notConvertible


--------------------------------------------------------------------------------
-- Equating Neutrals

equateNeu :: D.Neu -> D.Neu -> ConvertM ()
equateNeu neu0 neu1 =
    case (neu0.hd, neu1.hd) of
      (D.KLocal ix0, D.KLocal ix1) | ix0 == ix1 ->
          pure ()
      (D.KGlobal name0 ~v0, D.KGlobal name1 ~v1) ->
          equateGlobals name0 name1 v0 v1 neu0.spine neu1.spine
      _ ->
          notConvertible

equateGlobals :: Text -> Text -> D.Term -> D.Term -> [D.Frame] -> [D.Frame] -> ConvertM ()
equateGlobals name0 name1 ~v0 ~v1 spine0 spine1 = do
    getMode >>= \case
        Flex ->
            -- Flex Mode: Don't perform any unfolding.
            if name0 == name1 then
              equateSpines spine0 spine1
            else
              notConvertible
        Rigid ->
            -- Rigid Mode: If the heads match, try to equate the spines without performing
            -- any unfolding. If this fails, we switch into full mode to avoid large
            -- amounts of backtracking.
            if name0 == name1 then
              backtrack (withMode Flex (equateSpines spine0 spine1))
                        (withMode Full (equate v0 v1))
            else
              equate v0 v1
        Full ->
            -- Full Mode: We unfold /all/ globals in full mode, so
            -- if we hit this case something has gone horribly wrong.
            impossible "Encountered a folded global in Full mode."

equateFrame :: D.Frame -> D.Frame -> ConvertM ()
equateFrame (D.KAp v0) (D.KAp v1) =
    equate v0 v1
equateFrame D.KFst D.KFst =
    pure ()
equateFrame D.KSnd D.KSnd =
    pure ()
equateFrame (D.KNatElim mot0 z0 s0) (D.KNatElim mot1 z1 s1) = do
    equate mot0 mot1
    equate z0 z1
    equate s0 s1
equateFrame _ _ = 
    notConvertible

equateSpines :: [D.Frame] -> [D.Frame] -> ConvertM ()
equateSpines [] [] =
    pure ()
equateSpines (frm0 : spine0) (frm1 : spine1) = do
    equateSpines spine0 spine1
equateSpines  _ _ =
    notConvertible

--------------------------------------------------------------------------------
-- Eta Expansion

equateEta :: D.Neu -> D.Term -> ConvertM ()
equateEta neu0 v1 = do
    uv1 <- unfold v1
    case uv1 of
      D.VNeu neu1 ->
          equateNeu neu0 neu1
      D.VLam _ clo1 ->
          extend \v ->
          let unf vfn = Eval.doAp vfn v
          in equateEta (D.pushFrame neu0 (D.KAp v) unf) (Eval.instTmClo clo1 v)
      D.VPair vl vr -> do
          equateEta (D.pushFrame neu0 D.KFst Eval.doFst) vl
          equateEta (D.pushFrame neu0 D.KFst Eval.doSnd) vr
      _ ->
          notConvertible

--------------------------------------------------------------------------------
-- Equating Closures

equateTmClo :: D.Clo S.Term -> D.Clo S.Term -> ConvertM ()
equateTmClo clo0 clo1 =
    extend \v -> equate (Eval.instTmClo clo0 v) (Eval.instTmClo clo1 v)

equateTpClo :: D.Clo S.Type -> D.Clo S.Type -> ConvertM ()
equateTpClo clo0 clo1 =
    extend \v -> equateTp (Eval.instTpClo clo0 v) (Eval.instTpClo clo1 v)
