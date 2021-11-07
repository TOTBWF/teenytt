module TeenyTT.Frontend.Driver
  ( runDriver
  , loadFile
  , benchFile
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)

import Control.Monad.Reader

import Data.Foldable

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T

import System.IO.Silently

import Criterion.Measurement qualified as Bench
import Criterion.Measurement.Types qualified as Bench (nf, nfAppIO, Measured (..), fromInt)

import TeenyTT.Core.Ident
import TeenyTT.Core.Env (Env)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Refiner.Monad
import TeenyTT.Core.Pretty

import TeenyTT.Core.Eval qualified as Eval
import TeenyTT.Core.Quote qualified as Quote

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Syntax qualified as S
import TeenyTT.Core.Tactic qualified as T

import TeenyTT.Frontend.ConcreteSyntax qualified as CS
import TeenyTT.Frontend.Elaborator qualified as Elab
import TeenyTT.Frontend.Parser qualified as P

import TeenyTT.Frontend.Driver.Monad

--------------------------------------------------------------------------------
-- Commands

elabChkTm :: CS.Expr -> D.Type -> Driver (D.Value, D.Type)
elabChkTm e tp = do
    tm <- liftRM $ T.runChk (Elab.chkTm e) tp
    vtm <- liftRM $ liftEval $ Eval.eval tm
    pure (vtm, tp)

elabSynTm :: CS.Expr -> Driver (D.Value, D.Type)
elabSynTm e = do
    (tm, tp) <- liftRM $ T.runSyn $ Elab.synTm e
    vtm <- liftRM $ liftEval $ Eval.eval tm
    pure (vtm, tp)

command :: CS.Command -> Driver ()
command (CS.TypeAnn x e) = do
    tp <- liftRM $ T.runTp $ Elab.chkTp e
    vtp <- liftRM $ liftEval $ Eval.evalTp tp
    annotateTp x vtp
command (CS.Def x e) = do
    ann <- getAnnotation x
    (tm, tp) <- case ann of
      Just tp -> elabChkTm e tp
      Nothing -> elabSynTm e
    bindGlobal x tm tp
command (CS.Directive dir _) = liftIO $ putStrLn $ "Unsupported Directive: " <> (T.unpack dir)

loadFile :: FilePath -> Driver ()
loadFile path = do
    bytes <- liftIO $ BS.readFile path
    cmds <- hoistError $ P.commands path bytes
    traverse_ command cmds

--------------------------------------------------------------------------------
-- Benchmarking

renderBench :: Text -> Bench.Measured -> Doc ann
renderBench lbl msr@(Bench.Measured {..}) =
    nest 4 $ vsep [ pretty lbl <> ":"
                  , "Mean Time:" <+> (pretty $ Bench.secs (measTime / fromIntegral measIters))
                  , "Iterations:" <+> (pretty measIters)
                  ]

benchOp :: (NFData a, NFData b) => Text -> Int64 -> (a -> b) -> a -> Driver b
benchOp lbl iters f a = liftIO $ do
    input <- evaluate $ force a
    (msr, _) <- Bench.measure (Bench.nf f input) iters
    putDocLn $ renderBench lbl msr
    pure $ f input

-- [FIXME: Reed M, 06/11/2021] I should be using some sort of ident -> text conversion here
commandLabel :: CS.Command -> Text
commandLabel (CS.TypeAnn x _) = T.pack $ "Annotate " <> show x
commandLabel (CS.Def x _) = T.pack $ "Define " <> show x
commandLabel (CS.Directive x _) = x

benchCommand :: Int64 -> CS.Command -> Driver () 
benchCommand iters cmd = do
    action <- sandbox (command cmd)
    input <- liftIO $ evaluate $ force cmd
    (msr, _) <- liftIO $ silence $ Bench.measure (Bench.nfAppIO action ()) iters
    putDocLn $ renderBench (commandLabel input) msr
    -- NOTE: We need to actually run the command outside of a sandbox to ensure that
    -- globals get updated.
    command cmd

benchFile :: FilePath -> Int -> Driver ()
benchFile path (fromIntegral -> iters) = do
    bytes <- liftIO $ BS.readFile path
    toks <- benchOp "Lexer" iters (P.tokens path) bytes
    cmds <- hoistError =<< benchOp "Parser" iters (P.commands path) bytes
    traverse_ (benchCommand iters) cmds
    pure ()
