module TeenyTT.Frontend.Driver
  ( runDriver
  , loadFile
  , benchFile
  , withDebug
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)

import Control.Monad.Reader

import Data.Foldable

import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T

import System.IO.Silently

import Criterion.Measurement qualified as Bench
import Criterion.Measurement.Types qualified as Bench (nf, nfAppIO, Measured (..))

import TeenyTT.Core.Ident
import TeenyTT.Core.Pretty
import TeenyTT.Core.Position

import TeenyTT.Core.Refiner.Monad qualified as RM

import TeenyTT.Core.Eval qualified as Eval
import TeenyTT.Core.Quote qualified as Quote

import TeenyTT.Core.Domain qualified as D
import TeenyTT.Core.Tactic qualified as T

import TeenyTT.Frontend.ConcreteSyntax qualified as CS
import TeenyTT.Frontend.Elaborator qualified as Elab
import TeenyTT.Frontend.Parser qualified as P

import TeenyTT.Frontend.Driver.Monad

--------------------------------------------------------------------------------
-- Commands

elabChkTp :: Loc CS.Expr -> Driver D.Type
elabChkTp e = do
    message Debug $ "Checking Type" <+> squotes (dump e)
    tp <- liftRM $ T.runTp $ Elab.chkTp e
    message Debug $ "Elaborated Type" <+> squotes (dump tp)
    vtp <- liftRM $ RM.liftEval $ Eval.evalTp tp
    message Debug $ "Evaluated Type" <+> squotes (dump vtp)
    pure vtp

elabChkTm :: Loc CS.Expr -> D.Type -> Driver (D.Value, D.Type)
elabChkTm e tp = do
    message Debug $ "Checking Term" <+> squotes (dump e) <+> "of type" <+> squotes (dump tp)
    tm <- liftRM $ T.runChk (Elab.chkTm e) tp
    message Debug $ "Evaluating Term" <+> squotes (dump tm)
    vtm <- liftRM $ RM.liftEval $ Eval.eval tm
    message Debug $ "Evaluated Term" <+> squotes (dump vtm)
    pure (vtm, tp)

elabSynTm :: Loc CS.Expr -> Driver (D.Value, D.Type)
elabSynTm e = do
    message Debug $ "Synthesizing Term" <+> squotes (dump e)
    (tm, tp) <- liftRM $ T.runSyn $ Elab.synTm e
    message Debug $ "Elaborated Term" <+> squotes (dump tm) <+> "of type" <+> squotes (dump tp)
    vtm <- liftRM $ RM.liftEval $ Eval.eval tm
    message Debug $ "Evaluated Term" <+> squotes (dump vtm)
    pure (vtm, tp)

printBinding :: Ident -> D.Value -> D.Type -> Driver ()
printBinding x tm tp = do
    qtp <- liftRM $ RM.liftQuote $ Quote.quoteTp tp
    qtm <- liftRM $ RM.liftQuote $ Quote.quote tp tm
    divider
    message Info $ nest 2 $ vsep
      [ "Printing" <+> pretty x
      , pretty x <+> colon <+> dump qtp
      , pretty x <+> equals <+> dump qtm
      ]

command :: CS.Command -> Driver ()
command (CS.TypeAnn x e) = do
    vtp <- elabChkTp e
    annotateTp x vtp
command (CS.Def x e) = do
    ann <- getAnnotation x
    (tm, tp) <- case ann of
      Just tp -> elabChkTm e tp
      Nothing -> elabSynTm e
    bindGlobal x tm tp
command (CS.Directive "print" [unlocate -> CS.Var x]) = do
    binding <- getGlobal x
    case binding of
      Just (tm, tp) -> printBinding x tm tp
      Nothing       -> message Error $ "No such variable" <+> squotes (pretty x)
command (CS.Directive "debug" [unlocate -> CS.Var (User "on")]) =
    setDebugMode True
command (CS.Directive "debug" [unlocate -> CS.Var (User "off")]) =
    setDebugMode False
command (CS.Directive dir _) =
    message Error $ "Unsupported Directive:" <+> squotes (pretty dir)

loadFile :: FilePath -> Driver ()
loadFile path = do
    bytes <- liftIO $ BS.readFile path

    dbg <- getDebugMode 
    when dbg $ do
      toks <- hoistError $ P.tokens path bytes
      message Debug (pretty $ show toks)

    cmds <- hoistError $ P.commands path bytes
    message Debug (pretty $ show cmds)
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

--------------------------------------------------------------------------------
-- Debugging

withDebug :: Bool -> Driver () -> Driver ()
withDebug dbg m = do
    setDebugMode dbg
    m
