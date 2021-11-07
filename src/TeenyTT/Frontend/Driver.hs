module TeenyTT.Frontend.Driver
  ( runDriver
  , loadFile
  , benchFile
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable

import Data.IORef

import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

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

newtype Driver a = Driver { unDriver :: ReaderT DriverState IO a }
    deriving (Functor, Applicative, Monad, MonadReader DriverState, MonadIO)

runDriver :: Driver a -> IO a
runDriver m = do
    globals <- newIORef mempty
    typeAnns <- newIORef mempty
    runReaderT (unDriver m) (DriverState {..})

data DriverState = DriverState
    { globals :: IORef (Env (D.Value, D.Type))
    , typeAnns :: IORef (Map Ident D.Type)
    }

-- [FIXME: Reed M, 06/11/2021] Prevent annotating the same type twice
annTp :: Ident -> D.Type -> Driver ()
annTp x tp = do
    anns <- asks typeAnns
    liftIO $ modifyIORef anns (Map.insert x tp)

getAnn :: Ident -> Driver (Maybe D.Type)
getAnn x = do
    annRef <- asks typeAnns
    anns <- liftIO $ readIORef annRef
    pure $ Map.lookup x anns

-- [FIXME: Reed M, 06/11/2021] This should use proper pretty printing
hoistError :: (Show err) => Either err a -> Driver a
hoistError (Left err) = liftIO $ fail $ show err
hoistError (Right a) = pure a

liftRM :: RM a -> Driver a
liftRM m = do
    res <- liftIO $ runRM mempty m
    hoistError res

--------------------------------------------------------------------------------
-- Printing

divider :: Driver ()
divider = liftIO $ putStrLn (replicate 80 '-')

debugPrint :: (Debug a) => a -> Driver ()
debugPrint a = liftIO $ do
    putDoc $ dump a
    putStrLn ""


--------------------------------------------------------------------------------
-- Commands

-- [FIXME: Reed M, 06/11/2021] Provide the option to silence output
command :: CS.Command -> Driver ()
command (CS.TypeAnn x e) = do
    tp <- liftRM $ T.runTp $ Elab.chkTp e
    vtp <- liftRM $ liftEval $ Eval.evalTp tp
    -- divider
    -- debugPrint tp
    -- divider
    -- debugPrint vtp
    annTp x vtp
command (CS.Def x e) = do
    -- divider
    getAnn x >>= \case
      Just tp -> (void . liftIO . evaluate . force) =<< (liftRM $ T.runChk (Elab.chkTm e) tp)
      Nothing -> (void . liftIO . evaluate . force) =<< (liftRM $ T.runSyn (Elab.synTm e))
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
    putDoc $ renderBench lbl msr
    putStrLn ""
    pure $ f input

-- [FIXME: Reed M, 06/11/2021] I should be using some sort of ident -> text conversion here
commandLabel :: CS.Command -> Text
commandLabel (CS.TypeAnn x _) = T.pack $ "Annotate " <> show x
commandLabel (CS.Def x _) = T.pack $ "Define " <> show x
commandLabel (CS.Directive x _) = x

-- [FIXME: Reed M, 06/11/2021] This is going to have bizzare effects on my IORefs.
benchCommand :: Int64 -> CS.Command -> Driver () 
benchCommand iters cmd = do
    env <- ask
    liftIO $ do
      input <- evaluate $ force cmd
      (msr, _) <- Bench.measure (Bench.nfAppIO (flip runReaderT env . unDriver . command) input) iters
      putDoc $ renderBench (commandLabel input) msr
      putStrLn ""

benchFile :: FilePath -> Int -> Driver ()
benchFile path (fromIntegral -> iters) = do
    bytes <- liftIO $ BS.readFile path
    toks <- benchOp "Lexer" iters (P.tokens path) bytes
    cmds <- hoistError =<< benchOp "Parser" iters (P.commands path) bytes
    traverse_ (benchCommand iters) cmds
    pure ()
