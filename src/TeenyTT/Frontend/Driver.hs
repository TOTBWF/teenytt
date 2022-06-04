-- | The main driver of @teenytt@.
module TeenyTT.Frontend.Driver
  ( runDriver
  , loadFile
  , lexFile
  , parseFile
  ) where

import Control.Monad.IO.Class

import Data.ByteString qualified as BS
import Data.Foldable

import Prettyprinter.Render.Text

import TeenyTT.Base.Pretty

import TeenyTT.Elaborator.Monad
import TeenyTT.Elaborator.Refiner qualified as Elab
import TeenyTT.Elaborator.Tactic qualified as T

import TeenyTT.Frontend.Command (Command(..), displayCommand)
import TeenyTT.Frontend.Driver.Monad
import TeenyTT.Frontend.Parser qualified as P

unimplemented :: (MonadIO m) => String -> m ()
unimplemented msg = liftIO $ putStrLn $ "Unimplemented: " <> msg

divider :: Driver ()
divider = liftIO $ putStrLn $ '\n' : replicate 80 '-' 

--------------------------------------------------------------------------------
-- Loading

loadFile :: FilePath -> IO ()
loadFile path = do
    bytes <- BS.readFile path
    runDriver path bytes $ do
        cmds <- liftIO $ P.commands path bytes
        traverse_ execCmd cmds

lexFile :: FilePath -> IO ()
lexFile path = do
    bytes <- BS.readFile path
    runDriver path bytes $ do
        toks <- P.tokenize path bytes
        liftIO $ putDoc $ vcat $ fmap pretty toks

parseFile :: FilePath -> IO ()
parseFile path = do
    bytes <- BS.readFile path
    runDriver path bytes $ do
        toks <- P.tokenize path bytes
        liftIO $ putDoc $ vcat $ fmap pretty toks
        divider
        cmds <- liftIO $ P.commands path bytes
        liftIO $ putDoc $ vcat (fmap displayCommand cmds)
        divider


--------------------------------------------------------------------------------
-- Commands

execCmd :: Command -> Driver ()
execCmd (Annotate ident ctp) = do
    tp <-  elab $ T.runTp (Elab.typ ctp)
    vtp <- elab $ evalTp tp
    annotateTp ident vtp
execCmd (Define ident ctm) = do
    (tm, vtp) <- getAnnotation ident >>= \case
      Just vtp -> do
          tm <- elab $ T.runChk (Elab.chk ctm) vtp
          pure (tm, vtp)
      Nothing ->
          elab $ T.runSyn $ Elab.syn ctm
    ~vtm <- elab $ eval tm
    define ident vtm vtp
execCmd (Directive _) =
    unimplemented "directives"
