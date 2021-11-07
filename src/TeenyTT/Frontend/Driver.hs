module TeenyTT.Frontend.Driver
  ( loadFile
  , benchFile
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS


import Data.Int
import Data.Text (Text)
import Data.Text qualified as T

import Criterion.Measurement qualified as Bench
import Criterion.Measurement.Types qualified as Bench (nf, Measured (..), fromInt)

import TeenyTT.Core.Pretty
import TeenyTT.Frontend.Parser qualified as P

divider :: IO ()
divider = putStrLn (replicate 80 '-')

renderBench :: Text -> Bench.Measured -> Doc ann
renderBench lbl msr@(Bench.Measured {..}) =
    nest 4 $ vsep [ pretty lbl <> ":"
                  , "Mean Time:" <+> (pretty $ Bench.secs (measTime / fromIntegral measIters))
                  , "Iterations:" <+> (pretty measIters)
                  ]

benchOp :: (NFData a, NFData b) => Text -> Int64 -> (a -> b) -> a -> IO b
benchOp lbl iters f a = do
    input <- evaluate $ force a
    (msr, _) <- Bench.measure (Bench.nf f input) iters
    putDoc $ renderBench lbl msr
    putStrLn ""
    pure $ f input

benchFile :: FilePath -> Int -> IO ()
benchFile path (fromIntegral -> iters) = do
    bytes <- BS.readFile path
    toks <- benchOp "Lexer" iters (P.tokens path) bytes
    cmds <- benchOp "Parser" iters (P.commands path) bytes
    pure ()

loadFile :: FilePath -> IO ()
loadFile path = do
    bytes <- BS.readFile path
    divider
    print $ P.tokens path bytes
    divider
    print $ P.commands path bytes
