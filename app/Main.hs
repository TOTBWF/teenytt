module Main where

import Options.Applicative

import TeenyTT.Frontend.Driver qualified as TeenyTT

data Command
    = LoadFile FilePath
    | BenchFile FilePath Int


loadFileOpts :: Parser Command
loadFileOpts = LoadFile <$> argument str (metavar "FILE" <> action "file")

loadFileCmd :: Mod CommandFields Command
loadFileCmd = command "load" (info loadFileOpts (progDesc "Load a file with teenytt"))

benchFileOpts :: Parser Command
benchFileOpts = BenchFile <$> argument str (metavar "FILE" <> action "file")
                         <*> option auto (long "num-iters" <> short 'n' <> value 10000 <> metavar "ITERS")

benchFileCmd :: Mod CommandFields Command
benchFileCmd = command "bench" (info benchFileOpts (progDesc "Benchmark a file with teenytt"))

cmds :: ParserInfo Command
cmds = info (subparser (loadFileCmd <> benchFileCmd) <**> helper) (progDesc "TeenyTT -- A Teeny Type Theory")

main :: IO ()
main = do
    cmd <- execParser cmds
    case cmd of
      LoadFile path -> TeenyTT.loadFile path
      BenchFile path iters -> TeenyTT.benchFile path iters
