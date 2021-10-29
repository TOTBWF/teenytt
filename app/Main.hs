module Main where

import Options.Applicative

import TeenyTT.Frontend.Driver qualified as TeenyTT

data Command
    = LoadFile FilePath


loadFileOpts :: Parser Command
loadFileOpts = LoadFile <$> argument str (metavar "FILE" <> action "file")

loadFileCmd :: Mod CommandFields Command
loadFileCmd = command "load" (info loadFileOpts (progDesc "Load a file with teenytt"))

cmds :: ParserInfo Command
cmds = info (subparser $ loadFileCmd) (progDesc "TeenyTT -- A Teeny Type Theory")

main :: IO ()
main = do
    cmd <- execParser cmds
    case cmd of
      LoadFile path -> TeenyTT.loadFile path
