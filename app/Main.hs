module Main where

import Options.Applicative

import TeenyTT.Frontend.Driver qualified as TeenyTT


--------------------------------------------------------------------------------
-- Commands

data Command
    = LoadFile FilePath
    | LexFile FilePath
    | ParseFile FilePath

--------------------------------------------------------------------------------
-- File Loading

loadFileOpts :: Parser Command
loadFileOpts = LoadFile <$> argument str (metavar "FILE" <> action "file")

loadFileCmd :: Mod CommandFields Command
loadFileCmd = command "load" (info loadFileOpts (progDesc "Load a file with teenytt"))

--------------------------------------------------------------------------------
-- Lexing 

lexFileOpts :: Parser Command
lexFileOpts = LexFile <$> argument str (metavar "FILE" <> action "file")

lexFileCmd :: Mod CommandFields Command
lexFileCmd = command "lex" (info lexFileOpts (progDesc "Lex a file with teenytt"))

--------------------------------------------------------------------------------
-- Parsing 

parseFileOpts :: Parser Command
parseFileOpts = ParseFile <$> argument str (metavar "FILE" <> action "file")

parseFileCmd :: Mod CommandFields Command
parseFileCmd = command "parse" (info parseFileOpts (progDesc "Parse a file with teenytt"))

--------------------------------------------------------------------------------
-- Main

cmds :: ParserInfo Command
cmds = info (subparser (loadFileCmd <> lexFileCmd <> parseFileCmd) <**> helper) (progDesc "TeenyTT -- A Teeny Type Theory")

main :: IO ()
main = do
    cmd <- execParser cmds
    case cmd of 
      LoadFile path -> TeenyTT.loadFile path
      LexFile path -> TeenyTT.lexFile path
      ParseFile path -> TeenyTT.parseFile path
