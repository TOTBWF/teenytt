{
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module TeenyTT.Frontend.Parser.Grammar where

import Control.Monad.Except

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

-- FIXME: Hack
import Data.String

import qualified TeenyTT.Frontend.Parser.Token as T
import TeenyTT.Frontend.Parser.Lexer.Monad

import TeenyTT.Frontend.ConcreteSyntax
import TeenyTT.Core.Ident


}

%name toplevel toplevel
%name expr expr
%tokentype { T.Token }
%monad { Lexer }
%error { parseError }

%token
  name   { T.Identifier $$ }
  '->'   { T.Arrow }
  forall { T.ForAll }
  '('    { T.LParen }
  ':'    { T.Colon }
  '='    { T.Equal }
  ')'    { T.RParen }
  -- Layout Tokens
  block_open   { T.BlockOpen }
  block_break  { T.BlockBreak }
  block_close  { T.BlockClose }

%right '->'

%%


--------------------------------------------------------------------------------
-- Top Level

toplevel :: { [Command] }
toplevel : block_open cmds block_close { $2 }

--------------------------------------------------------------------------------
-- Commands

cmds :: { [Command] }
cmds : cmds_r { reverse $1 }

cmds_r :: { [Command] }
cmds_r : cmd                   { [$1] }
       | cmd block_break       { [$1] }
       | cmds block_break cmd  { $3 : $1 }

cmd :: { Command }
cmd : ident ':' expr { TypeAnn $1 $3 }
    | ident '=' expr { Def $1 $3 }


--------------------------------------------------------------------------------
-- Expressions

expr :: { Expr }
expr : ap  { atoms $1 }

ap :: { NonEmpty Expr }
ap :  ap atom { $2 <| $1 }
   |  atom    { $1 :| [] }

atom :: { Expr }
atom : '(' expr ')'                { $2 }
     | forall tele_cells '->' expr { Pi $2 $4 }
     | ident                       { Var $1 }

--------------------------------------------------------------------------------
-- Identifiers + Cells

cell :: { Cell Expr }
cell : '(' ident ':' expr ')' { Cell $2 $4 }
     | expr                   { Cell Anon $1 }

tele_cells :: { [Cell Expr] }
tele_cells : tele_cells_r { reverse $1 }

tele_cells_r :: { [Cell Expr] }
tele_cells_r : tele_cells_r cell { $2 : $1 }
             | cell              { [$1] }

ident :: { Ident }
ident : name { User $1 }

{
atoms ::  NonEmpty Expr -> Expr
atoms xs = case NE.reverse xs of
  (x :| []) -> x
  (x :| xs) -> App x xs


--------------------------------------------------------------------------------
-- Error Handling

parseError :: [T.Token] -> Lexer a
parseError [] = throwError $ "ParseError: Empty token stream."
parseError (tok:_) = throwError $ fromString $ "ParseError: Unexpected token '" <> show tok <> "'."
}
