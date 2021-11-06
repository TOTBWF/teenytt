{
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module TeenyTT.Frontend.Parser.Grammar where

import Control.Monad.Except

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

-- FIXME: Hack
import Data.String

import TeenyTT.Frontend.Parser.Token qualified as T
import TeenyTT.Frontend.Parser.Monad

import TeenyTT.Frontend.ConcreteSyntax
import TeenyTT.Core.Ident


}

-- We expect 0 shift/reduce conflicts
-- To debug these, run the following command
-- > happy -agc --strict Grammar.y -idetailed-info
--
-- 
%expect 0

%name toplevel toplevel
%name expr expr
%tokentype { T.Token }
%monad { Parser }
%error { failure }

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
cmds_r : cmd block_break       { [$1] }
       | cmd block_break cmds  { $1 : $3 }

cmd :: { Command }
cmd : ident ':' expr { TypeAnn $1 $3 }
    | ident '=' expr { Def $1 $3 }


--------------------------------------------------------------------------------
-- Expressions

expr :: { Expr }
expr : app    { atoms $1 }
     | arrow  { $1 }

arrow :: { Expr }
arrow : forall tele_cells '->' expr { Pi $2 $4 }
      | cell '->' expr              { Pi [$1] $3 }

app :: { NonEmpty Expr }
app :  app atom { $2 <| $1 }
    |  atom     { $1 :| [] }

atom :: { Expr }
atom : '(' expr ')'                { $2 }
     | ident                       { Var $1 }

--------------------------------------------------------------------------------
-- Identifiers + Cells

cell :: { Cell Expr }
cell : '(' ident ':' expr ')' { Cell $2 $4 }
     | app %shift             { Cell Anon (atoms $1) }

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

failure :: [T.Token] -> Parser a
failure [] = parseError $ "ParseError: Empty token stream."
failure (tok:_) = parseError $ fromString $ "ParseError: Unexpected token '" <> show tok <> "'."
}
