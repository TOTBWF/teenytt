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
  name      { T.Identifier $$ }
  directive { T.Directive $$ }
  numlit    { T.NumLit $$ }
  type      { T.Type }
  nat       { T.Nat }
  suc       { T.Suc }
  '->'      { T.Arrow }
  forall    { T.ForAll }
  lambda    { T.Lambda }
  '('       { T.LParen }
  ')'       { T.RParen }
  '{!'      { T.LBang }
  '!}'      { T.RBang }
  '?'       { T.Question }
  ':'       { T.Colon }
  '='       { T.Equal }
  '_'       { T.Underscore }
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
cmds : cmd block_break       { [$1] }
     | cmd block_break cmds  { $1 : $3 }

cmd :: { Command }
cmd : ident ':' expr  { TypeAnn $1 $3 }
    | ident '=' expr  { Def $1 $3 }
    | directive exprs { Directive $1 $2 }

--------------------------------------------------------------------------------
-- Expressions

expr :: { Expr }
expr : app    { atoms $1 }
     | arrow  { $1 }

exprs :: { [Expr] }
exprs : {- empty -} { [] }
      | app         { reverse (NE.toList $1) }
    

arrow :: { Expr }
arrow : forall tele_cells '->' expr { Pi $2 $4 }
      | cell '->' expr              { Pi [$1] $3 }
      | lambda idents '->' expr     { Lam $2 $4 }

app :: { NonEmpty Expr }
app :  app atom { $2 <| $1 }
    |  atom     { $1 :| [] }

-- [FIXME: Reed M, 06/11/2021] Add rule for 'Type' without
-- a universe level.
atom :: { Expr }
atom : '(' expr ')'                { $2 }
     | '?'                         { Hole }
     | '{!' expr '!}'              { Incomplete $2 }
     | ident                       { Var $1 }
     | numlit                      { NatLit $1 }
     | nat                         { Nat }
     | suc atom                    { Suc $2 }
     | type numlit                 { Univ $2 }

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

idents :: { [Ident] }
idents : idents_r { reverse $1 }

idents_r :: { [Ident] }
idents_r : ident { [$1] }
         | idents ident { $2 : $1 }

ident :: { Ident }
ident : name { User $1 }
      | '_'  { Anon }

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
