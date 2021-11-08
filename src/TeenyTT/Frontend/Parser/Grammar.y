{
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module TeenyTT.Frontend.Parser.Grammar where

import Control.Monad.Except

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

-- FIXME: Hack
import Data.String

import TeenyTT.Core.Ident
import TeenyTT.Core.Position

import TeenyTT.Frontend.Parser.Token qualified as T
import TeenyTT.Frontend.Parser.Monad

import TeenyTT.Frontend.ConcreteSyntax


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
  name      { T.TokIdent $$ }
  directive { T.TokDirective $$ }
  literal   { T.TokLiteral $$ }
  type      { T.TokKeyword T.Type $$ }
  nat       { T.TokKeyword T.Nat $$ }
  suc       { T.TokKeyword T.Suc $$ }
  '->'      { T.TokSymbol T.Arrow $$ }
  forall    { T.TokSymbol T.ForAll $$ }
  lambda    { T.TokSymbol T.Lambda $$ }
  '('       { T.TokSymbol T.LParen $$ }
  ')'       { T.TokSymbol T.RParen $$ }
  '{!'      { T.TokSymbol T.LBang $$ }
  '!}'      { T.TokSymbol T.RBang $$ }
  '?'       { T.TokSymbol T.Question $$ }
  ':'       { T.TokSymbol T.Colon $$ }
  '='       { T.TokSymbol T.Equal $$ }
  '_'       { T.TokSymbol T.Underscore $$ }
  -- Layout Tokens
  block_open   { T.TokSymbol T.BlockOpen $$ }
  block_break  { T.TokSymbol T.BlockBreak $$ }
  block_close  { T.TokSymbol T.BlockClose $$ }

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
cmd : plain_ident ':' expr  { TypeAnn $1 $3 }
    | plain_ident '=' expr  { Def $1 $3 }
    | directive exprs { Directive (unlocate $1) $2 }

--------------------------------------------------------------------------------
-- Expressions

expr :: { Loc Expr }
expr : app    { atoms $1 }
     | arrow  { $1 }

exprs :: { [Loc Expr] }
exprs : {- empty -} { [] }
      | app         { reverse (NE.toList $1) }
    

arrow :: { Loc Expr }
arrow : forall tele_cells '->' expr { Loc (locate $1 <> locate $4) (Pi $2 $4) }
      | cell '->' expr              { Loc (locate $1 <> locate $3) (Pi [$1] $3) }
      | lambda idents '->' expr     { Loc (locate $1 <> locate $4) (Lam $2 $4) }

app :: { NonEmpty (Loc Expr) }
app :  app atom { $2 <| $1 }
    |  atom     { $1 :| [] }

-- [FIXME: Reed M, 06/11/2021] Add rule for 'Type' without
-- a universe level.
atom :: { Loc Expr }
atom : '(' expr ')'                { $2 }
     | '?'                         { Loc (locate $1) Hole }
     | '{!' expr '!}'              { Loc (locate $1 <> locate $2) (Incomplete $2) }
     | ident                       { Loc (locate $1) (Var (unlocate $1)) }
     | literal                     { Loc (locate $1) (NatLit (natlit $ unlocate $1)) }
     | nat                         { Loc (locate $1) Nat }
     | suc atom                    { Loc (locate $1 <> locate $2) (Suc $2) }
     | type literal                { Loc (locate $1 <> locate $2) (Univ (natlit $ unlocate $2)) }

--------------------------------------------------------------------------------
-- Identifiers + Cells

cell :: { Cell (Loc Expr) }
cell : '(' plain_ident ':' expr ')' { Cell $2 $4 }
     | app %shift             { Cell Anon (atoms $1) }

tele_cells :: { [Cell (Loc Expr)] }
tele_cells : tele_cells_r { reverse $1 }

tele_cells_r :: { [Cell (Loc Expr)] }
tele_cells_r : tele_cells_r cell { $2 : $1 }
             | cell              { [$1] }

idents :: { [Ident] }
idents : idents_r { reverse $1 }

idents_r :: { [Ident] }
idents_r : plain_ident { [$1] }
         | idents_r plain_ident { $2 : $1 }

plain_ident :: { Ident }
plain_ident : ident { unlocate $1 }

ident :: { Loc Ident }
ident : name { Loc (locate $1) (User (unlocate $1)) }
      | '_'  { Loc (locate $1) Anon }

{
atoms :: NonEmpty (Loc Expr) -> (Loc Expr)
atoms xs = case NE.reverse xs of
  (x :| []) -> x
  (x :| xs) -> Loc (locations (x :| xs)) (App x xs)

-- [FIXME: Reed M, 07/11/2021] Handle literals better!
natlit :: T.Literal -> Int
natlit (T.NumLit n) = n


--------------------------------------------------------------------------------
-- Error Handling

failure :: [T.Token] -> Parser a
failure [] = parseError $ "ParseError: Empty token stream."
failure (tok:_) = parseError $ fromString $ "ParseError: Unexpected token '" <> show tok <> "'."
}
