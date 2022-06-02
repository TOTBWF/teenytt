{
-- HACK: @happy@ will crash when @-XStrictData@ is enabled, so
-- we explicitly turn it off.
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module TeenyTT.Frontend.Parser.Grammar where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE

import TeenyTT.Base.Ident
import TeenyTT.Base.Location

import TeenyTT.Frontend.Command
import TeenyTT.Frontend.Parser.Token qualified as T
import TeenyTT.Frontend.Parser.Monad

import TeenyTT.Elaborator.ConcreteSyntax

}

-- We expect 0 shift/reduce conflicts
-- To debug these, run the following command
-- > happy -agc --strict Grammar.y -idetailed-info
--
-- 
%expect 0

%name toplevel toplevel
%name term term
%tokentype { T.Token }
%monad { Parser }
%error { failure }

%token
  name      { T.TokIdent $$ }
  directive { T.TokDirective $$ }
  literal   { T.TokLiteral $$ }
  type      { T.TokKeyword (Loc $$ T.Type ) }
  nat       { T.TokKeyword (Loc $$ T.Nat) }
  suc       { T.TokKeyword (Loc $$ T.Suc) }
  fst       { T.TokKeyword (Loc $$ T.Fst) }
  snd       { T.TokKeyword (Loc $$ T.Snd) }
  '->'      { T.TokSymbol (Loc $$ T.Arrow) }
  forall    { T.TokSymbol (Loc $$ T.ForAll) }
  '*'       { T.TokSymbol (Loc $$ T.Times) }
  lambda    { T.TokSymbol (Loc $$ T.Lambda) }
  '('       { T.TokSymbol (Loc $$ T.LParen) }
  ')'       { T.TokSymbol (Loc $$ T.RParen) }
  '{!'      { T.TokSymbol (Loc $$ T.LBang) }
  '!}'      { T.TokSymbol (Loc $$ T.RBang) }
  '?'       { T.TokSymbol (Loc $$ T.Question) }
  ':'       { T.TokSymbol (Loc $$ T.Colon) }
  '='       { T.TokSymbol (Loc $$ T.Equal) }
  '_'       { T.TokSymbol (Loc $$ T.Underscore) }
  -- Layout Tokens
  block_open   { T.TokSymbol (Loc $$ T.BlockOpen) }
  block_break  { T.TokSymbol (Loc $$ T.BlockBreak) }
  block_close  { T.TokSymbol (Loc $$ T.BlockClose) }

%right '->'
%right '*'

%%

--------------------------------------------------------------------------------
-- Top Level

toplevel :: { [Command] }
toplevel : block_open cmds block_close { $2 }

cmds :: { [Command] }
cmds : cmd block_break       { [$1] }
     | cmd block_break cmds  { $1 : $3 }

cmd :: { Command }
cmd : plain_ident ':' term  { Annotate $1 $3 }
    | plain_ident '=' term  { Define $1 $3 }

--------------------------------------------------------------------------------
-- Terms

term :: { Term }
term : app       { applications $1 }
     | operator  { $1 }

app :: { NonEmpty Term }
app : atom app { $1 <| $2 }
    | atom     { $1 :| [] }

operator :: { Term }
operator : forall tele_cells '->' term       { Loc ($1 <> locate $4) (Pi $2 $4) }
         | term '->' term                    { Loc (locate $1 <> locate $3) (Pi [anon $1] $3) }
         | term '*' term                     { Loc (locate $1 <> locate $3) (Sigma [anon $1] $3) }
         | lambda plain_idents '->' term     { Loc ($1 <> locate $4) (Lam $2 $4) }

atom :: { Term }
atom : '(' term ')'    { $2 }
     | '?'             { Loc $1 Hole }
     | '{!' term '!}'  { Loc ($1 <> $3) (Incomplete $2) }
     | name            { Loc (locate $1) (Var (unlocate $1)) }
     | literal         { literal $1 }
     | nat             { Loc $1 Nat }
     | suc atom        { Loc ($1 <> locate $2) (Suc $2) }
     | type            { Loc $1 Univ }
     | fst atom        { Loc ($1 <> locate $2) (Fst $2) }
     | snd atom        { Loc ($1 <> locate $2) (Snd $2) }

--------------------------------------------------------------------------------
-- Identifiers + Cells

cell :: { Cell }
cell : '(' plain_idents ':' term ')' { Cell $2 $4 }

tele_cells :: { [Cell] }
tele_cells : tele_cells_r { reverse $1 }

tele_cells_r :: { [Cell] }
tele_cells_r : tele_cells_r cell { $2 : $1 }
             | cell              { [$1] }

plain_ident :: { Ident }
plain_ident : ident { unlocate $1 }

ident :: { Loc Ident }
ident : name { Loc (locate $1) (User (unlocate $1)) }
      | '_'  { Loc $1 Anon }

plain_idents :: { NonEmpty Ident }
plain_idents : plain_ident plain_idents { $1 <| $2 }
             | plain_ident              { $1 :| [] }

{
anon :: Term -> Cell
anon tm = Cell (Anon :| []) tm

literal :: T.Literal -> Term
literal (T.NumLit (Loc sp n)) = (Loc sp (Lit n))

applications :: NonEmpty Term -> Term
applications xs = case NE.reverse xs of
  (x :| []) -> x
  (x :| xs) -> Loc (locations (x :| xs)) (Ap x xs)

--------------------------------------------------------------------------------
-- Error Handling

failure :: [T.Token] -> Parser a
failure [] = parseError $ EmptyTokenStream
failure (tok : _) = parseError $ UnexpectedToken tok
}