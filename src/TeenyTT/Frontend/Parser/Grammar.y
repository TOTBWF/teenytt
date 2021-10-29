{
{-# LANGUAGE NoStrictData #-}
module TeenyTT.Frontend.Parser.Grammar where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.String

import qualified TeenyTT.Frontend.Parser.Token as T

import TeenyTT.Frontend.ConcreteSyntax
import TeenyTT.Core.Ident


}

%name parser expr
%tokentype { T.Token }
%monad { Either String }
%error { parseError }

%token
  name   { T.Identifier $$ }
  '->'   { T.Arrow }
  forall { T.ForAll }
  '('    { T.LParen }
  ':'    { T.Colon }
  ')'    { T.RParen }

%right '->'

%%


expr :: { Expr }
expr
  : ap  { atoms $1 }

ap :: { NonEmpty Expr }
ap
   :  ap atom { $2 <| $1 }
   |  atom    { $1 :| [] }

atom :: { Expr }
atom
     : '(' expr ')'                { $2 }
     | forall tele_cells '->' expr { Pi $2 $4 }
     | ident                       { Var $1 }

--------------------------------------------------------------------------------
-- Identifiers + Cells

cell :: { Cell Expr }
cell
     : '(' ident ':' expr ')' { Cell $2 $4 }
     | expr                   { Cell Anon $1 }

tele_cells :: { [Cell Expr] }
tele_cells
           : tele_cells_r { reverse $1 }

tele_cells_r :: { [Cell Expr] }
tele_cells_r
             : tele_cells_r cell { $2 : $1 }
             | cell              { [$1] }

ident :: { Ident }
ident
      : name { fromString $1 }

{
atoms ::  NonEmpty Expr -> Expr
atoms xs = case NE.reverse xs of
  (x :| []) -> x
  (x :| xs) -> App x xs


--------------------------------------------------------------------------------
-- Error Handling

parseError :: [T.Token] -> Either String a
parseError [] = Left $ "ParseError: Empty token stream."
parseError (tok:_) = Left $ "ParseError: Unexpected token '" <> show tok <> "'."
}
