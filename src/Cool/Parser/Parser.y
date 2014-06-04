{
module Cool.Parser.Parser where

import Control.Monad.Reader
import Data.List.NonEmpty
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Cool.Types
import qualified Cool.Lexer.Token as Lex
import Cool.Parser.Ast
import Cool.Utils.RecursionSchemes

}


%name parse
%tokentype { Lex.Token }
%monad { Reader FilePath } { (>>=) } { return }

%token CLASS      { Lex.Token _ Lex.Class }
       ELSE       { Lex.Token _ Lex.Else }
       FI         { Lex.Token _ Lex.Fi }
       IF         { Lex.Token _ Lex.If }
       IN         { Lex.Token _ Lex.In }
       INHERITS   { Lex.Token _ Lex.Inherits }
       ISVOID     { Lex.Token _ Lex.IsVoid }
       LET        { Lex.Token _ Lex.Let }
       LOOP       { Lex.Token _ Lex.Loop }
       POOL       { Lex.Token _ Lex.Pool }
       THEN       { Lex.Token _ Lex.Then }
       WHILE      { Lex.Token _ Lex.While }
       CASE       { Lex.Token _ Lex.Case }
       ESAC       { Lex.Token _ Lex.Esac }
       NEW        { Lex.Token _ Lex.New }
       OF         { Lex.Token _ Lex.Of }
       NOT        { Lex.Token _ Lex.Not }
       BOOLCONST  { Lex.Token _ (Lex.BoolConst $$) }
       INTCONST   { Lex.Token _ (Lex.IntConst $$) }
       STRCONST   { Lex.Token _ (Lex.StrConst $$) }
       TYPEID     { Lex.Token _ (Lex.TypeId $$) }
       OBJECTID   { Lex.Token _ (Lex.ObjectId $$) }
       '('        { Lex.Token _ Lex.OpenParen }
       ')'        { Lex.Token _ Lex.CloseParen }
       '{'        { Lex.Token _ Lex.OpenBrace }
       '}'        { Lex.Token _ Lex.CloseBrace }
       '+'        { Lex.Token _ Lex.Plus }
       '-'        { Lex.Token _ Lex.Minus }
       '*'        { Lex.Token _ Lex.Multiply }
       '/'        { Lex.Token _ Lex.Divide }
       '~'        { Lex.Token _ Lex.Tilde }
       '<'        { Lex.Token _ Lex.Lt }
       '<='       { Lex.Token _ Lex.Le }
       '='        { Lex.Token _ Lex.Eq }
       ':'        { Lex.Token _ Lex.Colon }
       ';'        { Lex.Token _ Lex.Semicolon }
       '.'        { Lex.Token _ Lex.Dot }
       '@'        { Lex.Token _ Lex.At }
       ','        { Lex.Token _ Lex.Comma }
       ASSIGN     { Lex.Token _ Lex.Assign }
       DARROW     { Lex.Token _ Lex.DArrow }
       ERROR      { Lex.Token _ (Lex.Error $$) }

%right IN
%nonassoc ASSIGN
%left NOT
%nonassoc '<=' '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%nonassoc '@'
%nonassoc '.'

%%

program         :: { Program Expr }
                : class ';' classes { Program ($1 :| $3) }

classes         :: { [Class Expr] }
                : class ';' classes { $1: $3 }
                | {- empty -}   { [] }

class           :: { Class Expr }
                : CLASS TYPEID opt_parent '{' features '}' {% asks T.pack >>= \file -> return (Class $2 $3 $5 file) }

opt_parent      :: { Maybe TId }
                : INHERITS TYPEID { Just $2 }
                | {- empty -}     { Nothing }

features        :: { [Feature Expr] }
                : feature ';' features { $1: $3 }
                | {- empty -}          { [] }

feature         :: { Feature Expr }
                : OBJECTID method_signature '{' stmt '}' { Method $1 $2 $4 }
                | OBJECTID ':' TYPEID opt_initializer    { Field $1 $3 $4 }

method_signature :: { Signature }
                 : '(' formals ')' ':' TYPEID { Signature $2 $5 }

formals         :: { [(Id, TId)] }
                : formal ',' formals { $1: $3 }
                | formal             { [$1] }
                | {- empty -}        { [] }

formal          :: { (Id, TId) }
                : OBJECTID ':' TYPEID { ($1, $3) }

opt_initializer :: { Maybe Expr }
                : ASSIGN stmt { Just $2 }
                | {- empty -} { Nothing }

stmt            :: { Expr }
                : IF expr THEN expr ELSE expr FI                  { Fix $ If $2 $4 $6 }
                | WHILE expr LOOP expr POOL                       { Fix $ While $2 $4 }
                | '{' expr ';' exprs_block '}'                    { Fix $ Block ($2 :| $4) }
                | LET OBJECTID ':' TYPEID opt_initializer IN expr { Fix $ Let $2 $4 $5 $7 }
                | CASE expr OF case_entry case_body ESAC          { Fix $ Case $2 ($4 :| $5) }
                | expr                                            { $1 }


expr            :: { Expr }
                : ISVOID expr1                                    { Fix $ IsVoid $2 }
                | expr1                                           { $1 }

expr1           :: { Expr }
                : expr1 opt_type '.' OBJECTID '(' exprs_call ')'   { Fix $ Call $1 $2 $4 $6 }
                | OBJECTID '(' exprs_call ')'                     { Fix $ SelfCall $1 $3 }
                | NEW TYPEID                                      { Fix $ New $2 }
                | arith_expr                                      { $1 }

arith_expr      :: { Expr }
                : arith_expr '+' arith_expr  { Fix $ Add $1 $3 }
                | arith_expr '-' arith_expr  { Fix $ Sub $1 $3 }
                | arith_expr '*' arith_expr  { Fix $ Mul $1 $3 }
                | arith_expr '/' arith_expr  { Fix $ Div $1 $3 }
                | '~' arith_expr             { Fix $ Not $2 }
                | arith_expr '<' arith_expr  { Fix $ LessThan $1 $3 }
                | arith_expr '<=' arith_expr { Fix $ Le $1 $3 }
                | arith_expr '=' arith_expr  { Fix $ Eq $1 $3 }
                | '(' stmt ')'               { $2 }
                | NOT arith_expr             { Fix $ Not $2 }
                | OBJECTID                   { Fix $ Identifier $1 }
                -- read will not fail here since tokens come from Alex that
                -- verifies that the're in fact integers
                | INTCONST                                        { Fix $ IntConst (read $ T.unpack $1) $1 }
                | STRCONST                                        { Fix $ StringConst $1 }
                | BOOLCONST                                       { Fix $ BoolConst $1 }

opt_type        :: { Maybe TId }
                : {- empty -} { Nothing }
                | '@' TYPEID  { Just $2 }

exprs_call      :: { [Expr] }
                : {- empty -}          { [] }
                | stmt ',' exprs_call { $1: $3 }

exprs_block     :: { [Expr] }
                : {- empty -}          { [] }
                | stmt ';' exprs_block { $1: $3 }

case_entry      :: { (Id, TId, Expr) }
                : OBJECTID ':' TYPEID DARROW stmt ';' { ($1, $3, $5) }

case_body       :: { [(Id, TId, Expr)] }
                : {- empty -}          { [] }
                | case_entry case_body { $1: $2 }

{

happyError :: [Lex.Token] -> a
happyError toks = error $ "failed" <> show toks

}

