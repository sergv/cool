----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Lexer.Token
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fwarn-monomorphism-restriction #-}

-- this module is intended to be imported qualified
module Cool.Lexer.Token where

import Data.Maybe
import Data.Text.Lazy (Text)

import Cool.Types (Id(..), TId(..), CoolString(..))
import Cool.PhaseIO
import qualified Cool.Utils.Iso as Iso
import qualified Cool.Utils.InvertibleSyntax as IS
import Cool.Utils.InvertibleSyntax.Operators
import Cool.Utils.InvertibleSyntax.Combinators
import qualified Cool.Utils.InvertibleSyntax.UUParser as Parser
import qualified Cool.Utils.InvertibleSyntax.Printer as Printer
import Cool.Utils.TH

data Tok = Class
         | Else
         | Fi
         | If
         | In
         | Inherits
         | IsVoid
         | Let
         | Loop
         | Pool
         | Then
         | While
         | Case
         | Esac
         | New
         | Of
         | Not
         | BoolConst Bool
         | IntConst Text
         | StrConst CoolString
         | TypeId TId
         | ObjectId Id
         | OpenParen
         | CloseParen
         | OpenBrace
         | CloseBrace
         | Plus
         | Minus
         | Multiply
         | Divide
         | Tilde
         | Lt
         | Le
         | Eq
         | Colon
         | Semicolon
         | Dot
         | At
         | Comma
         | Assign
         | DArrow
         | Error Text
         -- | Eof
         deriving (Eq, Ord, Show)

deriveConstructorIsomorphisms ''Tok

instance InvertibleSyntax Tok where
  syntax =
    isoClass     ^$^ text "CLASS" ^|^
    isoElse      ^$^ text "ELSE" ^|^
    isoFi        ^$^ text "FI" ^|^
    isoIf        ^$^ text "IF" ^|^
    isoIn        ^$^ text "IN" ^|^
    isoInherits  ^$^ text "INHERITS" ^|^
    isoIsVoid    ^$^ text "ISVOID" ^|^
    isoLet       ^$^ text "LET" ^|^
    isoLoop      ^$^ text "LOOP" ^|^
    isoPool      ^$^ text "POOL" ^|^
    isoThen      ^$^ text "THEN" ^|^
    isoWhile     ^$^ text "WHILE" ^|^
    isoCase      ^$^ text "CASE" ^|^
    isoEsac      ^$^ text "ESAC" ^|^
    isoNew       ^$^ text "NEW" ^|^
    isoOf        ^$^ text "OF" ^|^
    isoNot       ^$^ text "NOT" ^|^
    isoBoolConst ^$^ text "BOOL_CONST "
                 *^ (Iso.isoTrue ^$^ text "true" ^|^
                     Iso.isoFalse ^$^ text "false") ^|^
    isoIntConst  ^$^ text "INT_CONST " *^ (Iso.pack ^$^ IS.many digit) ^|^
    isoStrConst  ^$^ text "STR_CONST " *^ syntax ^|^
    isoTypeId    ^$^ text "TYPEID " *^ syntax ^|^
    isoObjectId  ^$^ text "OBJECTID " *^ syntax ^|^
    isoOpenParen  ^$^ text "'('" ^|^
    isoCloseParen ^$^ text "')'" ^|^
    isoOpenBrace  ^$^ text "'{'" ^|^
    isoCloseBrace ^$^ text "'}'" ^|^
    isoPlus       ^$^ text "'+'" ^|^
    isoMinus      ^$^ text "'-'" ^|^
    isoMultiply   ^$^ text "'*'" ^|^
    isoDivide     ^$^ text "'/'" ^|^
    isoTilde      ^$^ text "'~'" ^|^
    isoLt         ^$^ text "'<'" ^|^
    isoLe         ^$^ text "LE" ^|^
    isoEq         ^$^ text "'='" ^|^
    isoColon      ^$^ text "':'" ^|^
    isoSemicolon  ^$^ text "';'" ^|^
    isoDot        ^$^ text "'.'" ^|^
    isoAt         ^$^ text "'@'" ^|^
    isoComma      ^$^ text "','" ^|^
    isoAssign     ^$^ text "ASSIGN" ^|^
    isoDArrow     ^$^ text "DARROW" ^|^
    isoError      ^$^ text "ERROR " *^ between (char' '"')
                                               (char' '"')
                                               (Iso.inverse isoQuoted ^$^ syntax)

instance PhaseIO Tok where
  pipeShow x = fromMaybe (error "Tok printing failed") $ Printer.pprint syntax x
  pipeRead x = Parser.parse syntax x


newtype Line = Line { unLine :: Int }
             deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''Line

instance InvertibleSyntax Line where
  syntax = isoLine ^$^ text "#" *^ IS.integer

instance PhaseIO Line where
  pipeShow x = fromMaybe (error "Line printing failed") $ Printer.pprint syntax x
  pipeRead x = Parser.parse syntax x

data Token = Token Line Tok
           deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''Token

instance InvertibleSyntax Token where
  syntax = isoToken ^$^ syntax ^* text " " ^*^ syntax

instance PhaseIO Token where
  pipeShow x = fromMaybe (error "Token printing failed") $ Printer.pprint syntax x
  pipeRead x = Parser.parse syntax x


data LexerTokens = LexerTokens FilePath [Token]
                 deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''LexerTokens

instance InvertibleSyntax LexerTokens where
  syntax = isoLexerTokens ^$^ text "#name \"" *^ string ^* text "\"\n"
                          ^*^ IS.many (syntax ^* text "\n")
                          ^* IS.newlines

instance PhaseIO LexerTokens where
  pipeShow x = fromMaybe (error "LexerTokens printing failed") $ Printer.pprint syntax x
  pipeRead x = Parser.parse syntax x
