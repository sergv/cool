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
{-# OPTIONS_GHC -Wall -fwarn-monomorphism-restriction #-}

-- this module is intended to be imported qualified
module Cool.Lexer.Token where

import Control.Applicative
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Read

import Cool.Types (Id, CoolString(..))
import Cool.PhaseIO


data LexerTokens = LexerTokens FilePath [Token]
                 deriving (Show, Eq, Ord)

instance PhaseIO LexerTokens where
  pipeShow (LexerTokens fname toks) =
    "#name \"" <> T.pack fname <> "\"\n" <> T.intercalate "\n" (map pipeShow toks)
  pipeRead xs
    | (firstLine:rest) <- xss
    , Just firstLine'  <- T.stripPrefix "#name " firstLine
    = LexerTokens (T.unpack $ stripQuotes firstLine') <$> mapM pipeRead rest
    | otherwise = Left $ T.unpack $ "failed to read lexer LexerTokens from " <> xs
    where
      xss = T.lines xs

data Token = Token Line Tok
           deriving (Show, Eq, Ord)

newtype Line = Line { unLine :: Int }
             deriving (Show, Eq, Ord)

instance PhaseIO Line where
  pipeShow line = "#" <> T.pack (show $ unLine line)
  pipeRead xs
    | T.head xs == '#' = Line <$> (readEither $ T.unpack $ T.tail xs)
    | otherwise        = Left $ T.unpack $ "failed to read lexer Line from " <> xs

instance PhaseIO Token where
  pipeShow (Token line t) =
    pipeShow line <> " " <> showTok t
  pipeRead xs =
    let (line, tok) = T.breakOn " " xs
    in Token <$> pipeRead line <*> pipeRead (T.tail tok)
    -- Left $ T.unpack $ "failed to read lexer Token from " <> xs

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
         | TypeId Id
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

instance PhaseIO Tok where
  pipeShow x = showTok x
  pipeRead x = readTok x

showTok :: Tok -> Text
showTok Class          = "CLASS"
showTok Else           = "ELSE"
showTok Fi             = "FI"
showTok If             = "IF"
showTok In             = "IN"
showTok Inherits       = "INHERITS"
showTok IsVoid         = "ISVOID"
showTok Let            = "LET"
showTok Loop           = "LOOP"
showTok Pool           = "POOL"
showTok Then           = "THEN"
showTok While          = "WHILE"
showTok Case           = "CASE"
showTok Esac           = "ESAC"
showTok New            = "NEW"
showTok Of             = "OF"
showTok Not            = "NOT"
showTok (BoolConst b)  = "BOOL_CONST " <> (if b then "true" else "false")
showTok (IntConst x)   = "INT_CONST " <> x
showTok (StrConst str) = "STR_CONST \""  <> pipeShow (getCoolString str) <> "\""
showTok (TypeId x)     = "TYPEID " <> x
showTok (ObjectId x)   = "OBJECTID " <> x
showTok OpenParen      = "'('"
showTok CloseParen     = "')'"
showTok OpenBrace      = "'{'"
showTok CloseBrace     = "'}'"
showTok Plus           = "'+'"
showTok Minus          = "'-'"
showTok Multiply       = "'*'"
showTok Divide         = "'/'"
showTok Tilde          = "'~'"
showTok Lt             = "'<'"
showTok Le             = "LE"
showTok Eq             = "'='"
showTok Colon          = "':'"
showTok Semicolon      = "';'"
showTok Dot            = "'.'"
showTok At             = "'@'"
showTok Comma          = "','"
showTok Assign         = "ASSIGN"
showTok DArrow         = "DARROW"
showTok (Error msg)    = "ERROR \"" <> pipeShow msg <> "\""

readTok :: Text -> Either String Tok
readTok "CLASS"                            = Right $ Class
readTok "ELSE"                             = Right $ Else
readTok "FI"                               = Right $ Fi
readTok "IF"                               = Right $ If
readTok "IN"                               = Right $ In
readTok "INHERITS"                         = Right $ Inherits
readTok "ISVOID"                           = Right $ IsVoid
readTok "LET"                              = Right $ Let
readTok "LOOP"                             = Right $ Loop
readTok "POOL"                             = Right $ Pool
readTok "THEN"                             = Right $ Then
readTok "WHILE"                            = Right $ While
readTok "CASE"                             = Right $ Case
readTok "ESAC"                             = Right $ Esac
readTok "NEW"                              = Right $ New
readTok "OF"                               = Right $ Of
readTok "NOT"                              = Right $ Not
readTok "'('"                              = Right $ OpenParen
readTok "')'"                              = Right $ CloseParen
readTok "'{'"                              = Right $ OpenBrace
readTok "'}'"                              = Right $ CloseBrace
readTok "'+'"                              = Right $ Plus
readTok "'-'"                              = Right $ Minus
readTok "'*'"                              = Right $ Multiply
readTok "'/'"                              = Right $ Divide
readTok "'~'"                              = Right $ Tilde
readTok "'<'"                              = Right $ Lt
readTok "LE"                               = Right $ Le
readTok "'='"                              = Right $ Eq
readTok "':'"                              = Right $ Colon
readTok "';'"                              = Right $ Semicolon
readTok "'.'"                              = Right $ Dot
readTok "'@'"                              = Right $ At
readTok "','"                              = Right $ Comma
readTok "ASSIGN"                           = Right $ Assign
readTok "DARROW"                           = Right $ DArrow
readTok xs
  | Just xs' <- T.stripPrefix "BOOL_CONST " xs =
    case xs' of
      "true"  -> Right $ BoolConst True
      "false" -> Right $ BoolConst False
      _       -> Left $ T.unpack $ "failed to read BoolConst lexer Tok from " <> xs
  | Just xs' <- T.stripPrefix "INT_CONST " xs =
    IntConst <$> pipeRead xs'
  | Just xs' <- T.stripPrefix "STR_CONST " xs =
    StrConst <$> pipeRead (stripQuotes xs')
  | Just xs' <- T.stripPrefix "TYPEID " xs =
    Right $ TypeId xs'
  | Just xs' <- T.stripPrefix "OBJECTID " xs =
    Right $ ObjectId xs'
  | Just xs' <- T.stripPrefix "ERROR " xs =
    Error <$> pipeRead (stripQuotes xs')
  | otherwise = Left $ T.unpack $ "failed to read lexer Tok from " <> xs

stripQuotes :: Text -> Text
stripQuotes = T.dropAround (/= '"')

-- readTok ("BOOL_CONST " : xs)               = undefined -- (BoolConst b)
-- readTok ("INT_CONST " : xs)                = undefined -- (IntConst x)
-- readTok ("STR_CONST \"" : xs)              = undefined -- (StrConst str)
-- readTok ("TYPEID " : xs)                   = undefined -- (TypeId x)
-- readTok ("OBJECTID " : xs)                 = undefined -- (ObjectId x)
-- readTok ("ERROR \"" : xs)                  = undefined -- (Error msg)




