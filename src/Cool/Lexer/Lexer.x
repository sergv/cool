{
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Cool.Lexer.Lexer
  ( AlexUserState(tokens)
  , runAlex
  , alexMonadScan
  , module Cool.Lexer.Token
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Cool.Lexer.Token
import Cool.Types

}

%wrapper "monadUserState"

$ws    = [\ \n\f\r\t\v]

@a = [aA]
@b = [bB]
@c = [cC]
@d = [dD]
@e = [eE]
@f = [fF]
@g = [gG]
@h = [hH]
@i = [iI]
@j = [jJ]
@k = [kK]
@l = [lL]
@m = [mM]
@n = [nN]
@o = [oO]
@p = [pP]
@q = [qQ]
@r = [rR]
@s = [sS]
@t = [tT]
@u = [uU]
@v = [vV]
@w = [wW]
@x = [xX]
@y = [yY]
@z = [zZ]

@class    = @c@l@a@s@s
@else     = @e@l@s@e
@fi       = @f@i
@if       = @i@f
@in       = @i@n
@inherits = @i@n@h@e@r@i@t@s
@isvoid   = @i@s@v@o@i@d
@let      = @l@e@t
@loop     = @l@o@o@p
@pool     = @p@o@o@l
@then     = @t@h@e@n
@while    = @w@h@i@l@e
@case     = @c@a@s@e
@esac     = @e@s@a@c
@new      = @n@e@w
@of       = @o@f
@not      = @n@o@t

:-

<0, comment> $ws+       ;
<0> "--".*              ;
<0, comment> "(*"       { \_ _ -> do
                          modifyCommentDepth succ
                          alexSetStartCode comment
                          alexMonadScan
                        }
<comment> "*)"          { \_ _ -> do
                          d <- modifyCommentDepth pred
                          when (d == 0) $ do
                            alexSetStartCode 0
                          alexMonadScan
                        }
<0> "*)"                { \_ _ -> do
                          recordError $ "Unmatched *)"
                          alexMonadScan
                        }
<comment> .             ;

<0> \"                  { begin string }
<string> [^\"]          { \input len -> do
                          recordStringChar $ T.head $ retrieveToken input len
                          alexMonadScan
                        }
<string> \\(. | \n)     { \input len -> do
                          let c = T.head $ T.tail $ retrieveToken input len
                          recordStringChar $ lexDecodeEscapedChar c
                          alexMonadScan
                        }
<string> \n             { \_ _ -> do
                          recordError "Unterminated string constant"
                          resetStrConst
                          alexSetStartCode 0
                          alexMonadScan
                        }
<string> \0             { \_ _ -> do
                          recordError "String contains null character"
                          resetStrConst
                          alexMonadScan
                        }
<string> \"             { \_ _ -> do
                          getStrConst >>= recordToken
                          alexSetStartCode 0
                          alexMonadScan
                        }

-- The keywords of cool are: class, else, false, fi, if, in, inherits, isvoid,
-- let, loop, pool, then, while, case, esac, new, of, not, true.
-- Except for the constants true and false, keywords are case insensitive.

<0> @class              { \_ _ -> recordToken' Class }
<0> @else               { \_ _ -> recordToken' Else }
<0> f@a@l@s@e           { \_ _ -> recordToken' $ BoolConst False }
<0> @fi                 { \_ _ -> recordToken' Fi }
<0> @if                 { \_ _ -> recordToken' If }
<0> @in                 { \_ _ -> recordToken' In }
<0> @inherits           { \_ _ -> recordToken' Inherits }
<0> @isvoid             { \_ _ -> recordToken' IsVoid }
<0> @let                { \_ _ -> recordToken' Let }
<0> @loop               { \_ _ -> recordToken' Loop }
<0> @pool               { \_ _ -> recordToken' Pool }
<0> @then               { \_ _ -> recordToken' Then }
<0> @while              { \_ _ -> recordToken' While }
<0> @case               { \_ _ -> recordToken' Case }
<0> @esac               { \_ _ -> recordToken' Esac }
<0> @new                { \_ _ -> recordToken' New }
<0> @of                 { \_ _ -> recordToken' Of }
<0> @not                { \_ _ -> recordToken' Not }
<0> t@r@u@e             { \_ _ -> recordToken' $ BoolConst True }

<0> [0-9]+              { \input len -> recordToken' $ IntConst $ retrieveToken input len}
<0> [A-Z][A-Za-z0-9_]*  { \input len -> recordToken' $ TypeId $ retrieveToken input len }
<0> [a-z][A-Za-z0-9_]*  { \input len -> recordToken' $ ObjectId $ retrieveToken input len }

<0> "("                 { \_ _ -> recordToken' OpenParen }
<0> ")"                 { \_ _ -> recordToken' CloseParen }
<0> "{"                 { \_ _ -> recordToken' OpenBrace }
<0> "}"                 { \_ _ -> recordToken' CloseBrace }
<0> "+"                 { \_ _ -> recordToken' Plus }
<0> "-"                 { \_ _ -> recordToken' Minus }
<0> "*"                 { \_ _ -> recordToken' Multiply }
<0> "/"                 { \_ _ -> recordToken' Divide }
<0> "~"                 { \_ _ -> recordToken' Tilde }
<0> "<"                 { \_ _ -> recordToken' Lt }
<0> "<="                { \_ _ -> recordToken' Le }
<0> "="                 { \_ _ -> recordToken' Eq }
<0> ":"                 { \_ _ -> recordToken' Colon }
<0> ";"                 { \_ _ -> recordToken' Semicolon }
<0> "."                 { \_ _ -> recordToken' Dot }
<0> "@"                 { \_ _ -> recordToken' At }
<0> ","                 { \_ _ -> recordToken' Comma }
<0> "<-"                { \_ _ -> recordToken' Assign }
<0> "=>"                { \_ _ -> recordToken' DArrow }

<0> .                   { \input _ -> do
                          recordError $ T.pack $ take 1 $ alexInputString input
                          alexMonadScan
                        }
{

-- utility definitions

instance Functor Alex where
  fmap f x = Alex $ \s -> let res = (unAlex x) s
                          in fmap (second f) res

-- actual lexer

data AlexUserState = AlexUserState
                   { commentDepth :: Int
                   -- all tokens gathered so far
                   , tokens       :: [Token]
                   -- string characters in reverse order
                   , stringChars  :: [Char]
                   }
                   deriving (Show, Eq, Ord)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { commentDepth = 0
                                  , tokens       = []
                                  , stringChars  = []
                                  }

alexEOF :: Alex AlexUserState
alexEOF = do
  startCode <- alexGetStartCode
  if | startCode == 0       -> alexGetUserState
     | startCode == comment -> do
       recordToken $ Error "EOF in comment"
       alexGetUserState
     | startCode == string -> do
       recordToken $ Error "EOF in string constant"
       alexGetUserState

retrieveToken :: AlexInput -> Int -> Text
retrieveToken ((AlexPn _pos _line _col), _prevChar, _pendingBytes, str) len =
  T.pack $ take len str

alexInputString :: AlexInput -> String
alexInputString (_, _, _, str) = str

alexLine :: AlexInput -> Line
alexLine ((AlexPn _ line _), _, _, _) = Line line


modifyCommentDepth :: (Int -> Int) -> Alex Int
modifyCommentDepth f = do
  ust <- alexGetUserState
  let newDepth = f $ commentDepth ust
  alexSetUserState $ ust { commentDepth = newDepth }
  return newDepth

recordToken :: Tok -> Alex ()
recordToken tok = do
  line <- alexGetInput >>= return . alexLine
  ust <- alexGetUserState
  alexSetUserState $ ust { tokens = Token line tok: tokens ust }

recordError :: Text -> Alex ()
recordError = recordToken . Error

recordToken' :: Tok -> Alex AlexUserState
recordToken' tok = recordToken tok >> alexMonadScan

resetStrConst :: Alex ()
resetStrConst = do
  ust <- alexGetUserState
  alexSetUserState $ ust { stringChars = [] }

getStrConst :: Alex Tok
getStrConst = do
  ust <- alexGetUserState
  resetStrConst
  let str = T.pack $ reverse $ stringChars ust
  if T.length str <= 1024
  then return $ StrConst $ CoolString str
  else return $ Error "String constant too long"

recordStringChar :: Char -> Alex ()
recordStringChar c = do
  ust <- alexGetUserState
  alexSetUserState $ ust { stringChars = c: stringChars ust }

lexDecodeEscapedChar :: Char -> Char
lexDecodeEscapedChar c = M.findWithDefault c c escapeChars
  where
    escapeChars :: Map Char Char
    escapeChars =  M.fromList [ ('n',  '\n')
                              , ('b',  '\b')
                              , ('f',  '\f')
                              , ('t',  '\t')
                              , ('\n', '\n')
                              ]

}
