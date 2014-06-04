----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Parser.Main
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Cool.Parser.Main where

import Control.Monad.Reader
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.IO

import Cool.Parser.Ast
import Cool.Parser.Parser
import Cool.Lexer.Token
import Cool.PhaseIO


main :: IO ()
main = do
  s <- getContents
  let lexToks :: Either String LexerTokens
      lexToks = pipeRead $ T.pack s
  either (error) f lexToks
  where
    f :: LexerTokens -> IO ()
    f toks = TIO.putStrLn $
             pipeShow $
             fmap addTrivialTypeAnnotations $
             flip runReader (getFile toks) $
             parse $
             getToks toks
    getFile :: LexerTokens -> FilePath
    getFile (LexerTokens f _) = f
    getToks :: LexerTokens -> [Token]
    getToks (LexerTokens _ toks) = toks
