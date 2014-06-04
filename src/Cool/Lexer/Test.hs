----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Lexer.Test
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Cool.Lexer.Test where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Cool.Lexer.Token
import Cool.Types
import Cool.PhaseIO


main :: IO ()
main = do
  contents <- TIO.getContents
  let toks = pipeRead contents :: Either String LexerTokens
  print toks


