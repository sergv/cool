----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Lexer.Main
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

module Cool.Lexer.Main where

import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Environment
import System.Exit
import System.IO

import Cool.Lexer.Lexer
import Cool.Lexer.Token
import Cool.PhaseIO
import Cool.Types


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      withFile fname ReadMode $ \handle -> do
        s <- hGetContents handle
        -- print (alexScanTokens s)
        case runAlex s alexMonadScan of
          Left msg -> do
            putStrLn $ "Error occured while lexing: " ++ msg
            exitFailure
          Right res ->
            TIO.putStrLn $ pipeShow $ LexerTokens fname $ reverse $ tokens res
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "usage: " <> prog <> " file"
      exitFailure

