----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.InvertibleSyntax.Parser
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Cool.Utils.InvertibleSyntax.Parser where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import qualified Cool.Utils.Iso as Iso
import Cool.Utils.InvertibleSyntax

-- Sample parser and printer
newtype Parser a = Parser (Text -> [(a, Text)])

instance IsoFunctor Parser where
  iso ^$^ (Parser p) = Parser $ \s -> [ (y, s')
                                      | (x, s') <- p s
                                      , Just y <- [Iso.apply iso x]
                                      ]

instance ProductFunctor Parser where
  (Parser p) ^*^ (Parser p') = Parser $ \s -> [ ((x, y), s'')
                                              | (x, s')  <- p s
                                              , (y, s'') <- p' s'
                                              ]

instance PureAlternative Parser where
  (Parser p) ^|^ (Parser p') = Parser $ \s -> p s ++ p' s
  empty = Parser $ const []

instance Syntax Parser where
  pure x = Parser $ \s -> [(x, s)]
  token = Parser f
    where
      f t | Just (c, cs) <- T.uncons t = [(c, cs)]
          | otherwise                  = []


parse :: Parser a -> Text -> [a]
parse (Parser p) t = [ x | (x, t') <- p t, T.null t' ]

