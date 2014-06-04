----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Typecheck.Type
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Cool.Typecheck.Type where

import Cool.Utils.RecursionSchemes

data TypeF f = TInt
             | TString
             | TBool
             | TFunc f [f]
             deriving (Show, Eq, Ord)

type Type = Fix TypeF

