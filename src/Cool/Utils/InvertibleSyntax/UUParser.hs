----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.InvertibleSyntax.UUParser
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Cool.Utils.InvertibleSyntax.UUParser where

import qualified Control.Applicative as A
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.ParserCombinators.UU hiding (empty, pure)
import Text.ParserCombinators.UU.BasicInstances

import qualified Cool.Utils.Iso as Iso
import Cool.Utils.InvertibleSyntax

-- TODO replace String with Text once ListLike will be updated
newtype UUParser a = UUParser (P (Str Char String LineColPos) a)

parse :: UUParser a -> Text -> Either String a
parse (UUParser p) xs = if null errs
                        then Right res
                        else Left $ show errs
  where
    (res, errs) = parse_h ((,) <$> p <*> pEnd) input
    input       = createStr basicLoc $ T.unpack xs
    basicLoc    = LineColPos 0 0 0

instance IsoFunctor UUParser where
  iso ^$^ (UUParser p) = UUParser $ addLength 1 $ p >>= \x -> case Iso.apply iso x of
                                                                Just y  -> return y
                                                                Nothing -> A.empty

instance ProductFunctor UUParser where
  UUParser p ^*^ UUParser q = UUParser $ (,) <$> p <*> q

instance PureAlternative UUParser where
  UUParser p ^|^ UUParser q = UUParser $ p <|> q
  empty = UUParser A.empty

instance Syntax UUParser where
  pure x = UUParser $ A.pure x
  char c = UUParser $ pSym c
  token = UUParser $ pSatisfy (const True) (Insertion err err err)
    where
      err = error "unreachable"

