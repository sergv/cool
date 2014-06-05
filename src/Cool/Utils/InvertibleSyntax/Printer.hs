----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.InvertibleSyntax.Printer
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

module Cool.Utils.InvertibleSyntax.Printer
  ( Printer
  , pprint
  )
where

import Control.Applicative
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
-- import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc, text, vsep, nest)
-- import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Cool.Utils.Iso as Iso
import Cool.Utils.InvertibleSyntax

data PrintTree = Empty
               | Leaf Char
               | PrintTree :<>: PrintTree

collect :: PrintTree -> String
collect tree = go tree []
  where
    go Empty      xs = xs
    go (Leaf c)   xs = c: xs
    go (l :<>: r) xs = go l $ go r xs

newtype Printer a = Printer (a -> Maybe PrintTree)

pprint :: (Printer a) -> a -> Maybe Text
pprint (Printer pp) x = (T.pack . collect) <$> pp x

instance IsoFunctor Printer where
  iso ^$^ (Printer p) = Printer $ \y -> Iso.unapply iso y >>= p

instance ProductFunctor Printer where
  (Printer p) ^*^ (Printer q) = Printer $ \(x, y) -> (:<>:) <$> p x <*> q y

instance PureAlternative Printer where
  (Printer p) ^|^ (Printer q) = Printer $ \x -> getFirst $ First (p x) <> First (q x)
  empty = Printer $ const Nothing

instance Syntax Printer where
  pure x = Printer $ \x' -> if x == x' then Just Empty else Nothing
  token = Printer $ \c -> Just $ Leaf c



