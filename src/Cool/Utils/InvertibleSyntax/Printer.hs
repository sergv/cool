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
{-# LANGUAGE ScopedTypeVariables #-}

module Cool.Utils.InvertibleSyntax.Printer
  ( Printer
  , pprint
  )
where

import Control.Applicative
import Control.Monad.Reader
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

type PrintFunc a = a -> Maybe PrintTree

data Printer a = Printer (Int -> (PrintFunc a))
               | Indent Int (Printer a)

collapseIndents :: forall a b. (PrintFunc a -> PrintFunc b) -> Printer a -> Printer b
collapseIndents f p = Printer $ go 0 p
  where
    go :: Int -> Printer a -> Int -> PrintFunc b
    go n (Printer pfunc) = \m -> f $ pfunc (n + m)
    go n (Indent m p)    = go (n + m) p

collapseIndents2 :: forall a b c.
                    (PrintFunc a -> PrintFunc b -> PrintFunc c) ->
                    Printer a -> Printer b -> Printer c
collapseIndents2 f p q = Printer $ go 0 p 0 q
  where
    go :: Int -> Printer a -> Int -> Printer b -> Int -> PrintFunc c
    go n (Printer p)  m (Printer q)  = \k -> f (p $ n + k) (q $ m + k)
    go n (Indent k p) m q            = go (n + k) p m       q
    go n p            m (Indent k q) = go n       p (m + k) q

pprint :: (Printer a) -> a -> Maybe Text
pprint p x = go 0 p
  where
    go n (Printer pp)  = T.pack . collect <$> pp n x
    go n (Indent m pp) = go (n + m) pp

instance IsoFunctor Printer where
  -- iso ^$^ (Printer p) = Printer $ \y -> Iso.unapply iso y >>= p
  iso ^$^ p = collapseIndents f p
    where
      -- f :: PrintFunc -> PrintFunc
      f g = \y -> Iso.unapply iso y >>= g

instance ProductFunctor Printer where
  p ^*^ q = collapseIndents2 (\p q (x, y) -> (:<>:) <$> p x <*> q y) p q

instance PureAlternative Printer where
  -- (Printer p) ^|^ (Printer q) = Printer $ \x -> getFirst $ First (p x) <> First (q x)
  p ^|^ q = collapseIndents2 (\p q x -> getFirst $ First (p x) <> First (q x)) p q
  empty = Printer $ \_ _ -> Nothing

instance Syntax Printer where
  pure x = Printer $ \_ x' -> if x == x' then Just Empty else Nothing
  token = Printer $ \_ c -> Just $ Leaf c
  char c = Printer $ \_ c' -> if c == c' then Just (Leaf c) else Nothing

instance IndentedSyntax Printer where
  indent n p = Indent n p
  indentation = Printer f
    where
      f n () = Just $ foldr (:<>:) Empty $ replicate n (Leaf ' ')

