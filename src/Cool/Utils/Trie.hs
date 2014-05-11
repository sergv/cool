----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.Trie
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

module Cool.Utils.Trie
  ( Trie
  , empty
  , insert
  , fromList
  , lookup
  -- , lookupShortestMatch
  , lookupLongestMatch
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Prelude hiding (lookup)

data Trie k v = Node        (Map k (Trie k v))
              | ValueNode v (Map k (Trie k v))
              | Empty
              deriving (Show, Eq, Ord)

instance (Pretty k, Pretty v) => Pretty (Trie k v) where
  pretty node =
    case node of
      Empty           -> PP.angles $ PP.text "empty"
      (Node m)        -> PP.angles (PP.text "node") PP.<+> ppSubmaps m
      (ValueNode v m) -> PP.brackets (pretty v) PP.<+> ppSubmaps m
    where
      ppSubmaps m = PP.align $ PP.vsep (map ppPair (M.toList m))
      ppPair (k, v) = pretty k PP.<+> pretty v

-- utils
subMap :: (Ord k) => k -> Trie k v -> Trie k v
subMap k tr =
  subMap' k $
  case tr of
    Empty         -> error "Empty trie contains no submaps"
    Node m        -> m
    ValueNode _ m -> m

subMap' :: (Ord k) => k -> Map k (Trie k v) -> Trie k v
subMap' = M.findWithDefault Empty

-- interface
empty :: Trie k v
empty = Empty

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert []     v Empty           = ValueNode v M.empty
insert []     v (Node m)        = ValueNode v m
insert []     v (ValueNode _ m) = ValueNode v m
insert (k:ks) v Empty           = Node $ M.singleton k (insert ks v Empty)
insert (k:ks) v (Node m)        = Node $ M.insert k (insert ks v $ subMap' k m) m
insert (k:ks) v (ValueNode _ m) = Node $ M.insert k (insert ks v $ subMap' k m) m

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = foldr (\(ks, v) tr -> insert ks v tr) empty

-- Tries to find full key and fails if it can't do so.
lookup :: (Ord k) => [k] -> Trie k v -> Maybe v
lookup []     Empty           = Nothing
lookup []     (Node _)        = Nothing
lookup []     (ValueNode v _) = Just v
lookup _      Empty           = Nothing
lookup (k:ks) node            = lookup ks $ subMap k node

-- -- Stops as soon as node with value is found <=> some prefix of key
-- -- actually correnponds to a value.
-- lookupShortestMatch :: (Ord k) => [k] -> Trie k v -> Maybe v
-- lookupShortestMatch []     Empty           = Nothing
-- lookupShortestMatch []     (Node _)        = Nothing
-- lookupShortestMatch []     (ValueNode v _) = Just v
-- lookupShortestMatch _      Empty           = Nothing
-- lookupShortestMatch (k:ks) (Node m)        = lookupShortestMatch ks $ subMap' k m
-- lookupShortestMatch _      (ValueNode v _) = Just v

-- Tries to find longest prefix of key that results in a value. Returns
-- found value and rest of the key
lookupLongestMatch :: (Ord k) => [k] -> Trie k v -> Maybe (v, [k])
lookupLongestMatch []         Empty           = Nothing
lookupLongestMatch []         (Node _)        = Nothing
lookupLongestMatch []         (ValueNode v _) = Just (v, [])
lookupLongestMatch _          Empty           = Nothing
lookupLongestMatch (k:ks)     (Node m)        = lookupLongestMatch ks $ subMap' k m
lookupLongestMatch ks'@(k:ks) (ValueNode v m) =
  maybe (Just (v, ks')) Just $ lookupLongestMatch ks (subMap' k m)

