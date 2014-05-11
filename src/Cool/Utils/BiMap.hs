----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.BiMap
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Cool.Utils.BiMap
  ( BiMap
  , empty
  , lookupKey
  , lookupVal
  , insert
  , fromList
  )
where

import Data.Map (Map)
import qualified Data.Map as M


data BiMap k v = BiMap (Map k v) (Map v k)
               deriving (Show, Eq, Ord)

empty :: BiMap k v
empty = BiMap M.empty M.empty

lookupKey :: (Ord k) => k -> BiMap k v -> Maybe v
lookupKey k (BiMap d _) = M.lookup k d

lookupVal :: (Ord v) => v -> BiMap k v -> Maybe k
lookupVal v (BiMap _ d') = M.lookup v d'

insert :: (Ord k, Ord v) => k -> v -> BiMap k v -> BiMap k v
insert k v (BiMap d d') = BiMap (M.insert k v d) (M.insert v k d')

fromList :: (Ord k, Ord v) => [(k, v)] -> BiMap k v
fromList kvs = foldr (\(k, v) m -> insert k v m) empty kvs


