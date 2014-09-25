----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.RecursionSchemes
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cool.Utils.RecursionSchemes where

import Cool.Utils.TH

newtype Fix f = Fix { unFix :: f (Fix f) }

deriveConstructorIsomorphisms ''Fix

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

