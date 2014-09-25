----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.Types
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Cool.Utils.Types where

import qualified Data.Foldable as F
import Cool.Utils.TH

-- newtype (f :.: g) a = C (f (g a))
--                     deriving (Show, Eq, Ord, Functor, F.Foldable)

newtype K a b = K { unK :: a }
              deriving (Show, Eq, Ord, Functor, F.Foldable)

-- cofree comonad pattern functor
data AnnF f a r = AnnF (f r) a
                deriving (Show, Eq, Ord, Functor, F.Foldable)

deriveConstructorIsomorphisms ''AnnF
