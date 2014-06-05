----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Types
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
{-# LANGUAGE TemplateHaskell #-}

module Cool.Types
  ( Id(..)
  , TId(..)
  , CoolString(..)
  )
where

import Control.Applicative
import Control.Category
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Cool.PhaseIO
import Cool.Utils.TH
import Cool.Utils.Text
import Cool.Utils.Iso (Iso)
import qualified Cool.Utils.Iso as Iso
import qualified Cool.Utils.InvertibleSyntax as IS
import Cool.Utils.InvertibleSyntax.Operators
import Cool.Utils.InvertibleSyntax.Combinators
import Cool.Utils.InvertibleSyntax.Parser
import Cool.Utils.InvertibleSyntax.Printer

import Prelude hiding ((.))

newtype Id = Id { getId :: Text }
           deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''Id

instance InvertibleSyntax Id where
  syntax = isoId . Iso.pack . Iso.isoCons ^$^ (IS.lower ^*^ IS.many IS.alphaNumUnderscore)

newtype TId = TId { getTId :: Text }
            deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''TId

instance InvertibleSyntax TId where
  syntax = isoTId . Iso.pack . Iso.isoCons ^$^ (IS.upper ^*^ IS.many IS.alphaNumUnderscore)

-- does not carry double quotes around,
-- prints and parses as quoted but does not contain
-- quoting backslashes inside
newtype CoolString = CoolString { getCoolString :: Text }
                   deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''CoolString

instance InvertibleSyntax CoolString where
  syntax =
    isoCoolString ^$^ between (char' '"')
                              (char' '"')
                              (Iso.inverse isoQuoted ^$^ syntax)



instance PhaseIO CoolString where
  pipeShow = addQuotes . pipeShow . getCoolString
  pipeRead = (CoolString <$>) . pipeRead . stripQuotes


