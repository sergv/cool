----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.Text
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

module Cool.Utils.Text where

import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

addQuotes :: Text -> Text
addQuotes x = "\"" <> x <> "\""

-- addQuotesDoc

stripQuotes :: Text -> Text
stripQuotes = T.dropAround (== '"')

