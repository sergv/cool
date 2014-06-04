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

module Cool.Types
  ( Id(..)
  , TId(..)
  , CoolString(..)
  )
where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Cool.PhaseIO
-- import Cool.Utils.BiMap (BiMap)
-- import qualified Cool.Utils.BiMap as BM
import Cool.Utils.Trie (Trie)
import qualified Cool.Utils.Trie as TR


newtype Id = Id { getId :: Text }
           deriving (Show, Eq, Ord)

newtype TId = TId { getTId :: Text }
            deriving (Show, Eq, Ord)

newtype CoolString = CoolString { getCoolString :: Text }
                   deriving (Show, Eq, Ord)

instance PhaseIO CoolString where
  pipeShow = pipeShow . getCoolString
  pipeRead = (CoolString <$>) . pipeRead

instance PhaseIO Text where
  pipeShow = escape
  pipeRead = Right . unescape


escape :: Text -> Text
escape = T.foldr encodeEscapedChar T.empty

unescape :: Text -> Text
unescape = T.pack . go . T.unpack
  where
    go :: String -> String
    go []         = []
    go xs'@(x:xs) = case TR.lookupLongestMatch xs' decodeSequences of
                      Just (c, xs'') -> c: go xs''
                      Nothing        -> x: go xs


encodeEscapedChar :: Char -> Text -> Text
encodeEscapedChar c xs =
  maybe (T.cons c xs) (<> xs) $
  M.lookup c charsToEscape
  -- M.findWithDefault (T.singleton c) c charsToEscape <> xs

-- decodeEscapedChar :: Text -> Char
-- decodeEscapedChar xs =
--   fromMaybe  $ BM.lookupVal xs charsToEscape

charsToEscape :: Map Char Text
charsToEscape = M.fromList [ ('"',   "\\\"")
                           , ('\\',  "\\\\")
                           , ('\2',  "\\002")
                           , ('\3',  "\\003")
                           , ('\4',  "\\004")
                           , ('\5',  "\\005")
                           , ('\6',  "\\006")
                           , ('\7',  "\\007")
                           , ('\b',  "\\b")
                           , ('\t',  "\\t")
                           , ('\n',  "\\n") -- \\012
                           , ('\11', "\\013")
                           , ('\f',  "\\f") -- \\014
                           , ('\r',  "\\015")
                           , ('\16', "\\016")
                           , ('\15', "\\017")
                           , ('\16', "\\020")
                           , ('\17', "\\021")
                           , ('\18', "\\022")
                           , ('\19', "\\023")
                           , ('\20', "\\024")
                           , ('\21', "\\025")
                           , ('\22', "\\026")
                           , ('\23', "\\027")
                           , ('\24', "\\030")
                           , ('\25', "\\031")
                           , ('\26', "\\032")
                           , ('\27', "\\033")
                           ]

decodeSequences :: Trie Char Char
decodeSequences =
  TR.fromList $ map (\(c, txt) -> (T.unpack txt, c)) $ M.toList charsToEscape


