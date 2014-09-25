----------------------------------------------------------------------------
-- |
-- Module      :  Cool.PhaseIO
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

-- Module defining functions for pipe communication across
-- different stages of pipeline

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cool.PhaseIO
  ( PhaseIO(..)
  , Quoted
  , getQuoted
  , isoQuoted
  -- TODO remove escape from export list
  , escape
  )
where

import Control.Applicative
import Control.Category
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Read

import qualified Cool.Utils.InvertibleSyntax as IS
import Cool.Utils.InvertibleSyntax.Operators
import Cool.Utils.InvertibleSyntax.Combinators
import qualified Cool.Utils.InvertibleSyntax.UUParser as Parser
import qualified Cool.Utils.InvertibleSyntax.Printer as Printer
import Cool.Utils.Iso (Iso)
import qualified Cool.Utils.Iso as Iso
import Cool.Utils.TH
import Cool.Utils.Trie (Trie)
import qualified Cool.Utils.Trie as TR

import Prelude hiding ((.))

-- (\(Right x) -> x) . pipeRead . pipeShow == id
class PhaseIO a where
  pipeShow :: a -> Text
  pipeRead :: Text -> Either String a


instance (InvertibleSyntax a) => PhaseIO a where
  pipeShow x = fromMaybe (error "Printing failed") $ Printer.pprint syntax x
  pipeRead x = Parser.parse syntax x

instance PhaseIO Integer where
  pipeShow = T.pack . show
  pipeRead = readEither . T.unpack


newtype Quoted = QuotedText { getQuoted :: Text }
               deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''Quoted

instance InvertibleSyntax Quoted where
  syntax = isoQuotedText . Iso.pack ^$^ string

isoQuoted :: Iso Text Quoted
isoQuoted = Iso.mkIso (Just . QuotedText . escape) (Just . unescape . getQuoted)

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
                           , ('\1',  "\\001")
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

instance PhaseIO Text where
  pipeShow = escape
  pipeRead = Right . unescape
