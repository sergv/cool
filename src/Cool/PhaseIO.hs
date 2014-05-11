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

module Cool.PhaseIO where

import Control.Applicative
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Read

-- (\(Right x) -> x) . pipeRead . pipeShow == id
class PhaseIO a where
  pipeShow :: a -> Text
  pipeRead :: Text -> Either String a

instance PhaseIO Integer where
  pipeShow = T.pack . show
  pipeRead = readEither . T.unpack

