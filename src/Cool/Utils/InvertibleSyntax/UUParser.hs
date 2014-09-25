----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.InvertibleSyntax.UUParser
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
{-# LANGUAGE TupleSections #-}

module Cool.Utils.InvertibleSyntax.UUParser
  ( UUParser
  , parse
  )
where

import qualified Control.Applicative as A
import Control.Monad
import Control.Monad.Reader
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.ParserCombinators.UU hiding (empty, pure, parse, many)
import Text.ParserCombinators.UU.BasicInstances

import qualified Cool.Utils.Iso as Iso
import Cool.Utils.InvertibleSyntax

-- TODO replace String with Text once ListLike will be updated in Debian
data UUParser a = UUParser (ReaderT Int (P (Str Char String LineColPos)) a)
                | Indent Int (UUParser a)

collapseIndents :: (P (Str Char String LineColPos) a ->
                    P (Str Char String LineColPos) b) ->
                   UUParser a ->
                   UUParser b
collapseIndents f p = UUParser $ ReaderT $ \n -> go 0 p n
  where
    go m (UUParser p) = \n -> f $ runReaderT p (n + m)
    go m (Indent k p) = go (m + k) p

collapseIndents2 :: (P (Str Char String LineColPos) a ->
                     P (Str Char String LineColPos) b ->
                     P (Str Char String LineColPos) c) ->
                    UUParser a ->
                    UUParser b ->
                    UUParser c
collapseIndents2 f p q = UUParser $ ReaderT $ \n -> go 0 p 0 q n
  where
    go m (UUParser p) k (UUParser q) =
      \n -> f (runReaderT p (n + m)) (runReaderT q (n + k))
    go m (Indent n p) k q            = go (m + n) p k       q
    go m p            k (Indent n q) = go m       p (k + n) q

parse :: UUParser a -> Text -> Either String a
parse p xs = go 0 p
  where
    go m (UUParser p) = if null errs
                        then Right res
                        else Left $ show errs
      where
        (res, errs) = parse_h ((,) <$> runReaderT p m <*> pEnd) input
        input       = createStr basicLoc $ T.unpack xs
        basicLoc    = LineColPos 0 0 0
    go m (Indent n p) = go (m + n) p

instance IsoFunctor UUParser where
  iso ^$^ p = collapseIndents f p
    where
      setLen len (P a b c _) = P a b c len
      f p = let P _ _ _ len = p
            in setLen len $
               p >>= \x -> case Iso.apply iso x of
                             Just y  -> return y
                             Nothing -> A.empty

instance ProductFunctor UUParser where
  p ^*^ q = collapseIndents2 (\p' q' -> (,) <$> p' <*> q') p q

instance PureAlternative UUParser where
  p ^|^ q = collapseIndents2 (<|>) p q
  empty = UUParser A.empty

instance Syntax UUParser where
  pure x = UUParser $ A.pure x
  char c = UUParser $ lift $ pSym c
  token  = UUParser $ lift $ pSatisfy (const True) (Insertion err err err)
    where
      err = error "unreachable"

instance IndentedSyntax UUParser where
  indent n p  = Indent n p
  indentation = UUParser $ ReaderT $ \n -> replicateM_ n $ pSym ' '


data Tree = Node Tree Tree
          | Leaf
          deriving (Show, Eq, Ord)

isoNode :: Iso.Iso (Tree, Tree) Tree
isoNode = Iso.mkIso (Just . uncurry Node) g
  where
    g (Node l r) = Just (l, r)
    g _          = Nothing

isoLeaf :: Iso.Iso () Tree
isoLeaf = Iso.mkIso (Just . const Leaf) g
  where
    g Leaf = Just ()
    g _    = Nothing

tree :: (IndentedSyntax s) => s Tree
tree = isoLeaf ^$^ text "leaf" *^ newline ^|^
       isoNode ^$^ text "node" *^ newline *^ indent 2 (indentation *^ tree ^*^ (indentation *^ tree))
  where
    newline = text "\n"

-- data SK = S
--         | K
--         | App SK SK
--         deriving (Show, Eq, Ord)
--
--
-- s :: Iso.Iso () SK
-- s = Iso.mkIso f g
--   where
--     f () = Just S
--     g S  = Just ()
--     g _  = Nothing
--
-- k :: Iso.Iso () SK
-- k = Iso.mkIso f g
--   where
--     f () = Just K
--     g K  = Just ()
--     g _  = Nothing
--
-- app :: Iso.Iso (SK, SK) SK
-- app = Iso.mkIso f g
--   where
--     f (l, r)    = Just $ App l r
--     g (App l r) = Just (l, r)
--     g _         = Nothing
--
--
-- data SK = S
--         | K
--         | App SK SK
--         deriving (Show, Eq, Ord)
--
--
-- s :: Iso () SK
-- s = Iso f g
--   where
--     f () = Just S
--     g S  = Just ()
--     g _  = Nothing
--
-- k :: Iso () SK
-- k = Iso f g
--   where
--     f () = Just K
--     g K  = Just ()
--     g _  = Nothing
--
-- app :: Iso (SK, SK) SK
-- app = Iso f g
--   where
--     f (l, r)    = Just $ App l r
--     g (App l r) = Just (l, r)
--     g _         = Nothing
--
--
--
-- -- sk :: (Syntax s) => s SK
-- -- sk = term ^|^ app ^$^ sk ^*^ sk
-- --   where
-- --     term =
-- --           s ^$^ char' 'S'
-- --       ^|^ k ^$^ char' 'K'
-- --       ^|^ parens sk
--
-- -- expr ::= 'S' | 'K' | '(' expr ')' | expr expr
-- -- =>
-- -- term ::= 'S' | 'K' | '(' expr ')'
-- -- expr ::= term | term term
-- --
-- -- alternative:
-- -- term ::= 'S' | 'K' | '(' expr ')'
-- -- expr ::= term | term expr
-- --
-- --
-- --
-- -- expr   ::= atomic | app
-- -- atomic ::= s | k | '(' expr ')'
-- -- s      ::= 'S'
-- -- k      ::= 'K'
-- -- app    ::= expr atomic
--
-- {-
-- sk :: (Syntax s) => s SK
-- sk = expr
--   where
--     expr = atomic ^|^ appl
--     atomic = s ^$^ char 'S' ^|^
--              k ^$^ char 'K' ^|^
--              parens expr
--     appl = app ^$^ expr ^*^ atomic
-- -}
--
-- -- app    ::= expr atomic
-- -- app    ::= (atomic | app) atomic
-- -- app    ::= (atomic | expr atomic) atomic
-- -- app    ::= (atomic | (atomic | app) atomic) atomic
-- -- app    ::= (atomic | (atomic | expr atomic) atomic) atomic
-- -- app    ::= (atomic | (atomic | (atomic | app) atomic) atomic) atomic
--
--
-- -- expr   ::= expr atomic | atomic
-- -- atomic ::= s | k | '(' expr ')'
-- -- s      ::= 'S'
-- -- k      ::= 'K'
-- -- =>
-- -- expr        ::= atomic | appl_chain
-- -- appl_chain  ::= atomic appl_chain'
-- -- appl_chain' ::= atomic appl_chain' | eps
-- -- atomic      ::= s | k | '(' expr ')'
-- -- s           ::= 'S'
-- -- k           ::= 'K'
-- --
-- -- expr        ::= atomic expr'
-- -- expr'       ::= atomic expr' | eps
-- -- atomic      ::= s | k | '(' expr ')'
-- -- s           ::= 'S'
-- -- k           ::= 'K'
--
-- sk :: (Syntax s) => s SK
-- sk = expr
--   where
--     expr = appOrAtom ^$^ atomic ^*^ expr'
--     -- expr' :: s [SK]
--     expr' = Iso.isoCons ^$^ atomic ^*^ expr' ^|^
--             Iso.isoNil ^$^ pure ()
--     atomic = s ^$^ char' 'S' ^|^
--              k ^$^ char' 'K' ^|^
--              parens expr
--     appOrAtom :: Iso.Iso (SK, [SK]) SK
--     appOrAtom = Iso.foldl $ Iso.mkIso f g
--       where
--         f (x, y)    = Just $ App x y
--         g (App x y) = Just (x, y)
--         g _         = Nothing
--
--
-- -- Need curried partial isomorphisms to do anything substantial!
-- -- isoFold :: Iso (a, b) b -> Iso (a, [a]) b
-- -- isoFold iso = Iso f g
-- --   where
-- --     f (x, xs) = foldl (Iso.apply iso) x xs
--
--
-- -- infixr :->
--
-- -- (a, b, c, d) -> Maybe e
-- -- e            -> Maybe (a, b, c, d)
-- --              ?
-- -- e            -> Maybe (a, b, c)
-- --
-- -- (a, b, c, d)                            -> Maybe e
-- -- e            -> (a -> b -> c -> d -> x) -> Maybe x
-- --
-- --
-- -- a             -> Maybe e
-- -- e -> (a -> x) -> Maybe x
-- --
-- -- a -> b             -> Maybe e
-- -- e -> (a -> b -> x) -> Maybe x
-- --
-- -- a -> b -> c             -> Maybe e
-- -- e -> (a -> b -> c -> x) -> Maybe x -- can handle this inductively as follows
--                                     -- -- Introduce new GADT indexed with
--                                     -- -- finite-element heteregeneous list
--                                     -- -- (e.g. nested tuples). When induction is
--                                     -- -- needed we will cons new items onto
--                                     -- -- this index which will translate into
--                                     -- -- adding more arguments to this
--                                     -- -- continuation function (second argument,
--                                     -- -- here (a -> b -> c -> x)).
--
-- -- A -> A + B | B
-- -- B -> B * C | C
-- -- C -> digit | '(' A ')'
-- --
-- -- A -> A + B | B
-- -- B -> c
-- -- =>
-- -- A  -> B A'
-- -- A' -> '+' A | eps
-- -- B  -> c
--
--
-- main :: IO ()
-- main = do
--   print $ parse sk "SSK"
