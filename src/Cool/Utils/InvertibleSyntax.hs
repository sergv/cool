{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cool.Utils.InvertibleSyntax where

import qualified Control.Applicative as A
import Control.Category
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

import Cool.Utils.Iso (Iso)
import qualified Cool.Utils.Iso as Iso

import Prelude hiding (id, (.))


class IsoFunctor f where
  (^$^) :: Iso a b -> f a -> f b

class ProductFunctor f where
  (^*^) :: f a -> f b -> f (a, b)

class PureAlternative f where
  (^|^) :: f a -> f a -> f a
  empty :: f a -- parser/printer which always fails

infix 4 ^$^  -- semantic actions
infixl 5 ^*^ -- sequence
infixl 3 ^|^ -- alternative

-- NB both parsers and printers may fail
class (IsoFunctor s, ProductFunctor s, PureAlternative s) => Syntax s where
  -- (^$^) :: Iso a b -> s a -> s b
  -- (^*^) :: s a -> s b -> s (a, b)
  -- (^|^) :: s a -> s a -> s a
  -- empty :: s a

  -- atomic combinators
  -- pure x - parser does not consume input and returns x
  --        - printer discards values equal to x
  -- char' c - parser - recognize given character or fail
  --        - printer - print characters if they're equal to supplied one
  -- token - relates characters with themselves,
  --       - parser returns characters
  --       - printer - prints given characters
  token :: s Char
  char :: Char -> s Char
  char c = Iso.subset (==c) ^$^ token
  pure :: (Eq a) => a -> s a

class InvertibleSyntax a where
  syntax :: (Syntax s) => s a

many :: (Syntax s) => s a -> s [a]
many s =
  Iso.isoNil ^$^ pure () ^|^
  Iso.isoCons ^$^ s ^*^ many s

alpha :: (Syntax s) => s Char
alpha = Iso.subset isAlpha ^$^ token
  where
    isAlpha c = isUpper c || isLower c

upper :: (Syntax s) => s Char
upper = Iso.subset isUpper ^$^ token

lower :: (Syntax s) => s Char
lower = Iso.subset isLower ^$^ token

alphaNumUnderscore :: (Syntax s) => s Char
alphaNumUnderscore = Iso.subset f ^$^ token
  where
    f c = isLower c || isUpper c || isDecimalDigit c || c == '_'

digit :: (Syntax s) => s Char
digit = Iso.subset isDecimalDigit ^$^ token

isDecimalDigit :: Char -> Bool
isDecimalDigit c = '0' <= c && c <= '9'

integer :: (Syntax s, Integral a, Show a, Read a) => s a
integer = Iso.inverse Iso.decimal ^$^ many digit

-- Section 6 of original paper

ignore :: (Eq a) => a -> Iso a ()
ignore x = Iso.inverse $ Iso.element x

char' :: (Syntax s) => Char -> s ()
char' c = ignore c ^$^ char c

-- char' :: (Syntax s) => Char -> s ()
-- char' c = ignore c ^$^ token
--
-- char :: (Syntax s) => Char -> s Char
-- char c = Iso.subset (== c) ^$^ token

text :: (Syntax s) => Text -> s ()
text t | Just (c, cs) <- T.uncons t =
         ignore ((), ()) ^$^ char' c ^*^ text cs
       | otherwise = pure ()

-- only recognizes escaped doublequotes, returns string as-is with
-- doublequotes, but prints doublequotes with backslashes
string :: (Syntax s) => s String
string = pure [] ^|^
         Iso.isoCons ^$^ (Iso.subset (/= '"') ^$^ token) ^*^ string ^|^
         Iso.isoCons ^$^ char '\\' ^*^ (Iso.isoCons ^$^ char '"' ^*^ string)
  -- many (Iso.subset (/= '"') ^$^ token ^|^ char' '\\' *^ char '"')

infixl 5 *^
infixl 5 ^*

(*^) :: (Syntax s) => s () -> s a -> s a
(*^) p q = Iso.inverse Iso.unit . Iso.commute ^$^ p ^*^ q

(^*) :: (Syntax s) => s a -> s () -> s a
(^*) p q = Iso.inverse Iso.unit ^$^ p ^*^ q

between :: (Syntax s) => s () -> s () -> s a -> s a
between p q r = p *^ r ^* q

parens :: (Syntax s) => s a -> s a
parens = between (char' '(') (char' ')')

chainl1 :: (Syntax s) => s a -> s b -> Iso (a, (b, a)) a -> s a
chainl1 arg op f = Iso.foldl f ^$^ arg ^*^ many (op ^*^ arg)

newlines :: (Syntax s) => s ()
newlines = pure () ^|^ Iso.inverse Iso.unit ^$^ text "\n" ^*^ newlines

{-
-- class Contravariant f where
--   contramap :: (a -> b) -> (f b -> f a)

instance (Functor f) => IsoFunctor f where
  (^$^) (Iso f _) x = fmap f x

instance (Contravariant f) => IsoFunctor f where
  (^$^) (Iso _ g) = contramap g
-}


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
--
-- sk :: (Syntax s) => s SK
-- sk = expr
--   where
--     expr = appOrAtom ^$^ atomic ^*^ expr'
--     -- expr' :: s [SK]
--     expr' = Iso.cons ^$^ atomic ^*^ expr' ^|^
--             Iso.nil ^$^ pure ()
--     atomic = s ^$^ char 'S' ^|^
--              k ^$^ char' 'K' ^|^
--              parens expr
--     appOrAtom :: Iso (SK, [SK]) SK
--     appOrAtom = Iso.foldl $ Iso f g
--       where
--         f (x, y)    = Just $ App x y
--         g (App x y) = Just (x, y)
--         g _         = Nothing
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

