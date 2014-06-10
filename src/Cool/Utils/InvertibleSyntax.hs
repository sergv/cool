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

class (Syntax s) => IndentedSyntax s where
  -- indent at newlines
  indent :: Int -> s a -> s a
  -- parse piece of indentation at point
  indentation :: s ()

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


