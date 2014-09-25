----------------------------------------------------------------------------
-- |
-- Module      :  Iso
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cool.Utils.Iso
  ( Iso
  , mkIso
  , inverse
  , apply
  , unapply
  , isoNil
  , isoCons
  , isoTrue
  , isoFalse
  -- , isoTextEmpty
  -- , isoTextCons
  , foldl
  , (***)
  , associate
  , commute
  , isoTuple3
  , isoTuple4
  , isoJust
  , isoNothing
  , unit
  , element
  , subset
  , charToInt
  , add
  , subtract
  , pack
  , decimal
  , nonEmptyToList
  )
where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Read
import Text.Show

import qualified Prelude
import Prelude hiding (foldl, (.), id, subtract)

-- partial isomorphism
data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

mkIso :: (a -> Maybe b) -> (b -> Maybe a) -> Iso a b
mkIso = Iso

inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

apply :: Iso a b -> a -> Maybe b
apply (Iso f _) = f

unapply :: Iso a b -> b -> Maybe a
unapply (Iso _ g) = g

-- -- more relaxed than Eq, e.g. strings compared with whitespace ignored
-- -- needed because prettyprinting does not preserve whitespace
-- -- of parsed string
-- class Equivalent a where
--   (===) :: a -> a -> Bool
--
-- -- to load with Hugs
-- class Gen a where
--
-- prop_PartialIsomorphism :: (Equivalent a, Equivalent b, Gen a, Gen b) => a -> b -> Iso a b -> Bool
-- prop_PartialIsomorphism x y (Iso f g) =
--   check f g x && check g f y
--   -- it follows that domains of f and g must be of the same size!
--   where
--     check :: forall a b. (Equivalent a, Equivalent b) => (a -> Maybe b) -> (b -> Maybe a) -> a -> Bool
--     check f g x = (isJust y ==> isJust x') && x === fromJust x'
--       where
--         y  = f x
--         x' = g $ fromJust y
--
-- (==>) :: Bool -> Bool -> Bool
-- p ==> q = not (p && not q) -- <=> not p || q


-- data Iso' fwd bck where
--   E :: (a -> Maybe b)  -> (b -> Maybe a)      -> Iso' a      b
--   L :: (a -> Iso' b c) -> (c -> Maybe (a, b)) -> Iso' (a, b) c
--
-- apply' :: Iso' a b -> a -> Maybe b
-- apply' (L f _) (x, y) = apply' (f x) y
-- apply' (E f _) x      = f x

-- unapply' :: Iso' a b -> b -> Maybe a
-- unapply' ??? x = g

-- deriving instance (Show a) => (Show ((:->) a))

isoNil :: Iso () [a]
isoNil = Iso f g
  where
    f _  = Just []
    g [] = Just ()
    g _  = Nothing

isoCons :: Iso (a, [a]) [a]
isoCons = Iso f g
  where
    f (x, xs) = Just (x: xs)
    g (x: xs) = Just (x, xs)
    g _       = Nothing

isoTrue, isoFalse :: Iso () Bool
isoTrue = Iso (const (Just True)) (\b -> if b then Just () else Nothing)
isoFalse = Iso (const (Just False)) (\b -> if not b then Just () else Nothing)

-- isoTextEmpty :: Iso () Text
-- isoTextEmpty = Iso f g
--   where
--     f _ = Just T.empty
--     g x | T.null x  = Just ()
--         | otherwise = Nothing
--
-- Since Data.Text.cons is O(n) it's too expensive to implement following
-- function - algorithms will become O(n^2)
-- isoTextCons :: Iso (Char, Text) Text
-- isoTextCons

foldl :: Iso (a, b) a -> Iso (a, [b]) a
foldl (Iso f g) = Iso (uncurry f') g'
  where
    f' x []     = Just x
    f' x (y:ys) = f (x, y) >>= \x' -> f' x' ys
    g' x = go x []
    go x ys = case g x of
                Just (x', y) -> go x' $ y:ys
                Nothing      -> Just (x, ys)

instance Category Iso where
  id  = Iso Just Just
  (.) (Iso f g) (Iso f' g') = Iso (f' >=> f) (g >=> g')

-- id :: Iso a a
-- id = Iso Just Just
--
-- (.) :: Iso b c -> Iso a b -> Iso a c
-- (.) (Iso f g) (Iso f' g') = Iso (f' >=> f) (g >=> g')
--   where
--     (>=>) :: forall a b c. (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
--     (>=>) f g x = f x >>= g

(***) :: Iso a b -> Iso c d -> Iso (a, c) (b, d)
(***) f g = Iso f' g'
  where
    pair = (\a b -> (a, b))
    f' (x, y) = pair <$> apply f x <*> apply g y
    g' (x, y) = pair <$> unapply f x <*> unapply g y

associate :: Iso ((a, b), c) (a, (b, c))
associate = Iso f g
  where
    f ((x, y), z) = Just (x, (y, z))
    g (x, (y, z)) = Just ((x, y), z)

commute :: Iso (a, b) (b, a)
commute = Iso f g
  where
    f (x, y) = Just (y, x)
    g (y, x) = Just (x, y)

isoTuple3 :: Iso ((a, b), c) (a, b, c)
isoTuple3 = Iso f g
  where
    f ((x, y), z) = Just (x, y, z)
    g (x, y, z)   = Just ((x, y), z)

isoTuple4 :: Iso ((a, b), (c, d)) (a, b, c, d)
isoTuple4 = Iso f g
  where
    f ((x, y), (z, w)) = Just (x, y, z, w)
    g (x, y, z, w)     = Just ((x, y), (z, w))

isoJust :: Iso a (Maybe a)
isoJust = Iso f g
  where
    f x        = Just $ Just x
    g (Just x) = Just x
    g Nothing  = Nothing

isoNothing :: Iso () (Maybe a)
isoNothing = Iso f g
  where
    f ()       = Just Nothing
    g Nothing  = Just ()
    g (Just _) = Nothing

unit :: Iso a (a, ())
unit = Iso f g
  where
    f x       = Just (x, ())
    g (x, ()) = Just x

element :: (Eq a) => a -> Iso () a
element x = Iso f g
  where
    f _ = Just x
    g x' | x == x'   = Just ()
         | otherwise = Nothing

subset :: (a -> Bool) -> Iso a a
subset p = Iso f f
  where
    f x | p x       = Just x
        | otherwise = Nothing

charToInt :: Iso Char Int
charToInt = Iso (Just . ord) (Just . chr)

add :: (Num a) => a -> Iso a a
add x = Iso (Just . (+x)) (Just . (`Prelude.subtract` (x)))

subtract :: (Num a) => a -> Iso a a
subtract = inverse . add

pack :: Iso String Text
pack = Iso (Just . T.pack) (Just . T.unpack)

decimal :: (Integral a, Show a, Read a) => Iso a String
decimal = Iso f g
  where
    f = Just . show
    g = readMaybe

nonEmptyToList :: Iso (NE.NonEmpty a) [a]
nonEmptyToList = Iso (Just . NE.toList) g
  where
    g [] = Nothing
    g xs = Just $ NE.fromList xs
