----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Parser.Ast
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cool.Parser.Ast where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc, text, vsep, nest)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Cool.PhaseIO
import Cool.Typecheck.Type
import Cool.Types
import Cool.Utils.RecursionSchemes
import Cool.Utils.Types

data Program e = Program (NonEmpty (Class e))
               deriving (Show, Eq, Ord, Functor)

data Class e = Class
             { className :: TId
             , parent    :: Maybe TId
             , features  :: [Feature e]
             , classFile :: Text
             }
             deriving (Show, Eq, Ord, Functor)

type Initializer f = Maybe f

data Feature e = Method Id Signature e
               | Field Id TId (Initializer e)
               deriving (Show, Eq, Ord, Functor)

data Signature = Signature [(Id, TId)] TId
               deriving (Show, Eq, Ord)

data ExprF f = Call f (Maybe TId) Id [f]
             | SelfCall Id [f]
             | If f f f
             | While f f
             | Block (NonEmpty f)
             | Let Id TId (Initializer f) f
             | Case f (NonEmpty (Id, TId, f))
             | New TId
             | IsVoid f
             | Add f f
             | Sub f f
             | Mul f f
             | Div f f
             | Complement f
             | LessThan f f
             | Le f f
             | Eq f f
             | Not f
             | Identifier Id
             | IntConst Int Text
             | StringConst CoolString
             | BoolConst Bool
             deriving (Show, Eq, Ord, Functor, F.Foldable)

type OptionalTypeAnnotationF f = AnnF f (Maybe Type)

type Expr = Fix ExprF

type TypedExpr = Fix (OptionalTypeAnnotationF ExprF)

addTrivialTypeAnnotations :: Expr -> TypedExpr
addTrivialTypeAnnotations = cata alg
  where
    alg :: ExprF TypedExpr -> TypedExpr
    -- alg = Fix . OT . (, Nothing)
    alg x = Fix $ AnnF x Nothing


one :: PP.Doc
one = text "#1"

indent :: Int
indent = 2

class PipePretty a where
  pipePretty :: a -> Doc

instance (PipePretty e) => PipePretty (Program e) where
  pipePretty (Program classes) =
    one PP.<$> text "_program" PP.<$> PP.indent indent (pipePretty classes)

instance (PipePretty a) => PipePretty (NonEmpty a) where
  pipePretty = pipePretty . NE.toList

instance (PipePretty e) => PipePretty (Class e) where
  pipePretty (Class {className, parent, features, classFile}) =
    one PP.<$>
    text "_class" PP.<$>
    PP.indent indent (pipePretty className PP.<$>
                      text parent' PP.<$>
                      text classFile PP.<$>
                      text "(" PP.<$>
                      pipePretty features PP.<$>
                      text ")")
    where
      parent' = maybe "Object" getTId parent

instance (PipePretty a) => PipePretty [a] where
  pipePretty = vsep . map pipePretty

instance (PipePretty e) => PipePretty (Feature e) where
  pipePretty (Method methodName signature expr) =
    one PP.<$>
    text "_method" PP.<$>
    PP.indent indent (pipePretty methodName PP.<$>
                      pipePretty signature PP.<$>
                      pipePretty expr)
  pipePretty (Field name t initializer) =
    one PP.<$>
    text "_attr" PP.<$>
    PP.indent indent (pipePretty name PP.<$>
                      pipePretty t PP.<$>
                       maybe (pipePretty noInit) pipePretty initializer)
    where
      noInit :: (OptionalTypeAnnotationF (K Doc)) Doc
      noInit = AnnF (K (one PP.<$> text "_no_expr")) Nothing

instance PipePretty (K Doc b) where
  pipePretty = unK

instance PipePretty Signature where
  pipePretty (Signature formals ret) =
    one PP.<$>
    vsep (map (\(ident, t) -> text "_formal" PP.<$>
                              PP.indent indent (pipePretty ident PP.<$>
                                                pipePretty t))
              formals) PP.<$>
    pipePretty ret

instance PipePretty Id where
  pipePretty = pipePretty . getId

instance PipePretty TId where
  pipePretty = pipePretty . getTId

instance PipePretty Text where
  pipePretty = text

instance PipePretty (ExprF Doc) where
  pipePretty = alg
    where
      alg :: ExprF Doc -> Doc
      alg (Block x)        = one PP.<$> text "_block"  PP.<$> PP.indent indent x
      alg (Add x y)        = one PP.<$> text "_plus"   PP.<$> PP.indent indent (x PP.<$> y)
      alg (Sub x y)        = one PP.<$> text "_sub"    PP.<$> PP.indent indent (x PP.<$> y)
      alg (Mul x y)        = one PP.<$> text "_mul"    PP.<$> PP.indent indent (x PP.<$> y)
      alg (Div x y)        = one PP.<$> text "_divide" PP.<$> PP.indent indent (x PP.<$> y)
      alg (Complement x)   = one PP.<$> text "_comp"   PP.<$> PP.indent indent (x)
      alg (LessThan x y)   = one PP.<$> text "_lt"     PP.<$> PP.indent indent (x PP.<$> y)
      alg (Le x y)         = one PP.<$> text "_leq"    PP.<$> PP.indent indent (x PP.<$> y)
      alg (Eq x y)         = one PP.<$> text "_eq"     PP.<$> PP.indent indent (x PP.<$> y)
      alg (Not x)          = one PP.<$> text "_neg"    PP.<$> PP.indent indent x
      alg (Identifier x)   = one PP.<$> text "_object" PP.<$> PP.indent indent (pipePretty x)
      alg (IntConst _ txt) = one PP.<$> text "_int"    PP.<$> PP.indent indent (pipePretty txt)
      alg x = text $ "not implemented yet: " <> T.pack (show x)

instance forall f. (PipePretty (f Doc)) => PipePretty ((OptionalTypeAnnotationF f) Doc) where
  pipePretty = alg
    where
      alg :: (OptionalTypeAnnotationF f) Doc -> Doc
      alg (AnnF e Nothing) = pipePretty e PP.<$> text ": _no_type"
      alg _                = error "full type annotations not implemented yet"

instance (Functor f, PipePretty (f Doc)) => PipePretty (Fix f) where
  pipePretty = cata pipePretty

instance (PipePretty e) => PhaseIO (Program e) where
  pipeShow = T.pack . show . pipePretty
  pipeRead = error "not implemented"



