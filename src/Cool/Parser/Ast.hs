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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cool.Parser.Ast where

import Control.Category
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc, vsep, nest)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Cool.PhaseIO
import Cool.Typecheck.Type
import Cool.Types
import Cool.Utils.Iso (Iso)
import qualified Cool.Utils.Iso as Iso
import qualified Cool.Utils.InvertibleSyntax as IS
import Cool.Utils.InvertibleSyntax.Combinators (text, newline, many, manyDelim1, doubleQuotes, parens, between, char')
import Cool.Utils.InvertibleSyntax.Operators
import Cool.Utils.RecursionSchemes
import Cool.Utils.TH
import Cool.Utils.Text
import Cool.Utils.Types

import Prelude hiding ((.), id)

(%^) :: (IndentedSyntax s) => s () -> s a -> s a
(%^) l r = l *^ newline *^ indentation *^ r

infixl 5 %^, ^%^

(^%^) :: (IndentedSyntax s) => s a -> s b -> s (a, b)
(^%^) l r = l ^*^ newline *^ indentation *^ r

betweenManyIndented :: (IndentedSyntax s) => s () -> s () -> s a -> s [a]
betweenManyIndented l r x = between (l *^ newline)
                                    (indentation *^ r)
                                    (many (indentation *^ x ^* newline))

one :: (IndentedSyntax s) => s ()
one = text "#1"

indentationSize :: Int
indentationSize = 2

indent' :: (IndentedSyntax s) => s a -> s a
indent' s = indent indentationSize (indentation *^ s)

oneDoc :: PP.Doc
oneDoc = PP.text "#1"

-- class PipePretty a where
--   pipePretty :: a -> Doc

-- instance (PipePretty e) => PipePretty (Program e) where
--   pipePretty (Program classes) =
--     oneDoc PP.<$> PP.text "_program" PP.<$> PP.indent indentationSize (pipePretty classes)
--
-- instance (PipePretty a) => PipePretty (NonEmpty a) where
--   pipePretty = pipePretty . NE.toList
--
-- instance (PipePretty e) => PipePretty (Class e) where
--   pipePretty (Class {className, parent, features, classFile}) =
--     oneDoc PP.<$>
--     PP.text "_class" PP.<$>
--     PP.indent indentationSize (pipePretty className PP.<$>
--                            PP.text parent' PP.<$>
--                            PP.text (addQuotes classFile) PP.<$>
--                            PP.text "(" PP.<$>
--                            pipePretty features PP.<$>
--                            PP.text ")")
--     where
--       parent' = maybe "Object" getTId parent
--
-- instance (PipePretty a) => PipePretty [a] where
--   pipePretty = vsep . map pipePretty
--
-- instance PipePretty (K Doc b) where
--   pipePretty = unK
--
-- instance PipePretty Id where
--   pipePretty = pipePretty . getId
--
-- instance PipePretty TId where
--   pipePretty = pipePretty . getTId
--
-- instance PipePretty Text where
--   pipePretty = PP.text . pipeShow -- needs quoting
--
-- instance PipePretty Doc where
--   pipePretty = id
--
-- instance PipePretty Bool where
--   pipePretty True  = PP.text "1"
--   pipePretty False = PP.text "0"
--
-- instance PipePretty CoolString where
--   pipePretty = PP.text . addQuotes . pipeShow . getCoolString
--
-- instance (Functor f, PipePretty (f Doc)) => PipePretty (Fix f) where
--   pipePretty = cata pipePretty
--
-- instance (PipePretty e) => PhaseIO (Program e) where
--   pipeShow = T.pack . show . pipePretty
--   pipeRead = error "not implemented"

type Initializer f = Maybe f

data ExprF f = Call f (Maybe TId) Id [f]
             | SelfCall Id [f]
             | Assign Id f
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

deriveConstructorIsomorphisms ''ExprF

-- instance (InvertibleSyntax e) => InvertibleSyntax (ExprF e) where
--   syntax =
--     isoBlock ^$^ one %^ text "_block" *^ newline *^ indent' (many1 syntax) ^|^
--     isoNew   ^$^ one %^ text "_new" *^ newline *^ indent' syntax

instance (InvertibleSyntax e) => InvertibleSyntax (ExprF e) where
  syntax = one %^
    (-- isoWhile       ^$^ text "_while"
     isoAssign      ^$^ text "_assign" *^ indent'' (syntax ^%^ syntax) ^|^
     isoBlock       ^$^ text "_block"  *^ newline
                                       *^ indent indentationSize
                                                 (manyDelim1 newline (indentation *^ syntax)) ^|^
     isoNew         ^$^ text "_new"    *^ indent'' syntax              ^|^
     isoIsVoid      ^$^ text "_isvoid" *^ indent'' syntax              ^|^
     isoAdd         ^$^ text "_plus"   *^ indent'' (syntax ^%^ syntax) ^|^
     isoSub         ^$^ text "_sub"    *^ indent'' (syntax ^%^ syntax) ^|^
     isoMul         ^$^ text "_mul"    *^ indent'' (syntax ^%^ syntax) ^|^
     isoDiv         ^$^ text "_divide" *^ indent'' (syntax ^%^ syntax) ^|^
     isoComplement  ^$^ text "_comp"   *^ indent'' syntax              ^|^
     isoLessThan    ^$^ text "_lt"     *^ indent'' (syntax ^%^ syntax) ^|^
     isoLe          ^$^ text "_leq"    *^ indent'' (syntax ^%^ syntax) ^|^
     isoEq          ^$^ text "_eq"     *^ indent'' (syntax ^%^ syntax) ^|^
     isoNot         ^$^ text "_neg"    *^ indent'' syntax              ^|^
     isoIdentifier  ^$^ text "_object" *^ indent'' syntax              ^|^
     isoIntConst'   ^$^ text "_int"    *^ indent'' (many IS.digit)     ^|^
     isoStringConst ^$^ text "_string" *^ indent'' syntax              ^|^
     isoBoolConst   ^$^ text "_bool"   *^ indent'' (Iso.isoTrue ^$^ text "1" ^|^
                                                    Iso.isoFalse ^$^ text "0"))
    where
      indent'' x = newline *^ indent' x
      -- isoIntConst' :: Iso String (ExprF e)
      isoIntConst' = Iso.mkIso f g
        where
          f txt = Iso.apply (Iso.inverse Iso.decimal) txt >>=
                  Iso.apply isoIntConst . (, T.pack txt)
          g (IntConst _ txt) = Just $ T.unpack txt
          g _                = Nothing

--       alg (IntConst _ txt)  = oneDoc PP.<$> PP.text "_int"    PP.<$> PP.indent indentationSize (pipePretty txt)

-- instance PipePretty (ExprF Doc) where
--   pipePretty = alg
--     where
--       alg :: ExprF Doc -> Doc
--       alg (Block x)         = oneDoc PP.<$> PP.text "_block"  PP.<$> PP.indent indentationSize (pipePretty x)
--       alg (New t)           = oneDoc PP.<$> PP.text "_new"    PP.<$> PP.indent indentationSize (pipePretty t)
--       alg (IsVoid x)        = oneDoc PP.<$> PP.text "_isvoid" PP.<$> PP.indent indentationSize x
--       alg (Add x y)         = oneDoc PP.<$> PP.text "_plus"   PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Sub x y)         = oneDoc PP.<$> PP.text "_sub"    PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Mul x y)         = oneDoc PP.<$> PP.text "_mul"    PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Div x y)         = oneDoc PP.<$> PP.text "_divide" PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Complement x)    = oneDoc PP.<$> PP.text "_comp"   PP.<$> PP.indent indentationSize x
--       alg (LessThan x y)    = oneDoc PP.<$> PP.text "_lt"     PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Le x y)          = oneDoc PP.<$> PP.text "_leq"    PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Eq x y)          = oneDoc PP.<$> PP.text "_eq"     PP.<$> PP.indent indentationSize (x PP.<$> y)
--       alg (Not x)           = oneDoc PP.<$> PP.text "_neg"    PP.<$> PP.indent indentationSize x
--       alg (Identifier x)    = oneDoc PP.<$> PP.text "_object" PP.<$> PP.indent indentationSize (pipePretty x)
--       alg (IntConst _ txt)  = oneDoc PP.<$> PP.text "_int"    PP.<$> PP.indent indentationSize (pipePretty txt)
--       alg (StringConst str) = oneDoc PP.<$> PP.text "_string" PP.<$> PP.indent indentationSize (pipePretty str)
--       alg (BoolConst x)     = oneDoc PP.<$> PP.text "_bool"   PP.<$> PP.indent indentationSize (pipePretty x)
--       alg x                 = PP.text $ "not implemented yet: " <> T.pack (show x)

type OptionalTypeAnnotationF f = AnnF f (Maybe Type)

instance forall e f. (InvertibleSyntax e, InvertibleSyntax (f e)) =>
         InvertibleSyntax (OptionalTypeAnnotationF f e) where
  syntax = isoAnnF ^$^ syntax ^%^ (Iso.isoNothing ^$^ text ": _no_type")

-- instance forall f. (PipePretty (f Doc)) => PipePretty ((OptionalTypeAnnotationF f) Doc) where
--   pipePretty = alg
--     where
--       alg :: (OptionalTypeAnnotationF f) Doc -> Doc
--       alg (AnnF e Nothing) = pipePretty e PP.<$> PP.text ": _no_type"
--       alg _                = error "full type annotations not implemented yet"

type Expr = Fix ExprF

type TypedExpr = Fix (OptionalTypeAnnotationF ExprF)

instance (Functor f, InvertibleSyntax (f (Fix f))) => InvertibleSyntax (Fix f) where
  syntax = isoFix ^$^ syntax

data Signature = Signature [(Id, TId)] TId
               deriving (Show, Eq, Ord)

deriveConstructorIsomorphisms ''Signature

instance InvertibleSyntax Signature where
  syntax = isoSignature ^$^ one
                         %^ many (text "_formal" *^ newline *^
                                  indent' (syntax ^%^ syntax))
                        ^%^ syntax

-- instance PipePretty Signature where
--   pipePretty (Signature formals ret) =
--     oneDoc PP.<$>
--     vsep (map (\(ident, t) -> PP.text "_formal" PP.<$>
--                               PP.indent indentationSize (pipePretty ident PP.<$>
--                                                          pipePretty t))
--               formals) PP.<$>
--     pipePretty ret

data Feature e = Method Id Signature e
               | Field Id TId (Initializer e)
               deriving (Show, Eq, Ord, Functor)

deriveConstructorIsomorphisms ''Feature

instance (InvertibleSyntax e) => InvertibleSyntax (Feature e) where
  syntax =
    isoMethod . Iso.isoTuple3 ^$^ one %^
                                  text "_method" *^ newline *^
                                  indent' (syntax ^%^
                                           syntax ^%^
                                           syntax)
    ^|^
    isoField . Iso.isoTuple3 ^$^ one %^
                                 text "_attr" *^ newline *^
                                 indent' (syntax ^%^
                                          syntax ^%^
                                          (Iso.isoJust ^$^ syntax ^|^
                                           Iso.isoNothing ^$^ one %^ text "_no_expr" %^ text ": _no_type"))

-- instance (PipePretty e) => PipePretty (Feature e) where
--   pipePretty (Method methodName signature expr) =
--     oneDoc PP.<$>
--     PP.text "_method" PP.<$>
--     PP.indent indentationSize (pipePretty methodName PP.<$>
--                            pipePretty signature PP.<$>
--                            pipePretty expr)
--   pipePretty (Field name t initializer) =
--     oneDoc PP.<$>
--     PP.text "_attr" PP.<$>
--     PP.indent indentationSize (pipePretty name PP.<$>
--                                  pipePretty t PP.<$>
--                                   maybe (pipePretty noInit) pipePretty initializer)
--     where
--       noInit :: (OptionalTypeAnnotationF (K Doc)) Doc
--       noInit = AnnF (K (oneDoc PP.<$> PP.text "_no_expr")) Nothing

data Class e = Class
             { className :: TId
             , parent    :: TId
             , features  :: [Feature e]
             , classFile :: Text
             }
             deriving (Show, Eq, Ord, Functor)

deriveConstructorIsomorphisms ''Class

instance (InvertibleSyntax e) => InvertibleSyntax (Class e) where
  syntax =
    one %^
    text "_class" *^ newline *^
    indent' (f ^$^ syntax ^%^
                   syntax ^%^
                   (Iso.commute ^$^ doubleQuotes (Iso.inverse isoQuoted ^$^ syntax)
                                ^%^ betweenManyIndented (char' '(')
                                                        (char' ')')
                                                        syntax))
    where
      -- f :: Iso.Iso ((TId, TId), ([Feature e], Text)) (Class e)
      f = isoClass . Iso.isoTuple4
  -- pipePretty (Class {className, parent, features, classFile}) =
  --   oneDoc PP.<$>
  --   text "_class" PP.<$>
  --   PP.indent indentationSize (pipePretty className PP.<$>
  --                              text parent' PP.<$>
  --                              text (addQuotes classFile) PP.<$>
  --                              text "(" PP.<$>
  --                              pipePretty features PP.<$>
  --                              text ")")
  --   where
  --     parent' = maybe "Object" getTId parent

data Program e = Program (NonEmpty (Class e))
               deriving (Show, Eq, Ord, Functor)

deriveConstructorIsomorphisms ''Program

instance (InvertibleSyntax e) => InvertibleSyntax (Program e) where
  syntax =
    one %^
    text "_program" *^ newline *^
    indent indentationSize
           (isoProgram ^$^ manyDelim1 newline (indentation *^ syntax))

  -- pipePretty (Program classes) =
  --   oneDoc PP.<$> text "_program" PP.<$> PP.indent indentationSize (pipePretty classes)

addTrivialTypeAnnotations :: Expr -> TypedExpr
addTrivialTypeAnnotations = cata alg
  where
    alg :: ExprF TypedExpr -> TypedExpr
    -- alg = Fix . OT . (, Nothing)
    alg x = Fix $ AnnF x Nothing

