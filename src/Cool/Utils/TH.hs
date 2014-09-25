----------------------------------------------------------------------------
-- |
-- Module      :  Cool.Utils.TH
-- Copyright   :  (c) Sergey Vinokurov 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Cool.Utils.TH where

import Control.Applicative
import Data.Char
import Data.List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Language.Haskell.TH

import Cool.Utils.Iso (Iso)
import qualified Cool.Utils.Iso as Iso

-- requires type name
deriveConstructorIsomorphisms :: Name -> Q [Dec]
deriveConstructorIsomorphisms typeName = do
  TyConI dataDef <- reify typeName
  let (datatypeType, tvars, constructors) =
        case dataDef of
          DataD _ctx typeName tvars cs _csNames ->
            (mkDatatypeType typeName tvars, tvars, cs)
          NewtypeD _ctx typeName tvars c _ ->
            (mkDatatypeType typeName tvars, tvars, [c])
      conCount = length constructors
  concat <$> mapM (constructIso conCount datatypeType tvars) constructors
  where
    mkDatatypeType :: Name -> [TyVarBndr] -> TypeQ
    mkDatatypeType typeName tvars =
      foldl' appT (conT typeName) $ map (varT . tvarName) tvars
      where
        tvarName :: TyVarBndr -> Name
        tvarName (PlainTV name)    = name
        tvarName (KindedTV name _) = name

    expandCon :: Con -> (Name, [Type])
    expandCon (NormalC name fieldTypes) = (name, map snd fieldTypes)
    expandCon (RecC name fieldTypes) = (name, map (\(_, _, t) ->t) fieldTypes)

    constructIso :: Int -> TypeQ -> [TyVarBndr] -> Con -> Q [Dec]
    constructIso conCount datatypeType datatypeVars con = do
      let (name, fieldTypes) = expandCon con
      vs <- mapM (const $ newName "x") fieldTypes
      let isoName  = mkName $ "iso" ++ nameBase name
          projType = case fieldTypes of
                       [t] -> return t
                       _   -> foldl' appT
                                     (tupleT (length fieldTypes))
                                     (map return fieldTypes)
          projPat  = conP name $ map varP vs
          projBody = tupE $ map varE vs
          injPat   = tupP $ map varP vs
          injBody  = foldl' appE (conE name) $ map varE vs
      injName  <- newName "inj"
      projName <- newName "proj"
      sig <- sigD isoName $
             forallT datatypeVars (cxt []) $
             [t| Iso |] `appT` projType `appT` datatypeType -- [t| Iso $(projType) $(datatypeType) |]
      fun <- funD isoName
                  [ clause []
                           (normalB [e| Iso.mkIso $(varE injName) $(varE projName) |])
                           [ funD injName
                                  [ clause [injPat]
                                           (normalB [| Just $ $(injBody) |])
                                           []]
                           , funD projName
                                  ([ clause [projPat]
                                            (normalB [| Just $ $(projBody)|])
                                            []
                                   ] ++
                                   if conCount > 1
                                   then [ clause [wildP]
                                                 (normalB [| Nothing |])
                                                 []
                                        ]
                                   else []
                                  )
                           ]
                  ]
      return [sig, fun]


