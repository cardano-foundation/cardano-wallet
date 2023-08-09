-- | Export 'Typ' definitions to Haskell type definitions.
module Export.Haskell
    ( HsModule (..)
    , prettyPrint

    , haskellFromModule
    ) where

import Prelude

import Export.Haskell.Language
import Typ

import qualified Data.Map as Map
import qualified Language.Haskell.Exts as Hs

{-----------------------------------------------------------------------------
    Haskell
------------------------------------------------------------------------------}
newtype HsModule = HsModule { getHsModule :: Hs.Module Annotation }

prettyPrint :: HsModule -> String
prettyPrint = Hs.prettyPrint . getHsModule

haskellFromModule :: Module -> HsModule
haskellFromModule m =
    HsModule
    $ Hs.Module l (Just moduleHead) languageExtensions imports declarations
  where
    moduleHead =
        Hs.ModuleHead l (Hs.ModuleName l $ moduleName m) Nothing Nothing
    languageExtensions =
        map (Hs.LanguagePragma l . (:[]) . Hs.Ident l)
            [ "DeriveGeneric"
            , "DerivingStrategies"  -- see Note [Deriving]
            , "NoImplicitPrelude"
            ]
    imports =
        map hsImportQualified
            [ "Prelude"
            , "Data.ByteString"
            , "Data.Map"
            , "Data.Set"
            , "Data.Text"
            , "GHC.Generics"
            , "Numeric.Natural"
            ]
    declarations =
        [ declarationFromTyp name typ
        | (name, typ) <- Map.toList (moduleDeclarations m)
        ]

{-----------------------------------------------------------------------------
    Convert Typ to Haskell declaration
------------------------------------------------------------------------------}
declarationFromTyp :: TypName -> Typ -> Hs.Decl Annotation
declarationFromTyp name typ = case typ of
    Record fields ->
        Hs.DataDecl l (Hs.DataType l) Nothing declaredName
            (declareRecord name fields)
            derivingEqOrdGeneric
    Union constructors ->
        Hs.DataDecl l (Hs.DataType l) Nothing declaredName
            (declareUnion name constructors)
            derivingEqOrdGeneric
    _ ->
        Hs.TypeDecl l declaredName (typeFromTyp typ)
  where
    declaredName = Hs.DHead l $ Hs.Ident l name

{- Note [Deriving]

haskell-src-exts version 1.23.1 is not able to represent
the Haskell98 deriving clause (`deriving (Eq,Ord,Generic)`).
Instead, the generated code needs the `DerivingStrategies` extension.

-}
derivingEqOrdGeneric :: [Hs.Deriving Annotation]
derivingEqOrdGeneric =
    map derivingClass
        [ "Prelude.Eq"
        , "Prelude.Ord"
        , "Prelude.Show"
        , "GHC.Generics.Generic"
        ]
  where
    derivingClass c =
        Hs.Deriving l Nothing
        [ Hs.IRule l Nothing Nothing (instanceHead c) ]
    instanceHead = Hs.IHCon l . Hs.UnQual l . Hs.Ident l

declareRecord
    :: TypName
    -> [(FieldName, Typ)]
    -> [Hs.QualConDecl Annotation]
declareRecord name fields =
    [ Hs.QualConDecl l Nothing Nothing
        $ Hs.RecDecl l
            (Hs.Ident l name)
            [ Hs.FieldDecl l [Hs.Ident l n] (typeFromTyp t) | (n,t) <- fields ]
    ]

declareUnion
    :: TypName
    -> [(ConstructorName, Typ)]
    -> [Hs.QualConDecl Annotation]
declareUnion name constructors =
    [ Hs.QualConDecl l Nothing Nothing
        $ Hs.ConDecl l (Hs.Ident l (raiseFirstLetter name)) [typeFromTyp typ]
    | (name,typ) <- constructors
    ]

typeFromTyp :: Typ -> Hs.Type Annotation
typeFromTyp = go
  where
    go Abstract = error "Abstract is not supported by Haskell"
    go (Var "ℤ") = hsType "Prelude.Integer"
    go (Var "ℕ") = hsType "Numeric.Natural.Natural"
    go (Var "Bool") = hsType "Prelude.Bool"
    go (Var "Bytes") = hsType "Data.ByteString.ByteString"
    go (Var "Text") = hsType "Data.Text.Text"
    go (Var "Unit") = hsUnit
    go (Var name) = hsType name
    go (Unary fun a) = fun1 `tyApp` go a
      where
        fun1 = case fun of
            Option -> hsType "Prelude.Maybe"
            Sequence -> hsList
            PowerSet -> hsType "Data.Set.Set"
    go (Binary fun a b) = (fun2 `tyApp` go a) `tyApp` go b
      where
        fun2 = case fun of
            Sum -> hsType "Prelude.Either"
            Product -> hsPair
            PartialFunction -> hsType "Data.Map.Map"
            FiniteSupport -> hsType "Data.Map.Map" -- Fixme: Ambiguous
    go (Record fields) =
        error "Nested Record is not supported by Haskell"
    go (Union constructors) =
        error "Nested Union is not supported by Haskell"
