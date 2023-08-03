{-# LANGUAGE OverloadedStrings #-}

-- | Export type definitions to OpenAPI JSON schemas.
--
-- https://www.openapis.org
module Export.OpenAPI
    ( OpenAPISchema (..)

    , schemaFromModule
    , supportsJSON
    , convertToJSON
    ) where

import Prelude

import Data.Aeson
    ( (.=) )
import Data.Text
    ( Text )
import Module
    ( resolveVars )
import Typ

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS.Key
import qualified Data.Aeson.Types as JS
import qualified Data.Map as Map
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    OpenAPI
------------------------------------------------------------------------------}
newtype OpenAPISchema = OpenAPISchema { getOpenAPISchema :: JS.Value }

-- | Export 
--
-- Assumes that the argument satisfies 'supportsJSON'.
schemaFromModule :: Module -> OpenAPISchema
schemaFromModule m =
    OpenAPISchema
        $ wrapSchemasInHeader (T.pack $ moduleName m)
            [ (T.pack name, schemaFromTyp typ)
            | (name, typ) <- Map.toList declarations
            ]
  where
    declarations = moduleDeclarations m

-- | Test whether a 'Module' only uses types supported by JSON.
--
-- JSON does not support finite maps such as @↦@, @↦0@, @→∗@.
supportsJSON :: Module -> Bool
supportsJSON =
    and . Map.map isSupportedTyp . moduleDeclarations
  where
    isSupportedTyp = everything (&&) isSupported
    isSupported (Binary fun _ _) =
        not (fun `elem` [PartialFunction, FiniteSupport])
    isSupported _ = True

-- | Convert 'Typ' definitions to JSON.
--
-- The result satisfied 'supportsJSON'.
--
-- Note: For clarity, it is recommended to declare new types that
-- explicitly supports JSON, and to provide evidence that
-- these types elaborate the argument.
convertToJSON :: Declarations -> Declarations
convertToJSON declarations = Map.map (jsonify declarations) declarations

{-----------------------------------------------------------------------------
    Convert Typ to JSON schema
------------------------------------------------------------------------------}
wrapSchemasInHeader :: Text -> [(Text, JS.Value)] -> JS.Value
wrapSchemasInHeader title xs =
    object
        [ "openapi" .= s "3.0.3"
        , "info" .= object
            [ "title" .= s title
            , "version" .= s "1"
            ]
        , "components" .= object
            [ "schemas" .= object
                [ key name .= x
                | (name,x) <- xs
                ]
            ]
        , "paths" .= object []
        ]

schemaFromTyp :: Typ -> JS.Value
schemaFromTyp = go
  where
    go Abstract = object
        [ "type" .= s "object" ]
    go (Var "ℤ") = object
        [ "type" .= s "integer" ]
    go (Var "ℕ") = object
        [ "type" .= s "integer"
        , "minimum" .= JS.toJSON (0 :: Int)
        ]
    go (Var "Bool") = object
        [ "type" .= s "boolean"
        ]
    go (Var "Bytes") = object
        [ "type" .= s "string"
        , "format" .= s "base16"
        ]
    go (Var "Text") = object
        [ "type" .= s "string" ]
    go (Var "Unit") = object
        [ "type" .= s "null"
        ]
    go (Var name) = object
        [ "$ref" .= s (T.pack $ "#/components/schemas/" <> name) ]
    go (Unary Option a) = object
        [ "type" .= s "object"
        , "properties" .= object [ "0" .= go a ]
        ]
    go (Unary Sequence a) = object
        [ "type" .= s "array"
        , "items" .= go a
        ]
    go (Unary PowerSet a) =
        go (Unary Sequence a)
    go (Binary Sum a b) =
        schemaFromUnion [("0",a), ("1",b)]
    go (Binary Product a b) = object
        [ "type" .= s "object"
        , "properties" .= object [ "0" .= go a, "1" .= go b ]
        , "required" .= array [ s "0", s "1"]
        ]
    go (Binary PartialFunction _ _) =
        error "PartialFunction is not supported by JSON schema"
    go (Binary FiniteSupport a b) =
        error "FiniteSupport is not supported by JSON schema"
    go (Record fields) =
        schemaFromRecord fields
    go (Union constructors) =
        schemaFromUnion constructors

-- | Map a record type to a JSON schema.
--
-- Field that are option types (@?@) will be mapped to optional fields.
schemaFromRecord :: [(FieldName, Typ)] -> JS.Value
schemaFromRecord fields =
    object
        [ "type" .= s "object"
        , "properties" .= object
            [ key (T.pack name) .= schemaFromTyp (stripOption typ)
            | (name,typ) <- fields
            ]
        , "required" .= array required
        ]
  where
    required =
        [ s (T.pack name)
        | (name,typ) <- fields, not (isOption typ)
        ]

stripOption :: Typ -> Typ
stripOption (Unary Option a) = a
stripOption a = a

isOption :: Typ -> Bool
isOption (Unary Option _) = True
isOption _ = False

-- | Map a union type to a JSON.
--
-- The encoding corresponds to the 'ObjectWithSingleField' encoding.
schemaFromUnion :: [(ConstructorName, Typ)] -> JS.Value
schemaFromUnion constructors =
    object [ "oneOf" .= array (map fromConstructor constructors) ]
  where
    fromConstructor (name,typ) =
        object
            [ "type" .= s "object"
            , "title" .= s (T.pack name)
            , "properties" .=
                object [ key (T.pack name) .= schemaFromTyp typ ]
            , "required" .= array [ s (T.pack name) ]
            , "additionalProperties" .= JS.toJSON False
            ]

{-----------------------------------------------------------------------------
    Preprocessing
------------------------------------------------------------------------------}
-- | Modify the 'Typ' to be closer to JSON.
jsonify :: Declarations -> Typ -> Typ
jsonify declarations =
    mergeRecords . representFiniteMaps . resolveVars declarations

representFiniteMaps :: Typ -> Typ
representFiniteMaps = everywhere represent
  where
    represent x@(Binary op a b)
        | op == FiniteSupport || op == PartialFunction =
            Unary Sequence (Binary Product a b)
        | otherwise =
            x
    represent x = x

mergeRecords :: Typ -> Typ
mergeRecords = everywhere merge
  where
    merge (Binary Product (Record a) (Record b)) =
        Record (a <> b)
    merge x = x

{-----------------------------------------------------------------------------
    JSON helpers
------------------------------------------------------------------------------}
key :: Text -> JS.Key
key = JS.Key.fromText

s :: Text -> JS.Value
s = JS.String

object :: [JS.Pair] -> JS.Value
object = JS.object

array :: [JS.Value] -> JS.Value
array = JS.toJSON
