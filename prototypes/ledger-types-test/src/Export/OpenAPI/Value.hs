{-# LANGUAGE OverloadedStrings #-}

module Export.OpenAPI.Value
    ( jsonFromValue
    ) where

import Prelude

import Data.Aeson
    ( (.=) )
import Data.Base16.Types
    ( extractBase16 )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Module
    ( resolveVars )
import Typ
import Value

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as JS.Key
import qualified Data.Aeson.Types as JS
import qualified Data.ByteString.Base16 as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    JSON export
------------------------------------------------------------------------------}
-- | Convert a 'Value' with a 'Typ' to a JSON value.
--
-- We need the 'Typ' of the 'Value' in order to add field names.
jsonFromValue :: Typ -> Value -> JS.Value
jsonFromValue = go
  where
    go :: Typ -> Value -> JS.Value
    go (Binary Product ta tb) (ProductV [a,b])
        = JS.object [ "0" .= go ta a, "1" .= go tb b ]
    go (Record fields) x@(ProductV xs)
        = jsonFromRecord fields
            $ flattenBinaryProductV (length fields) x

    go (Binary Sum ta _) (SumV 0 a)
        = JS.object [ "0" .= go ta a ]
    go (Binary Sum _ tb) (SumV 1 b)
        = JS.object [ "1" .= go tb b ]
    go (Union constructors) (SumV ix a)
        = jsonFromUnion constructors ix a

    go Abstract _ = error "jsonFromValue: Typ may not be abstract."
    go (Var _) (Zero v) = go0 v
    go (Unary op t) (One v) = go1 op t v
    go (Binary op ta tb) (Two v) = go2 v

    go _ _ = error "jsonFromValue: Typ error"

    go0 :: ZeroF -> JS.Value
    go0 (BoolV b) = JS.toJSON b
    go0 (BytesV s) = JS.toJSON $ toHex s
    go0 (IntegerV i) = JS.toJSON i
    go0 (NaturalV n) = JS.toJSON n
    go0 (TextV t) = JS.toJSON t
    go0 (UnitV) = JS.Null

    go1 :: OpUnary -> Typ -> OneF Value -> JS.Value
    go1 Option t (OptionV (Just x)) = JS.object [ "0" .= go t x ]
    go1 Option t (OptionV Nothing) = JS.object []
    go1 Sequence t (SequenceV xs) = JS.toJSON $ map (go t) xs
    go1 PowerSet t (PowerSetV xs) = JS.toJSON $ map (go t) $ Set.toList xs
    go1 _ _ _ = error "jsonFromValue: Typ error"

    go2 :: TwoF Value Value -> JS.Value
    go2 (FiniteMapV _)
        = error "FiniteMapV is not supported by JSON"

-- | Flatten a chain of @n@ binary products to a single 'ProductV'.
flattenBinaryProductV :: Int -> Value -> [Value]
flattenBinaryProductV = flatten
  where
    flatten :: Int -> Value -> [Value]
    flatten 1 x = [x]
    flatten n (ProductV [x,y]) = x : flatten (n-1) y
    flatten n x = [x]

jsonFromRecord :: [(FieldName, Typ)] -> [Value] -> JS.Value
jsonFromRecord fields xs
    | length fields == length xs
        = JS.object
            [ key (T.pack field) .= jsonFromValue typ2 x2
            | ((field,typ), x) <- zip fields xs
            , omitNothingOption x
            , let (typ2,x2) = skipJustOption (typ,x)
            ]
    | otherwise
        = error "jsonFromRecord: field count of Value does not match Typ"
  where
    omitNothingOption = (One (OptionV Nothing) /=)

    skipJustOption :: (Typ,Value) -> (Typ,Value)
    skipJustOption (Unary Option typ, One (OptionV (Just x))) = (typ,x)
    skipJustOption y = y

jsonFromUnion :: [(ConstructorName, Typ)] -> Ix -> Value -> JS.Value
jsonFromUnion constructors ix a
    | 0 <= ix && ix < length constructors
        = JS.object [ key (T.pack name) .= jsonFromValue typ a ]
    | otherwise
        = error "jsonFromUnion: index of Value does not match Typ"
  where
    (name, typ) = constructors !! ix

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
toHex :: ByteString -> Text
toHex = extractBase16 . B.encodeBase16

key :: Text -> JS.Key
key = JS.Key.fromText
