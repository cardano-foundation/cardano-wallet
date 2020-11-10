{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Utils.OpenApi where

import Prelude

import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.Encode.Pretty
    ( encodePretty )
import Data.Data
    ( Typeable, typeRep )
import Data.OpenApi
    ( ToSchema (..), toSchema, validatePrettyToJSON )
import Data.OpenApi.Internal.Schema.Validation
    ( ValidationError, validateToJSONWithPatternChecker )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Servant
    ( JSON )
import Servant.Swagger.Internal.TypeLevel.API
    ( BodyTypes )
import Servant.Swagger.Internal.TypeLevel.Every
    ( Every, EveryTF, tmapEvery )
import Servant.Swagger.Internal.TypeLevel.TMap
    ( TMap )
import Test.Hspec
    ( Spec )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), Property, counterexample, property )

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Regex pattern for @string@ type.
type Pattern = Text

-- Rip-off of 'Servant.Swagger.Test' since there is no openapi3 equivalent
-- of servant-swagger yet.
validateEveryToJSON
  :: forall proxy api .
     TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema])
          (BodyTypes JSON api)
  => proxy api   -- ^ Servant API.
  -> Spec
validateEveryToJSON _ = props
    (Proxy :: Proxy [ToJSON, ToSchema])
    (maybeCounterExample . validatePrettyToJSON)
    (Proxy :: Proxy (BodyTypes JSON api))
  where
    maybeCounterExample :: Maybe String -> Property
    maybeCounterExample Nothing  = property True
    maybeCounterExample (Just s) = counterexample s (property False)

    props :: forall p p'' cs xs. TMap (Every (Typeable ': Show ': Arbitrary ': cs)) xs =>
      p cs                                          -- ^ A list of constraints.
      -> (forall x. EveryTF cs x => x -> Property)  -- ^ Property predicate.
      -> p'' xs                                     -- ^ A list of types.
      -> Spec
    props _ f px = sequence_ specs
      where
        specs :: [Spec]
        specs = tmapEvery (Proxy :: Proxy (Typeable ': Show ': Arbitrary ': cs)) aprop px

        aprop :: forall p' a. (EveryTF cs a, Typeable a, Show a, Arbitrary a) => p' a -> Spec
        aprop _ = prop (show (typeRep (Proxy :: Proxy a))) (f :: a -> Property)

-- | Verify that every type used with @'JSON'@ content type in a servant API
-- has compatible @'ToJSON'@ and @'ToSchema'@ instances using @'validateToJSONWithPatternChecker'@.
--
-- For validation without patterns see @'validateEveryToJSON'@.
validateEveryToJSONWithPatternChecker :: forall proxy api. TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema]) (BodyTypes JSON api) =>
        (Pattern -> Text -> Bool)   -- ^ @'Pattern'@ checker.
        -> proxy api                -- ^ Servant API.
        -> Spec
validateEveryToJSONWithPatternChecker checker _ = props
    (Proxy :: Proxy [ToJSON, ToSchema])
    (maybeCounterExample . prettyValidateWith (validateToJSONWithPatternChecker checker))
    (Proxy :: Proxy (BodyTypes JSON api))
  where
    -- | Construct property tests for each type in a list.
    -- The name for each property is the name of the corresponding type.
    --
    -- >>> :{
    --  hspec $
    --    context "read . show == id" $
    --      props
    --        (Proxy :: Proxy [Eq, Show, Read])
    --        (\x -> read (show x) === x)
    --        (Proxy :: Proxy [Bool, Int, String])
    -- :}
    -- <BLANKLINE>
    -- read . show == id
    --   Bool
    -- ...
    --   Int
    -- ...
    --   [Char]
    -- ...
    -- Finished in ... seconds
    -- 3 examples, 0 failures
    props :: forall p p'' cs xs. TMap (Every (Typeable ': Show ': Arbitrary ': cs)) xs =>
      p cs                                          -- ^ A list of constraints.
      -> (forall x. EveryTF cs x => x -> Property)  -- ^ Property predicate.
      -> p'' xs                                     -- ^ A list of types.
      -> Spec
    props _ f px = sequence_ specs
      where
        specs :: [Spec]
        specs = tmapEvery (Proxy :: Proxy (Typeable ': Show ': Arbitrary ': cs)) aprop px

        aprop :: forall p' a. (EveryTF cs a, Typeable a, Show a, Arbitrary a) => p' a -> Spec
        aprop _ = prop (show (typeRep (Proxy :: Proxy a))) (f :: a -> Property)

    -- | Provide a counterexample if there is any.
    maybeCounterExample :: Maybe String -> Property
    maybeCounterExample Nothing  = property True
    maybeCounterExample (Just s) = counterexample s (property False)

    -- | Pretty print validation errors
    -- together with actual JSON and Swagger Schema
    -- (using 'encodePretty').
    --
    -- >>> import Data.Aeson
    -- >>> import Data.Foldable (traverse_)
    -- >>> data Person = Person { name :: String, phone :: Integer } deriving (Generic)
    -- >>> instance ToJSON Person where toJSON p = object [ "name" .= name p ]
    -- >>> instance ToSchema Person
    -- >>> let person = Person { name = "John", phone = 123456 }
    -- >>> traverse_ putStrLn $ prettyValidateWith validateToJSON person
    -- Validation against the schema fails:
    --   * property "phone" is required, but not found in "{\"name\":\"John\"}"
    -- <BLANKLINE>
    -- JSON value:
    -- {
    --     "name": "John"
    -- }
    -- <BLANKLINE>
    -- Swagger Schema:
    -- {
    --     "required": [
    --         "name",
    --         "phone"
    --     ],
    --     "type": "object",
    --     "properties": {
    --         "phone": {
    --             "type": "integer"
    --         },
    --         "name": {
    --             "type": "string"
    --         }
    --     }
    -- }
    -- <BLANKLINE>
    --
    -- FIXME: this belongs in "Data.Swagger.Schema.Validation" (in @swagger2@).
    prettyValidateWith
      :: forall a. (ToJSON a, ToSchema a)
      => (a -> [ValidationError]) -> a -> Maybe String
    prettyValidateWith f x =
      case f x of
        []      -> Nothing
        errors  -> Just $ unlines
          [ "Validation against the schema fails:"
          , unlines (map ("  * " ++) errors)
          , "JSON value:"
          , ppJSONString json
          , ""
          , "Swagger Schema:"
          , ppJSONString (toJSON schema)
          ]
      where
        ppJSONString = TL.unpack . TL.decodeUtf8 . encodePretty

        json   = toJSON x
        schema = toSchema (Proxy :: Proxy a)
