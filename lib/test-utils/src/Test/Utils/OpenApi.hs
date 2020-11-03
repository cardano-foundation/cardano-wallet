{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Utils.OpenApi where

import Prelude

import Data.Aeson
    ( ToJSON (..) )
import Data.Data
    ( Typeable, typeRep )
import Data.OpenApi
    ( ToSchema (..), validatePrettyToJSON )
import Data.Proxy
    ( Proxy (..) )
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

