{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.TypesSpec (spec) where

import Prelude

import Cardano.Wallet
    ( mkWalletName, walletNameMaxLength, walletNameMinLength )
import Cardano.Wallet.Api
    ( api )
import Cardano.Wallet.Api.Types
    ( AddressPoolGap
    , ApiT (..)
    , PoolId (..)
    , Wallet (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Lens
    ( Lens', at, (^.) )
import Control.Monad
    ( replicateM )
import Data.Aeson
    ( FromJSON, ToJSON )
import Data.Either
    ( rights )
import Data.FileEmbed
    ( embedFile )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Swagger
    ( NamedSchema (..)
    , Operation
    , PathItem (..)
    , Swagger
    , ToSchema (..)
    , definitions
    , delete
    , get
    , patch
    , paths
    , post
    , put
    )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import GHC.TypeLits
    ( KnownSymbol, symbolVal )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>), (:>), Capture, StdMethod (..), Verb )
import Servant.Swagger.Test
    ( validateEveryToJSON )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Proxy (Proxy)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , roundtripAndGoldenSpecsWithSettings
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), arbitraryBoundedEnum, arbitraryPrintableChar, choose )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Instances.Time
    ()

import qualified Data.Text as T
import qualified Data.UUID.Types as UUID
import qualified Data.Yaml as Yaml

spec :: Spec
spec = do
    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            roundtripAndGolden $ Proxy @ Wallet
            roundtripAndGolden $ Proxy @ (ApiT AddressPoolGap)
            roundtripAndGolden $ Proxy @ (ApiT (WalletDelegation (ApiT PoolId)))
            roundtripAndGolden $ Proxy @ (ApiT WalletId)
            roundtripAndGolden $ Proxy @ (ApiT WalletName)
            roundtripAndGolden $ Proxy @ (ApiT WalletBalance)
            roundtripAndGolden $ Proxy @ (ApiT WalletPassphraseInfo)
            roundtripAndGolden $ Proxy @ (ApiT WalletState)

    describe
        "verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using validateToJSON." $
        validateEveryToJSON api

    describe
        "verify that every path specified by the servant server matches an \
        \existing path in the specification" $
        validateEveryPath api

--
-- Golden tests files are generated automatically on first run. On later runs
-- we check that the format stays the same. The golden files should be tracked
-- in git.
--
-- Example:
-- >>> roundtripAndGolden $ Proxy @ Wallet
--
-- ...will compare @ToJSON@ of @Wallet@ against `Wallet.json`. It may either
-- match and succeed, or fail and write `Wallet.faulty.json` to disk with the
-- new format. Faulty golden files should /not/ be commited.
--
-- The directory `test/data/Cardano/Wallet/Api` is used.
roundtripAndGolden
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => Proxy a
    -> Spec
roundtripAndGolden = roundtripAndGoldenSpecsWithSettings settings
  where
    settings :: Settings
    settings = defaultSettings
        { goldenDirectoryOption =
            CustomDirectoryName "test/data/Cardano/Wallet/Api"
        , useModuleNameAsSubDirectory =
            False
        , sampleSize = 4
        }

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    arbitrary = Quantity <$> arbitraryBoundedEnum

instance Arbitrary Wallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (WalletDelegation (ApiT PoolId)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PoolId where
    arbitrary = PoolId . T.pack <$> replicateM 3 arbitraryPrintableChar

instance Arbitrary WalletId where
    arbitrary = WalletId . uuidFromWords <$> arbitrary

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a, b, c, d) = UUID.fromWords a b c d

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        either (error "Unable to create arbitrary WalletName") id
            . mkWalletName
            . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink =
        rights
            . fmap (mkWalletName . T.pack)
            . shrink
            . T.unpack
            . getWalletName

instance Arbitrary WalletPassphraseInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT


{-------------------------------------------------------------------------------
                   Specification / Servant-Swagger Machinery

  Below is a bit of complicated API-Level stuff in order to achieve two things:

  1/ Verify that every response from the API that actually has a JSON content
     type returns a JSON instance that matches the JSON format described by the
     specification (field names should be the same, and constraints on values as
     well).
     For this, we need three things:
         - ToJSON instances on all those types, it's a given with the above
         - Arbitrary instances on all those types, that reflect as much as
           possible, all possible values of those types. Also given by using
           'genericArbitrary' whenever possible.
         - ToSchema instances which tells how do a given type should be
           represented.
     The trick is for the later point. In a "classic" scenario, we would have
     defined the `ToSchema` instances directly in Haskell on our types, which
     eventually becomes a real pain to maintain. Instead, we have written the
     spec by hand, and we want to check that our implementation matches it.
     So, we "emulate" the 'ToSchema' instance by:
         - Parsing the specification file (which is embedded at compile-time)
         - Creating missing 'ToSchema' by doing lookups in that global schema

  2/ The above verification is rather weak, because it just controls the return
     types of endpoints, but not that those endpoints are somewhat valid. Thus,
     we've also built another check 'validateEveryPath' which crawls our servant
     API type, and checks whether every path we have in our API appears in the
     specification. It does it by defining a few recursive type-classes to
     crawl the API, and for each endpoint:
         - construct the corresponding path (with verb)
         - build an HSpec scenario which checks whether the path is present
    This seemingly means that the identifiers we use in our servant paths (in
    particular, those for path parameters) should exactly match the specs.

-------------------------------------------------------------------------------}

-- | Specification file, embedded at compile-time and decoded right away
specification :: Swagger
specification =
    unsafeDecode bytes
  where
    bytes = $(embedFile "specifications/api/swagger.yaml")
    unsafeDecode = either ( error . (msg <>) . show) id . Yaml.decodeEither'
    msg = "Whoops! Failed to parse or find the api specification document: "

-- | Ad-hoc 'ToSchema' instance for the 'Wallet' definition, we simply look it
-- up from the specification.
instance ToSchema Wallet where
    declareNamedSchema _ = case specification ^. definitions . at "Wallet" of
        Nothing ->
            error "unable to find the definition for 'Wallet' in the spec"
        Just schema ->
            return $ NamedSchema (Just "Wallet") schema

-- | Verify that all servant endpoints are present and match the specification
class ValidateEveryPath api where
    validateEveryPath :: Proxy api -> Spec

instance {-# OVERLAPS #-} HasPath a => ValidateEveryPath a where
    validateEveryPath proxy = do
        let (verb, path) = getPath proxy
        it (show verb <> " " <> path <> " exists in specification") $ do
            case specification ^. paths . at path of
                Just item | isJust (item ^. atMethod verb) -> return @IO ()
                _ -> fail "couldn't find path in specification"

instance (HasPath a, ValidateEveryPath b) => ValidateEveryPath (a :<|> b) where
    validateEveryPath _ = do
        validateEveryPath (Proxy @a)
        validateEveryPath (Proxy @b)

-- | Extract the path of a given endpoint, in a format that is swagger-friendly
class HasPath api where
    getPath :: Proxy api -> (StdMethod, String)

instance (Method m) => HasPath (Verb m s ct a) where
    getPath _ = (method (Proxy @m), "")

instance (KnownSymbol path, HasPath sub) => HasPath (path :> sub) where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in (verb, "/" <> symbolVal (Proxy :: Proxy path) <> sub)

instance (KnownSymbol param, HasPath sub) => HasPath (Capture param t :> sub)
  where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in (verb, "/{" <> symbolVal (Proxy :: Proxy param) <> "}" <> sub)

-- A way to demote 'StdMethod' back to the world of values. Servant provides a
-- 'reflectMethod' that does just that, but demote types to raw 'ByteString' for
-- an unknown reason :/
instance Method 'GET where method _ = GET
instance Method 'POST where method _ = POST
instance Method 'PUT where method _ = PUT
instance Method 'DELETE where method _ = DELETE
instance Method 'PATCH where method _ = PATCH
class Method (m :: StdMethod) where
    method :: Proxy m -> StdMethod

atMethod :: StdMethod -> Lens' PathItem (Maybe Operation)
atMethod = \case
    GET -> get
    POST -> post
    PUT -> put
    DELETE -> delete
    PATCH -> patch
    m -> error $ "atMethod: unsupported method: " <> show m
