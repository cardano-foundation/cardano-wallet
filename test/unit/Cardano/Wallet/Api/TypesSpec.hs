{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.TypesSpec (spec) where

import Prelude hiding
    ( id )

import Cardano.Wallet.Api
    ( api )
import Cardano.Wallet.Api.Types
    ( ApiAddress (..)
    , ApiMnemonicT (..)
    , ApiT (..)
    , ApiWallet (..)
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , passphraseMaxLength
    , passphraseMinLength
    , walletNameMaxLength
    , walletNameMinLength
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( CheckSumBits
    , ConsistentEntropy
    , Entropy
    , EntropySize
    , MnemonicException (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ambiguousNatVal
    , entropyToBytes
    , entropyToMnemonic
    , mkEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , PoolId (..)
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
    ( FromJSON (..), ToJSON (..) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.FileEmbed
    ( embedFile )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Swagger
    ( Definitions
    , NamedSchema (..)
    , Operation
    , PathItem (..)
    , Schema
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
import Data.Swagger.Declare
    ( Declare )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import GHC.TypeLits
    ( KnownSymbol, symbolVal )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>), (:>), Capture, QueryParam, ReqBody, StdMethod (..), Verb )
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
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , frequency
    , vectorOf
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Instances.Time
    ()

import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID.Types as UUID
import qualified Data.Yaml as Yaml
import qualified Prelude

spec :: Spec
spec = do
    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            roundtripAndGolden $ Proxy @ ApiAddress
            roundtripAndGolden $ Proxy @ ApiWallet
            roundtripAndGolden $ Proxy @ WalletPostData
            roundtripAndGolden $ Proxy @ WalletPutData
            roundtripAndGolden $ Proxy @ WalletPutPassphraseData
            roundtripAndGolden $ Proxy @ (ApiT Address)
            roundtripAndGolden $ Proxy @ (ApiT AddressPoolGap)
            roundtripAndGolden $ Proxy @ (ApiT (WalletDelegation (ApiT PoolId)))
            roundtripAndGolden $ Proxy @ (ApiT WalletId)
            roundtripAndGolden $ Proxy @ (ApiT WalletName)
            roundtripAndGolden $ Proxy @ (ApiT WalletBalance)
            roundtripAndGolden $ Proxy @ (ApiT WalletPassphraseInfo)
            roundtripAndGolden $ Proxy @ (ApiT WalletState)
            roundtripAndGolden $ Proxy @ (ApiT (Passphrase "encryption"))

    describe
        "verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using validateToJSON." $
        validateEveryToJSON api

    describe
        "verify that every path specified by the servant server matches an \
        \existing path in the specification" $
        validateEveryPath api

    describe "verify parsing failures too" $ do
        it "ApiT Address" $ do
            let msg = "Error in $: Unable to decode Address: \
                    \expected Base58 encoding"
            Aeson.parseEither parseJSON [aesonQQ|"-----"|]
                `shouldBe` (Left @String @(ApiT Address) msg)

        it "ApiT (Passphrase \"encryption\") (too short)" $ do
            let msg = "Error in $: passphrase is too short: \
                    \expected at least " <> show passphraseMinLength <> " chars"
            Aeson.parseEither parseJSON [aesonQQ|"patate"|]
                `shouldBe` (Left @String @(ApiT (Passphrase "encryption")) msg)

        it "ApiT (Passphrase \"encryption\") (too long)" $ do
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show passphraseMaxLength <> " chars"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*passphraseMaxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "encryption")) msg)

        it "ApiT WalletName (too short)" $ do
            let msg = "Error in $: name is too short: \
                    \expected at least " <> show walletNameMinLength <> " chars"
            Aeson.parseEither parseJSON [aesonQQ|""|]
                `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiT WalletName (too long)" $ do
            let msg = "Error in $: name is too long: \
                    \expected at most " <> show walletNameMaxLength <> " chars"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*walletNameMaxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiMnemonicT '[12] (not enough words)" $ do
            let msg = "Error in $: ErrMnemonicWords (ErrWrongNumberOfWords 3 12)"
            Aeson.parseEither parseJSON [aesonQQ|
                ["toilet", "toilet", "toilet"]
            |] `shouldBe` (Left @String @(ApiMnemonicT '[12] "test") msg)

        it "ApiT AddressPoolGap (too small)" $ do
            let msg = "Error in $: ErrGapOutOfRange 9"
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap minBound - 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT AddressPoolGap (too big)" $ do
            let msg = "Error in $: ErrGapOutOfRange 101"
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap maxBound + 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

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

instance Arbitrary ApiAddress where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Address where
    arbitrary = Address . B8.pack <$> replicateM 50 arbitrary

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    arbitrary = Quantity <$> arbitraryBoundedEnum

instance Arbitrary ApiWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutPassphraseData where
    arbitrary = genericArbitrary
    shrink = genericShrink

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
        WalletName . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink (WalletName t)
        | T.length t <= walletNameMinLength = []
        | otherwise = [WalletName $ T.take walletNameMinLength t]

instance Arbitrary (Passphrase "encryption") where
    arbitrary = do
        n <- choose (passphraseMinLength, passphraseMaxLength)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take passphraseMinLength
            $ BA.convert bytes ]

instance Arbitrary WalletPassphraseInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ ambiguousNatVal @n
            entropy =
                mkEntropy  @n . B8.pack <$> vectorOf (size `quot` 8) arbitrary
        in
            either (error . show . UnexpectedEntropyError) Prelude.id <$> entropy

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    )
    => Arbitrary (ApiMnemonicT (mw ': '[]) purpose)
  where
    arbitrary = do
        ent <- arbitrary @(Entropy n)
        return $ ApiMnemonicT
            ( Passphrase $ entropyToBytes ent
            , mnemonicToText $ entropyToMnemonic ent
            )

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , Arbitrary (ApiMnemonicT rest purpose)
    )
    => Arbitrary (ApiMnemonicT (mw ': rest) purpose)
  where
    arbitrary = do
        ApiMnemonicT x <- arbitrary @(ApiMnemonicT '[mw] purpose)
        ApiMnemonicT y <- arbitrary @(ApiMnemonicT rest purpose)
        -- NOTE
        -- If we were to "naively" combine previous generators without weights,
        -- we would be tilting probabilities towards the leftmost element, so
        -- that every element would be twice as likely to appear as its right-
        -- hand neighbour, with an exponential decrease. (After the 7th element,
        -- subsequent elements would have less than 1 percent chance of
        -- appearing.) By tweaking the weights a bit as we have done below, we
        -- make it possible for every element to have at least 10% chance of
        -- appearing, for lists up to 10 elements.
        frequency
            [ (1, pure $ ApiMnemonicT x)
            , (5, pure $ ApiMnemonicT y)
            ]

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
    unsafeDecode = either (error . (msg <>) . show) Prelude.id . Yaml.decodeEither'
    msg = "Whoops! Failed to parse or find the api specification document: "

instance ToSchema ApiAddress where
    declareNamedSchema _ = declareSchemaForDefinition "Address"

instance ToSchema ApiWallet where
    declareNamedSchema _ = declareSchemaForDefinition "Wallet"

instance ToSchema WalletPostData where
    declareNamedSchema _ = declareSchemaForDefinition "WalletPostData"

instance ToSchema WalletPutData where
    declareNamedSchema _ = declareSchemaForDefinition "WalletPutData"

instance ToSchema WalletPutPassphraseData where
    declareNamedSchema _ = declareSchemaForDefinition "WalletPutPassphraseData"

-- | Utility function to provide an ad-hoc 'ToSchema' instance for a definition:
-- we simply look it up within the Swagger specification.
declareSchemaForDefinition :: T.Text -> Declare (Definitions Schema) NamedSchema
declareSchemaForDefinition ref =
    case specification ^. definitions . at ref of
        Nothing -> error $
            "unable to find the definition for " <> show ref <> " in the spec"
        Just schema ->
            return $ NamedSchema (Just ref) schema

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

instance HasPath sub => HasPath (ReqBody a b :> sub) where
    getPath _ = getPath (Proxy @sub)

instance HasPath sub => HasPath (QueryParam a b :> sub) where
    getPath _ = getPath (Proxy @sub)

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
