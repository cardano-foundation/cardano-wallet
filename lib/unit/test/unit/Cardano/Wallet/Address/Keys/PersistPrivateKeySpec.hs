{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Tests for V1 and V2 key serialization roundtrips in
-- "Cardano.Wallet.Address.Keys.PersistPrivateKey".
module Cardano.Wallet.Address.Keys.PersistPrivateKeySpec
    ( spec
    )
where

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.Crypto.WalletHD.Encrypted
    ( EncryptedKey
    , encryptedCreate
    , withFastKdfForTesting
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv
    , unsafeDeserializeXPrv
    )
import Cardano.Wallet.DB.Arbitrary
    (
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( PassphraseHash
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials (..)
    )
import Control.Exception
    ( evaluate
    )
import Test.Hspec
    ( Spec
    , anyException
    , describe
    , it
    , shouldBe
    , shouldThrow
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , forAll
    , generate
    , property
    )
import Prelude

import qualified Data.ByteString as BS

{-------------------------------------------------------------------------------
    Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "PersistPrivateKey" $ do
    describe "V1/V2 serialization roundtrips" $ do
        it "V1 Shelley key roundtrips through serialize/deserialize"
            $ property
            $ forAll genV1ShelleyCreds
            $ \creds ->
                unsafeDeserializeXPrv
                    ShelleyKeyS
                    (serializeXPrv ShelleyKeyS creds)
                    `shouldBe` creds
        it "V1 Byron key roundtrips through serialize/deserialize"
            $ property
            $ forAll genV1ByronCreds
            $ \creds ->
                unsafeDeserializeXPrv
                    ByronKeyS
                    (serializeXPrv ByronKeyS creds)
                    `shouldBe` creds
        it "V2 Shelley key roundtrips"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let creds :: HashedCredentials ShelleyKey
                    creds = HashedCredentialsV2 ekey Nothing
                    creds' =
                        unsafeDeserializeXPrv
                            ShelleyKeyS
                            (serializeXPrv ShelleyKeyS creds)
                creds' `shouldBe` creds
        it "V2 Byron key (with payload) roundtrips"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                ByronKey _ _ payload <- generate (arbitrary @(ByronKey 'RootK XPrv))
                let creds :: HashedCredentials ByronKey
                    creds = HashedCredentialsV2 ekey (Just payload)
                    creds' =
                        unsafeDeserializeXPrv
                            ByronKeyS
                            (serializeXPrv ByronKeyS creds)
                creds' `shouldBe` creds
        it "V2 key column is longer than 256 hex chars"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let creds :: HashedCredentials ShelleyKey
                    creds = HashedCredentialsV2 ekey Nothing
                    (keyCol, _) = serializeXPrv ShelleyKeyS creds
                BS.length keyCol > 256 `shouldBe` True
        it "V2 hash column is empty"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let creds :: HashedCredentials ShelleyKey
                    creds = HashedCredentialsV2 ekey Nothing
                    (_, hashCol) = serializeXPrv ShelleyKeyS creds
                hashCol `shouldBe` ""
    describe "Malformed input throws" $ do
        it "garbled Shelley key column throws"
            $ evaluate (unsafeDeserializeXPrv ShelleyKeyS ("garbage", ""))
            `shouldThrow` anyException
        it "V2-prefix with invalid hex payload throws"
            $ evaluate (unsafeDeserializeXPrv ShelleyKeyS ("V2:notvalidhex!", ""))
            `shouldThrow` anyException
        it "short key column (neither V1 nor V2) throws"
            $ evaluate (unsafeDeserializeXPrv ShelleyKeyS ("aabbcc", ""))
            `shouldThrow` anyException
        it "garbled Byron key column throws"
            $ evaluate (unsafeDeserializeXPrv ByronKeyS ("garbage", ""))
            `shouldThrow` anyException
        it "V2-prefix with invalid hex payload throws for Byron"
            $ evaluate (unsafeDeserializeXPrv ByronKeyS ("V2:notvalidhex!", ""))
            `shouldThrow` anyException
    describe "Cross-version rejection" $ do
        it "V2 Shelley column deserializes as HashedCredentialsV2, not V1"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let creds :: HashedCredentials ShelleyKey
                    creds = HashedCredentialsV2 ekey Nothing
                    result = unsafeDeserializeXPrv ShelleyKeyS (serializeXPrv ShelleyKeyS creds)
                case result of
                    HashedCredentialsV1{} -> fail "V2 Shelley column misidentified as V1"
                    HashedCredentialsV2{} -> pure ()
        it "V1 Shelley column deserializes as HashedCredentialsV1, not V2"
            $ property
            $ forAll genV1ShelleyCreds
            $ \v1Creds ->
                case unsafeDeserializeXPrv ShelleyKeyS (serializeXPrv ShelleyKeyS v1Creds) of
                    HashedCredentialsV2{} -> fail "V1 Shelley column misidentified as V2"
                    HashedCredentialsV1{} -> pure () :: IO ()
        it "V2 Byron column deserializes as HashedCredentialsV2, not V1"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                ByronKey _ _ payload <- generate (arbitrary @(ByronKey 'RootK XPrv))
                let creds :: HashedCredentials ByronKey
                    creds = HashedCredentialsV2 ekey (Just payload)
                    result = unsafeDeserializeXPrv ByronKeyS (serializeXPrv ByronKeyS creds)
                case result of
                    HashedCredentialsV1{} -> fail "V2 Byron column misidentified as V1"
                    HashedCredentialsV2{} -> pure ()
        it "V1 Byron column deserializes as HashedCredentialsV1, not V2"
            $ property
            $ forAll genV1ByronCreds
            $ \v1Creds ->
                case unsafeDeserializeXPrv ByronKeyS (serializeXPrv ByronKeyS v1Creds) of
                    HashedCredentialsV2{} -> fail "V1 Byron column misidentified as V2"
                    HashedCredentialsV1{} -> pure () :: IO ()

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}

genV1ShelleyCreds :: Gen (HashedCredentials ShelleyKey)
genV1ShelleyCreds =
    HashedCredentialsV1
        <$> arbitrary @(ShelleyKey 'RootK XPrv)
        <*> arbitrary @PassphraseHash

genV1ByronCreds :: Gen (HashedCredentials ByronKey)
genV1ByronCreds =
    HashedCredentialsV1
        <$> arbitrary @(ByronKey 'RootK XPrv)
        <*> arbitrary @PassphraseHash

-- | Create a test 'EncryptedKey' from fixed-byte inputs.
-- Must be called within 'withFastKdfForTesting'.
mkTestEncryptedKey :: IO EncryptedKey
mkTestEncryptedKey =
    encryptedCreate
        (BS.replicate 32 0x02 :: BS.ByteString)
        ("test-passphrase-0123456789" :: BS.ByteString)
        (BS.replicate 32 0xAB :: BS.ByteString)
        >>= \case
            Left err -> error $ "mkTestEncryptedKey: " <> show err
            Right k -> pure k
