{-# LANGUAGE DataKinds #-}
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
    ( ByronKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv
    , unsafeDeserializeXPrv
    )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , forAll
    , property
    )
import Prelude

import qualified Data.ByteArray as BA
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
                unsafeDeserializeXPrv ShelleyKeyS
                    (serializeXPrv ShelleyKeyS creds)
                    `shouldBe` creds
        it "V1 Byron key roundtrips through serialize/deserialize"
            $ property
            $ forAll genV1ByronCreds
            $ \creds ->
                unsafeDeserializeXPrv ByronKeyS
                    (serializeXPrv ByronKeyS creds)
                    `shouldBe` creds
        it "V2 Shelley key (no payload) roundtrips"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let creds :: HashedCredentials ShelleyKey
                    creds = HashedCredentialsV2 ekey Nothing
                    creds' = unsafeDeserializeXPrv ShelleyKeyS
                        (serializeXPrv ShelleyKeyS creds)
                creds' `shouldBe` creds
        it "V2 Byron key (with payload) roundtrips"
            $ withFastKdfForTesting
            $ do
                ekey <- mkTestEncryptedKey
                let payload =
                        Passphrase
                            (BA.convert @BS.ByteString
                                "payload-bytes-0123456789abcdef0")
                    creds :: HashedCredentials ByronKey
                    creds = HashedCredentialsV2 ekey (Just payload)
                    creds' = unsafeDeserializeXPrv ByronKeyS
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
    case
        encryptedCreate
            (BS.replicate 32 0x02 :: BS.ByteString)
            ("test-passphrase-0123456789" :: BS.ByteString)
            (BS.replicate 32 0xAB :: BS.ByteString)
    of
        Left err -> error $ "mkTestEncryptedKey: " <> show err
        Right k -> pure k
