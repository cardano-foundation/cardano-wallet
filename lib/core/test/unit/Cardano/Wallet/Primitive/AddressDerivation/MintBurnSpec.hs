{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.MintBurnSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( KeyHash )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (PolicyK, RootK, ScriptK)
    , DerivationType (Hardened)
    , Index
    , Passphrase
    , WalletKey (publicKey)
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( derivePolicyKeyAndHash, derivePolicyPrivateKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Unsafe
    ( unsafeXPrv )
import qualified Data.ByteString as BS
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, vector, (===) )
import Test.QuickCheck.Arbitrary
    ( arbitraryBoundedEnum )

import qualified Cardano.Address.Script as CA

spec :: Spec
spec = do
    parallel $ describe "Mint/Burn Policy key Address Derivation Properties" $ do
        it "Policy key derivation from master key works for various indexes" $
            property prop_keyDerivationFromXPrv
        it "Policy public key hash matches private key" $
            property prop_keyHashMatchesXPrv

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationFromXPrv
    :: Passphrase "encryption"
    -> XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyDerivationFromXPrv pwd masterkey policyIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: XPrv
    rndKey = derivePolicyPrivateKey pwd masterkey policyIx

prop_keyHashMatchesXPrv
    :: Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
    -> Index 'Hardened 'PolicyK
    -> Property
prop_keyHashMatchesXPrv pwd masterkey policyIx =
    hashVerificationKey
      CA.Payment
      (getPublicKey rndKey)
      === keyHash
  where
    rndKey :: ShelleyKey 'PolicyK XPrv
    keyHash :: KeyHash
    (rndKey, keyHash) = derivePolicyKeyAndHash pwd masterkey policyIx

    getPublicKey
        :: ShelleyKey 'PolicyK XPrv
        -> ShelleyKey 'ScriptK XPub
    getPublicKey =
        publicKey . (liftRawKey :: XPrv -> ShelleyKey 'ScriptK XPrv) . getRawKey

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128

instance Arbitrary (Index 'Hardened 'PolicyK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum
