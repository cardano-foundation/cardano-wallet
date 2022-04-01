{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.IcarusSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.Gen
    ( genLegacyAddress )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , vector
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    parallel $ describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    parallel $ describe "MkKeyFingerprint Properties" $ do
        it "paymentKeyFingerprint . liftPaymentAddress == pure" $
            property prop_roundtripFingerprintLift

{-------------------------------------------------------------------------------
                                 Properties
-------------------------------------------------------------------------------}

-- | Deriving address public key should be equal to deriving address
-- private key and extracting public key from it (works only for non-hardened
-- child keys).
--
-- To compute the public child key of a parent private key:
--  * N(CKDpriv((kpar, cpar), i)) (works always).
--  * CKDpub(N(kpar, cpar), i) (works only for non-hardened child keys).
--
-- Thus:
--
-- N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)
--
-- if (kpar, cpar) is a non-hardened key.
--
-- For details see <https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#private-parent-key--public-child-key bip-0039>
prop_publicChildKeyDerivation
    :: SomeMnemonic
    -> Passphrase "encryption"
    -> Role
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation seed encPwd cc ix =
    addrXPub1 === addrXPub2
  where
    accXPrv = unsafeGenerateKeyFromSeed seed encPwd :: IcarusKey 'AccountK XPrv
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = publicKey $ deriveAddressPrivateKey encPwd accXPrv cc ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (publicKey accXPrv) cc ix

prop_accountKeyDerivation
    :: SomeMnemonic
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation seed encPwd ix =
    accXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed seed encPwd :: IcarusKey 'RootK XPrv
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv ix

prop_roundtripFingerprintLift
    :: Address
    -> Property
prop_roundtripFingerprintLift addr =
    let
        fingerprint = paymentKeyFingerprint @IcarusKey addr
        eAddr = liftPaymentAddress @'Mainnet <$> fingerprint
    in
        eAddr === Right addr

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary Role where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vector n
        return $ Passphrase $ BA.convert bytes

instance Arbitrary Address where
    arbitrary = genLegacyAddress Nothing
