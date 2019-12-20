{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.IcarusSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( encodeAddress )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , Depth (..)
    , DerivationType (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , XPrv
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ( genLegacyAddress )
import Cardano.Wallet.Primitive.Types
    ( Address )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , vectorOf
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Golden Tests - Icarus' style addresses" $ do
        let seed0 = Passphrase "4\175\242L\184\243\191 \169]\171 \207\r\v\233\NUL~&\ETB"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x80000000) UTxOExternal (toEnum 0x00000000)
            "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x80000000) UTxOExternal (toEnum 0x0000000E)
            "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"

        goldenAddressGeneration $ GoldenAddressGeneration
            seed0 (toEnum 0x8000000E) UTxOInternal (toEnum 0x0000002A)
            "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "MkKeyFingerprint Properties" $ do
        it "paymentKeyFingerprint . liftPaymentAddress == pure" $
            property prop_roundtripFingerprintLift

{-------------------------------------------------------------------------------
                               Golden Tests
-------------------------------------------------------------------------------}

data GoldenAddressGeneration = GoldenAddressGeneration
    { goldSeed :: Passphrase "seed"
    , goldAcctIx :: Index 'Hardened 'AccountK
    , goldAcctStyle :: AccountingStyle
    , goldAddrIx :: Index 'Soft 'AddressK
    , goldAddr :: String
    }

-- | Compare addresses obtained from a given derivation path and a root seed to
-- their known equivalent in base58.
goldenAddressGeneration
    :: GoldenAddressGeneration
    -> Spec
goldenAddressGeneration test = it title $ do
    let encPwd = mempty
    let rootXPrv = generateKeyFromSeed goldSeed encPwd
    let acctXPrv = deriveAccountPrivateKey encPwd rootXPrv goldAcctIx
    let addrXPrv = deriveAddressPrivateKey encPwd acctXPrv goldAcctStyle goldAddrIx
    base58 (paymentAddress @'Mainnet $ publicKey addrXPrv) `shouldBe` goldAddr
  where
    GoldenAddressGeneration
        { goldSeed
        , goldAddr
        , goldAcctIx
        , goldAddrIx
        , goldAcctStyle
        } = test

    title = unwords
        [ fmtPath goldAcctIx goldAcctStyle goldAddrIx
        , "-->"
        , goldAddr
        ]

    base58 = T.unpack . encodeAddress @'Mainnet

    -- e.g. m/.../0'/0/0
    fmtPath p3 p4 p5 = mconcat
        [ "m/.../"
        , show (fromEnum p3 - fromEnum (minBound @(Index 'Hardened _)))
        , "'/"
        , show (fromEnum p4)
        , "/"
        , show (fromEnum p5)
        ]

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
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> AccountingStyle
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
    :: Passphrase "seed"
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

instance Arbitrary AccountingStyle where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

instance Arbitrary Address where
    arbitrary = genLegacyAddress (30, 50)
