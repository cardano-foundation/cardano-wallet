{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.ShelleySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( ChainCode (..)
    , ChangeChain (..)
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , XPrv
    , XPub (..)
    , paymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..)
    , addrGroupedSize
    , addrSingleSize
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.Exception
    ( SomeException, evaluate )
import Data.Either
    ( isLeft, isRight )
import Data.List
    ( isSubsequenceOf )
import Data.Maybe
    ( isJust, isNothing )
import Test.Hspec
    ( Spec, describe, it, shouldThrow )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , conjoin
    , elements
    , expectFailure
    , property
    , suchThat
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Bounded / Enum relationship" $ do
        it "Calling toEnum for invalid value gives a runtime err (ChangeChain)"
            (property prop_toEnumChangeChain)

    describe "Enum Roundtrip" $ do
        it "ChangeChain" (property prop_roundtripEnumChangeChain)

    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Encoding" $ do
        let cc = ChainCode "<ChainCode is not used by singleAddressToKey>"

        let userException str (e :: SomeException) = str `isSubsequenceOf` show e

        it "throws when encoding XPub of invalid length (Mainnet)" $ do
            let msg = "length was 2, but expected to be 33"
            evaluate (paymentAddress @'Mainnet (ShelleyKey $ XPub "\148" cc))
                `shouldThrow` userException msg

        it "throws when encoding XPub of invalid length (Testnet)" $ do
            let msg = "length was 2, but expected to be 33"
            evaluate (paymentAddress @'Testnet (ShelleyKey $ XPub "\148" cc))
                `shouldThrow` userException msg

    describe "KeyFingerprint" $ do
        it "Single addresses have a payment key but no delegation key"
            (property prop_fingerprintSingleAddress)
        it "Grouped addresses have a payment key and a delegation key"
            (property prop_fingerprintGroupedAddress)
        it "Inspecting Invalid addresses throws"
            (property prop_fingerprintInvalidAddress)

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_toEnumChangeChain :: Int -> Property
prop_toEnumChangeChain n =
    n > fromEnum InternalChain ==> expectFailure $ property $
        (toEnum n :: ChangeChain) `seq` ()

prop_roundtripEnumChangeChain :: ChangeChain -> Property
prop_roundtripEnumChangeChain ix =
    (toEnum . fromEnum) ix === ix

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
    :: (Passphrase "seed", Passphrase "generation")
    -> Passphrase "encryption"
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Property
prop_publicChildKeyDerivation (seed, recPwd) encPwd cc ix =
    addrXPub1 === addrXPub2
  where
    accXPrv = unsafeGenerateKeyFromSeed (seed, recPwd) encPwd :: ShelleyKey 'AccountK XPrv
    -- N(CKDpriv((kpar, cpar), i))
    addrXPub1 = publicKey $ deriveAddressPrivateKey encPwd accXPrv cc ix
    -- CKDpub(N(kpar, cpar), i)
    addrXPub2 = deriveAddressPublicKey (publicKey accXPrv) cc ix

prop_accountKeyDerivation
    :: (Passphrase "seed", Passphrase "generation")
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Property
prop_accountKeyDerivation (seed, recPwd) encPwd ix =
    accXPub `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed (seed, recPwd) encPwd :: ShelleyKey 'RootK XPrv
    accXPub = deriveAccountPrivateKey encPwd rootXPrv ix

-- | Single addresses have a payment key but no delegation key
prop_fingerprintSingleAddress
    :: SingleAddress
    -> Property
prop_fingerprintSingleAddress (SingleAddress addr) = conjoin $ property <$>
    [ isRight (paymentKeyFingerprint @ShelleyKey addr)
    , either (const False) isNothing (delegationKeyFingerprint @ShelleyKey addr)
    ]

-- | Grouped addresses have a payment key and a delegation key
prop_fingerprintGroupedAddress
    :: GroupedAddress
    -> Property
prop_fingerprintGroupedAddress (GroupedAddress addr) = conjoin $ property <$>
    [ isRight (paymentKeyFingerprint @ShelleyKey addr)
    , either (const False) isJust (delegationKeyFingerprint @ShelleyKey addr)
    ]

-- | Inspecting Invalid addresses throws
prop_fingerprintInvalidAddress
    :: InvalidAddress
    -> Property
prop_fingerprintInvalidAddress (InvalidAddress addr) = conjoin $ property <$>
    [ isLeft (paymentKeyFingerprint @ShelleyKey addr)
    , isLeft (delegationKeyFingerprint @ShelleyKey addr)
    ]

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary ChangeChain where
    shrink _ = []
    arbitrary = elements [InternalChain, ExternalChain]

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

newtype SingleAddress = SingleAddress Address deriving (Eq, Show)

instance Arbitrary SingleAddress where
    arbitrary = SingleAddress . Address . BS.pack
        <$> vectorOf addrSingleSize arbitrary

newtype GroupedAddress = GroupedAddress Address deriving (Eq, Show)

instance Arbitrary GroupedAddress where
    arbitrary = GroupedAddress . Address . BS.pack
        <$> vectorOf addrGroupedSize arbitrary

newtype InvalidAddress = InvalidAddress Address deriving (Eq, Show)

instance Arbitrary InvalidAddress where
    arbitrary = InvalidAddress . Address . BS.pack <$> do
        n <- suchThat (choose (1, 100)) (`notElem` [addrSingleSize, addrGroupedSize])
        vectorOf n arbitrary
