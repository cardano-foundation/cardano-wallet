{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , DerivationType (..)
    , Index
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , generateKeyFromSeed
    , getIndex
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Control.Monad
    ( replicateM )
import Data.Proxy
    ( Proxy (..) )
import Fmt
    ( build, fmt )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , elements
    , expectFailure
    , property
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Bounded / Enum relationship" $ do
        it "The calls Index.succ maxBound should result in a runtime err (hard)"
            prop_succMaxBoundHardIx
        it "The calls Index.pred minBound should result in a runtime err (hard)"
            prop_predMinBoundHardIx
        it "The calls Index.succ maxBound should result in a runtime err (soft)"
            prop_succMaxBoundSoftIx
        it "The calls Index.pred minBound should result in a runtime err (soft)"
            prop_predMinBoundSoftIx
        it "Calling toEnum for invalid value gives a runtime err (ChangeChain)"
            (property prop_toEnumChangeChain)

    describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @(Passphrase "encryption")

    describe "Enum Roundtrip" $ do
        it "ChangeChain" (property prop_roundtripEnumChangeChain)
        it "Index @'Hardened _" (property prop_roundtripEnumIndexHard)
        it "Index @'Soft _" (property prop_roundtripEnumIndexSoft)

    describe "BIP-0044 Derivation Properties" $ do
        it "deriveAccountPrivateKey works for various indexes" $
            property prop_accountKeyDerivation
        it "N(CKDpriv((kpar, cpar), i)) === CKDpub(N(kpar, cpar), i)" $
            property prop_publicChildKeyDerivation

    describe "Golden Tests - Yoroi's style addresses" $ do
        let seed0 = Passphrase "4\175\242L\184\243\191 \169]\171 \207\r\v\233\NUL~&\ETB"
        let recPwd0 = mempty
        it "m/0'/0/0 --> Ae2tdPwUPEZGB...EfoeiuW4MtaXZ" $ do
            let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x00000000)
            goldenYoroiAddr (seed0, recPwd0) ExternalChain accIx addrIx
                "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"
        it "m/0'/0/14 --> Ae2tdPwUPEZD...bxbkCyQYyxckP" $ do
            let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x0000000E)
            goldenYoroiAddr (seed0, recPwd0) ExternalChain accIx addrIx
                "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"
        it "m/14'/1/42 --> Ae2tdPwUPEZ...EkxDbkPodpMAi" $ do
            let (accIx, addrIx) = (toEnum 0x8000000E, toEnum 0x0000002A)
            goldenYoroiAddr (seed0, recPwd0) InternalChain accIx addrIx
                "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

        let seed1 = Passphrase "\171\151\240\DC4\147Q\ACK\NULfJxq\176h\172\DEL/\DC4\DC2\227\&6\155\129\134\f\221/\NUL\175a\252\249"
        let recPwd1 = Passphrase "Cardano the cardano that cardano!"
        it "m/0'/0/0 --> Ae2tdPwUPEZ1D...64dqTSRpWqzLH" $ do
            let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x00000000)
            goldenYoroiAddr (seed1, recPwd1) ExternalChain accIx addrIx
                "Ae2tdPwUPEZ1DYmhvpJWtVkMUbypPVkCVjQLNJeKRRG4LJ64dqTSRpWqzLH"
        it "m/0'/0/14 --> Ae2tdPwUPEZ7...pVwEPhKwseVvf" $ do
            let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x0000000E)
            goldenYoroiAddr (seed1, recPwd1) ExternalChain accIx addrIx
                "Ae2tdPwUPEZ7ZyqyuDKkCnjrRjTY1vMJ8353gD7XWrUYufpVwEPhKwseVvf"
        it "m/14'/1/42 --> Ae2tdPwUPEZ...nRtbfw6EHRv1D" $ do
            let (accIx, addrIx) = (toEnum 0x8000000E, toEnum 0x0000002A)
            goldenYoroiAddr (seed1, recPwd1) InternalChain accIx addrIx
                "Ae2tdPwUPEZLSqQN7XNJRMJ6yHWdfFLaQgPPYgyJKrJnCVnRtbfw6EHRv1D"


{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}


prop_succMaxBoundHardIx :: Property
prop_succMaxBoundHardIx = expectFailure $
    property $ succ (maxBound @(Index 'Hardened _)) `seq` ()

prop_predMinBoundHardIx :: Property
prop_predMinBoundHardIx = expectFailure $
    property $ pred (minBound @(Index 'Hardened _)) `seq` ()

prop_succMaxBoundSoftIx :: Property
prop_succMaxBoundSoftIx = expectFailure $
    property $ succ (maxBound @(Index 'Soft _)) `seq` ()

prop_predMinBoundSoftIx :: Property
prop_predMinBoundSoftIx = expectFailure $
    property $ pred (minBound @(Index 'Soft _)) `seq` ()

prop_toEnumChangeChain :: Int -> Property
prop_toEnumChangeChain n =
    n > fromEnum InternalChain ==> expectFailure $ property $
        (toEnum n :: ChangeChain) `seq` ()

prop_roundtripEnumChangeChain :: ChangeChain -> Property
prop_roundtripEnumChangeChain ix =
    (toEnum . fromEnum) ix === ix

prop_roundtripEnumIndexHard :: Index 'Hardened 'AccountK -> Property
prop_roundtripEnumIndexHard ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripEnumIndexSoft :: Index 'Soft 'AddressK -> Property
prop_roundtripEnumIndexSoft ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

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
    accXPrv = unsafeGenerateKeyFromSeed (seed, recPwd) (encPwd)
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
    rootXPrv = generateKeyFromSeed (seed, recPwd) encPwd
    accXPub = deriveAccountPrivateKey encPwd rootXPrv ix

goldenYoroiAddr
    :: (Passphrase "seed", Passphrase "generation")
    -> ChangeChain
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> String
    -> Property
goldenYoroiAddr (seed, recPwd) cc accIx addrIx addr =
    let
        encPwd = mempty
        rootXPrv = generateKeyFromSeed (seed, recPwd) encPwd
        accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
        addrXPrv = deriveAddressPrivateKey encPwd accXPrv cc addrIx
    in
        fmt (build $ keyToAddress $ publicKey addrXPrv) === addr


{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Passphrase goal) where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance {-# OVERLAPS #-} Arbitrary (Passphrase "encryption") where
    arbitrary = do
        let p = Proxy :: Proxy "encryption"
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes

instance Arbitrary ChangeChain where
    shrink _ = []
    arbitrary = elements [InternalChain, ExternalChain]
