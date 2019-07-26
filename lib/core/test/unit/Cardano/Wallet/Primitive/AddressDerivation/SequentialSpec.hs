{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.SequentialSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index, Passphrase (..), publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( ChangeChain (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , elements
    , expectFailure
    , property
    , (===)
    , (==>)
    )

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

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary ChangeChain where
    shrink _ = []
    arbitrary = elements [InternalChain, ExternalChain]
