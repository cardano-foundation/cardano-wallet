{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , WalletKey (..)
    , fromMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , property
    , (.&&.)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    goldenSpecMainnet
    goldenSpecTestnet
    propSpec

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpecMainnet :: Spec
goldenSpecMainnet =
    describe "Golden tests for Byron Addresses w/ random scheme (Mainnet)" $ do
    it "check isOurs for initial account" $
        checkIsOurs CheckIsOursTest
        { mnem =
                arbitraryMnemonic
        , addr =
                "82d818584283581ca08bcb9e5e8cd30d5aea6d434c46abd8604fe4907d\
                \56b9730ca28ce5a101581e581c22e25f2464ec7295b556d86d0ec33bc1\
                \a681e7656da92dbc0582f5e4001a3abe2aa5"
        , accIndex =
                2147483648
        , addrIndex =
                2147483648
        , expected = True
        }
    it "check isOurs for another account" $
        checkIsOurs CheckIsOursTest
        { mnem =
                arbitraryMnemonic
        , addr =
                "82d818584283581cb039e80866203e82fc834b8e6a355b83ec6f8fd199\
                \66078a40e6d6b2a101581e581c22e27fb12d08728073cd416dfbfcb8dc\
                \0e760335d1d60f65e8740034001a4bce4d1a"
        , accIndex =
                2694138340
        , addrIndex =
                2512821145
        , expected = True
        }
    it "check isOurs for bogus address" $
        checkIsOurs CheckIsOursTest
        { mnem =
                arbitraryMnemonic
        , addr =
                "82d818584283581cb039e80866203e82fc834b8e6a355b83ec6f8fd199"
        , accIndex =
                2694138340
        , addrIndex =
                2512821145
        , expected = False
        }

goldenSpecTestnet :: Spec
goldenSpecTestnet =
    describe "Golden tests forByron Addresses w/ random scheme (Testnet)" $ do
    it "check isOurs - initial account" $
        checkIsOurs CheckIsOursTest
        { mnem =
                arbitraryMnemonic
        , addr =
                "82d818584983581ca03d42af673855aabcef3059e21c37235ae706072d\
                \38150dcefae9c6a201581e581c22e25f2464ec7295b556d86d0ec33bc1\
                \a681e7656da92dbc0582f5e402451a4170cb17001a39a0b7b5"
        , accIndex =
                2147483648
        , addrIndex =
                2147483648
        , expected = True
        }
    it "check isOurs - another account" $
        checkIsOurs CheckIsOursTest
        { mnem =
                arbitraryMnemonic
        , addr =
                "82d818584983581c267b40902921c3afd73926a83a23ca08ae9626a64a\
                \4b5616d14d6709a201581e581c22e219c90fb572d565134f6daeab650d\
                \c871d130430afe594116f1ae02451a4170cb17001aee75f28a"
        , accIndex =
                3337448281
        , addrIndex =
                3234874775
        , expected = True
        }

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data CheckIsOursTest = CheckIsOursTest
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    , expected :: Bool
    } deriving (Show, Eq)

-- An arbitrary mnemonic sentence for the tests
arbitraryMnemonic :: [Text]
arbitraryMnemonic =
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

-- A different arbitrary mnemonic sentence for the tests.
anotherMnemonic :: [Text]
anotherMnemonic =
    [ "toss", "reunion", "lunar", "pilot", "direct", "chicken"
    , "give", "total", "future", "wrap", "sunny", "ostrich" ]

checkIsOurs :: CheckIsOursTest -> Expectation
checkIsOurs CheckIsOursTest{..} = do
    fst (isOurs addr' rndState) `shouldBe` expected
    fst (isOurs addr' rndStateWrong) `shouldBe` False
  where
    Right addr' = Address <$> convertFromBase Base16 addr
    rndState = rndStateFromMnem arbitraryMnemonic
    rndStateWrong = rndStateFromMnem anotherMnemonic

rndStateFromMnem :: [Text] -> RndState
rndStateFromMnem mnem = RndState rootXPrv
  where
    rootXPrv = generateKeyFromSeed (Passphrase seed) (Passphrase "")
    Right (Passphrase seed) = fromMnemonic @'[12] mnem

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

propSpec :: Spec
propSpec = describe "Random Address Discovery Properties" $ do
    it "isOurs works as expected during key derivation" $ do
        property prop_derivedKeysAreOurs

prop_derivedKeysAreOurs
    :: RndStatePassphrase
    -> RndStatePassphrase
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> Property
prop_derivedKeysAreOurs
    (RndStatePassphrase st pwd)
    (RndStatePassphrase st' _)
    accIx addrIx =
    fst (isOurs addr st) .&&. not (fst (isOurs addr st'))
  where
    rk = getRndState st
    addr = keyToAddress @DummyTarget addrKey
    accKey = deriveAccountPrivateKey pwd rk accIx
    addrKey = publicKey $ deriveAddressPrivateKey pwd accKey addrIx

{-------------------------------------------------------------------------------
                    Instances
-------------------------------------------------------------------------------}

instance Eq RndState where
    (RndState a) == (RndState b) = getKey a == getKey b

instance Show RndState where
    show (RndState a) = show (getKey a)

data RndStatePassphrase = RndStatePassphrase
    { rndState :: RndState
    , passphrase :: Passphrase "encryption"
    } deriving (Show, Eq)

instance Arbitrary RndStatePassphrase where
    shrink _ = []  -- no shrinking
    arbitrary = do
        (s, e) <- (,)
            <$> genPassphrase @"seed" (16, 32)
            <*> genPassphrase @"encryption" (0, 16)
        let st = generateKeyFromSeed s e
        pure $ RndStatePassphrase (RndState st) e
      where
        genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
        genPassphrase range = do
            n <- choose range
            InfiniteList bytes _ <- arbitrary
            return $ Passphrase $ BA.convert $ BS.pack $ take n bytes
