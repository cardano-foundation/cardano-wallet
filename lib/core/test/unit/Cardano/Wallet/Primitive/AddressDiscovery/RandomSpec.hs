{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , PaymentAddress (..)
    , NetworkDiscriminant (..)
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
    ( GenChange (..), IsOurs (..), IsOwned (..), KnownAddresses (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..), mkRndState )
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
    , conjoin
    , property
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    let goldenInitial = GoldenTest
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
    let goldenAnother = GoldenTest
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
    let goldenBogus = GoldenTest
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
    it "check isOurs for initial account" $
        checkIsOurs goldenInitial
    it "check isOurs for another account" $
        checkIsOurs goldenAnother
    it "check isOurs for bogus address" $
        checkIsOurs goldenBogus
    it "check isOwned for initial account" $
        checkIsOwned goldenInitial
    it "check isOwned for another account" $
        checkIsOwned goldenAnother
    it "check isOwned for bogus address" $
        checkIsOwned goldenBogus


goldenSpecTestnet :: Spec
goldenSpecTestnet =
    describe "Golden tests forByron Addresses w/ random scheme (Testnet)" $ do
    let goldenInitial = GoldenTest
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
    let goldenAnother = GoldenTest
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
    it "check isOurs - initial account" $
        checkIsOurs goldenInitial
    it "check isOurs - another account" $
        checkIsOurs goldenAnother
    it "check isOwned - initial account" $
        checkIsOwned goldenInitial
    it "check isOwned - another account" $
        checkIsOwned goldenAnother


{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data GoldenTest = GoldenTest
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

checkIsOurs :: GoldenTest -> Expectation
checkIsOurs GoldenTest{..} = do
    fst (isOurs addr' rndState) `shouldBe` expected
  where
    Right addr' = Address <$> convertFromBase Base16 addr
    rndState = rndStateFromMnem arbitraryMnemonic

checkIsOwned :: GoldenTest -> Expectation
checkIsOwned GoldenTest{..} = do
    isOwned st (rndKey, pwd) addr' `shouldBe` expectation
  where
    pwd = Passphrase ""
    Right addr' = Address <$> convertFromBase Base16 addr
    st@RndState{rndKey} = rndStateFromMnem arbitraryMnemonic
    accXPrv = deriveAccountPrivateKey pwd rndKey (Index accIndex)
    addrXPrv = deriveAddressPrivateKey pwd accXPrv (Index addrIndex)
    expectation = if expected then
        Just (addrXPrv, pwd)
        else Nothing

rndStateFromMnem :: [Text] -> RndState 'Testnet
rndStateFromMnem mnem = mkRndState @'Testnet rootXPrv 0
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
    it "isOwned works as expected during key derivation" $ do
        property prop_derivedKeysAreOwned
    it "GenChange address always satisfies isOurs" $ do
        property prop_changeAddressesBelongToUs
    it "each address discovered by isOurs is in forbiden addresses and different than change address" $ do
        property prop_forbiddenAddreses

-- | A pair of random address discovery state, and the encryption passphrase for
-- the RndState root key.
data Rnd = Rnd (RndState 'Testnet) (Passphrase "encryption")

prop_derivedKeysAreOurs
    :: Rnd
    -> Rnd
    -> Index 'Hardened 'AddressK
    -> Property
prop_derivedKeysAreOurs
    (Rnd st@(RndState rk accIx _ _ _) pwd) (Rnd st' _) addrIx =
    fst (isOurs addr st) .&&. not (fst (isOurs addr st'))
  where
    addr = paymentAddress @'Testnet addrKey
    accKey = deriveAccountPrivateKey pwd rk accIx
    addrKey = publicKey $ deriveAddressPrivateKey pwd accKey addrIx

prop_derivedKeysAreOwned
    :: Rnd
    -> Rnd
    -> Index 'Hardened 'AddressK
    -> Property
prop_derivedKeysAreOwned
    (Rnd st@(RndState rk accIx _ _ _) pwd)
    (Rnd st'@(RndState rk' _ _ _ _) pwd')
    addrIx =
    isOwned st (rndKey st, pwd) addr === Just (addrKeyPrv, pwd)
    .&&.
    isOwned st' (rk', pwd') addr === Nothing
  where
    addr = paymentAddress @'Testnet (publicKey addrKeyPrv)
    accKey = deriveAccountPrivateKey pwd rk accIx
    addrKeyPrv = deriveAddressPrivateKey pwd accKey addrIx

prop_changeAddressesBelongToUs
    :: Rnd
    -> Rnd
    -> Property
prop_changeAddressesBelongToUs
    (Rnd st pwd)
    (Rnd st' _) =
    fst (isOurs addr st) .&&. not (fst (isOurs addr st'))
  where
    (addr, _) = genChange pwd st

prop_forbiddenAddreses
    :: Rnd
    -> Index 'Hardened 'AddressK
    -> Property
prop_forbiddenAddreses (Rnd st@(RndState rk accIx _ _ _) pwd) addrIx = conjoin
    [ (Set.notMember addr (forbidden st))
    , (Set.member addr (forbidden isOursSt))
    , (Set.notMember changeAddr (forbidden isOursSt))
    , (Set.member changeAddr (forbidden changeSt))
    , (addr `elem` knownAddresses isOursSt)
    , (changeAddr `notElem` knownAddresses changeSt)
    ]
  where
    (_ours, isOursSt) = isOurs addr st
    (changeAddr, changeSt) = genChange pwd isOursSt

    forbidden s = Set.fromList $ Map.elems $ addresses s <> pendingAddresses s

    addr = paymentAddress @'Testnet (publicKey addrKeyPrv)
    accKey = deriveAccountPrivateKey pwd rk accIx
    addrKeyPrv = deriveAddressPrivateKey pwd accKey addrIx

{-------------------------------------------------------------------------------
                    Instances
-------------------------------------------------------------------------------}

instance Eq (RndState t) where
    (RndState k1 accIx1 _ _ _) == (RndState k2 accIx2 _ _ _)
        = getKey k1 == getKey k2 && accIx1 == accIx2

instance Show Rnd where
    show (Rnd (RndState a _ _ _ _) _) = show (getKey a)

instance Arbitrary Rnd where
    shrink _ = []  -- no shrinking
    arbitrary = do
        s <- genPassphrase @"seed" (16, 32)
        e <- genPassphrase @"encryption" (0, 16)
        let key = generateKeyFromSeed s e
        pure $ Rnd (mkRndState key 0) e
      where
        genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
        genPassphrase range = do
            n <- choose range
            InfiniteList bytes _ <- arbitrary
            return $ Passphrase $ BA.convert $ BS.pack $ take n bytes
