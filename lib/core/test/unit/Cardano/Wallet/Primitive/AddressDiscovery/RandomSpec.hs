{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..), SomeMnemonic (..) )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , WalletKey (..)
    , liftIndex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..), IsOurs (..), IsOwned (..), KnownAddresses (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..), findUnusedPath, mkRndState )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.List
    ( find )
import Data.Maybe
    ( isJust, isNothing )
import Data.Word
    ( Word32 )
import System.Random
    ( mkStdGen )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , conjoin
    , counterexample
    , property
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
    goldenSpecMainnet
    goldenSpecTestnet
    propSpec

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpecMainnet :: Spec
goldenSpecMainnet =
    parallel $ describe "Golden tests for Byron Addresses w/ random scheme (Mainnet)" $ do
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
    it "findUnusedPath: indexes are always in the 'hardened' realm" $
        property prop_IndexesAlwaysHardened

prop_IndexesAlwaysHardened
    :: Int
    -> Index 'Hardened 'AccountK
    -> Property
prop_IndexesAlwaysHardened g accIx =
    let
        ((accIx', addrIx), _) = findUnusedPath (mkStdGen g) accIx Set.empty
    in
        accIx' >= liftIndex (minBound :: Index 'Hardened 'AccountK)
      .&&.
        addrIx >= liftIndex (minBound :: Index 'Hardened 'AddressK)

goldenSpecTestnet :: Spec
goldenSpecTestnet =
    parallel $ describe "Golden tests forByron Addresses w/ random scheme (Testnet)" $ do
    let golden01 = GoldenTest
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

    let golden02 = GoldenTest
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

    let golden03 = GoldenTest
            { mnem =
                    arbitraryMnemonic
            , addr =
                    "82d818584083581cf26d102b29332fd6c244a9915b6cad7890f5b54ac3\
                    \4dcd62975b525aa201565522f6c70e9b236c753e50a3758e18e8bbf7c3\
                    \f9e34e02451a2d964a09001a3993f9ea"
            , accIndex =
                    14
            , addrIndex =
                    42
            , expected = True
            }

    forM_ [golden01, golden02, golden03] $ \test -> do
        it "isOurs Golden"  (checkIsOurs test)
        it "isOwned Golden" (checkIsOwned test)

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data GoldenTest = GoldenTest
    { mnem :: SomeMnemonic
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    , expected :: Bool
    } deriving (Show, Eq)

-- An arbitrary mnemonic sentence for the tests
arbitraryMnemonic :: SomeMnemonic
arbitraryMnemonic = either (error . show) id $ mkSomeMnemonic @'[12]
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

checkIsOurs :: GoldenTest -> Expectation
checkIsOurs GoldenTest{..} = do
    isJust (fst $ isOurs addr' rndState) `shouldBe` expected
  where
    Right addr' = Address <$> convertFromBase Base16 addr
    (_, rndState) = rndStateFromMnem arbitraryMnemonic

checkIsOwned :: GoldenTest -> Expectation
checkIsOwned GoldenTest{..} = do
    isOwned st (rndKey, pwd) addr' `shouldBe` expectation
  where
    pwd = Passphrase ""
    Right addr' = Address <$> convertFromBase Base16 addr
    (rndKey, st) = rndStateFromMnem arbitraryMnemonic
    accXPrv = deriveAccountPrivateKey pwd rndKey (Index accIndex)
    addrXPrv = deriveAddressPrivateKey pwd accXPrv (Index addrIndex)
    expectation = if expected then
        Just (addrXPrv, pwd)
        else Nothing

rndStateFromMnem :: SomeMnemonic -> (ByronKey 'RootK XPrv, RndState 'Mainnet)
rndStateFromMnem mnemonic = (rootXPrv, mkRndState @'Mainnet rootXPrv 0)
  where
    rootXPrv = generateKeyFromSeed mnemonic (Passphrase "")

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

propSpec :: Spec
propSpec = parallel $ describe "Random Address Discovery Properties" $ do
    it "isOurs works as expected during key derivation" $ do
        property prop_derivedKeysAreOurs
    it "isOwned works as expected during key derivation" $ do
        property prop_derivedKeysAreOwned
    it "GenChange address always satisfies isOurs" $ do
        property prop_changeAddressesBelongToUs
    it "each address discovered by isOurs is in forbidden addresses and different than change address" $ do
        property prop_forbiddenAddresses
    it "address that are discovered via isOurs are marked as 'Used'" $ do
        property prop_oursAreUsed

-- | A pair of random address discovery state, and the encryption passphrase for
-- the RndState root key.
data Rnd = Rnd
    (RndState 'Mainnet)
    (ByronKey 'RootK XPrv)
    (Passphrase "encryption")
    deriving Show

prop_derivedKeysAreOurs
    :: Rnd
    -> Rnd
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_derivedKeysAreOurs rnd@(Rnd st _ _) (Rnd st' _ _) addrIx =
    isJust (fst $ isOurs addr st) .&&. isNothing (fst $ isOurs addr st')
  where
    addr = mkAddress rnd addrIx

prop_derivedKeysAreOwned
    :: Rnd
    -> Rnd
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_derivedKeysAreOwned (Rnd st rk pwd) (Rnd st' rk' pwd') addrIx =
    isOwned st (rk, pwd) addr === Just (addrKey, pwd)
    .&&.
    isOwned st' (rk', pwd') addr === Nothing
  where
    addr = paymentAddress @'Mainnet (publicKey addrKey)
    addrKey = deriveAddressPrivateKey pwd acctKey addrIx
    acctKey = deriveAccountPrivateKey pwd rk (liftIndex $ accountIndex st)

prop_changeAddressesBelongToUs
    :: Rnd
    -> Rnd
    -> Property
prop_changeAddressesBelongToUs (Rnd st rk pwd) (Rnd st' _ _) =
    isJust (fst $ isOurs addr st) .&&. isNothing (fst $ isOurs addr st')
  where
    (addr, _) = genChange (rk, pwd) st

prop_forbiddenAddresses
    :: Rnd
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_forbiddenAddresses rnd@(Rnd st rk pwd) addrIx = conjoin
    [ (Set.notMember addr (forbidden st))
    , (Set.member addr (forbidden isOursSt))
    , (Set.notMember changeAddr (forbidden isOursSt))
    , (Set.member changeAddr (forbidden changeSt))
    , (addr `elem` ((\(a,_,_) -> a) <$> knownAddresses isOursSt))
    , (changeAddr `elem` ((\(a,_,_) -> a) <$> knownAddresses changeSt))
    ]
  where
    (_ours, isOursSt) = isOurs addr st
    (changeAddr, changeSt) = genChange (rk, pwd) isOursSt
    addr = mkAddress rnd addrIx
    forbidden s =
        Set.fromList $ Map.elems $ (fst <$> discoveredAddresses s) <> pendingAddresses s

prop_oursAreUsed
    :: Rnd
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_oursAreUsed rnd@(Rnd st _ _) addrIx = do
    case find (\(a,_,_) -> (a == addr)) $ knownAddresses $ snd $ isOurs addr st of
        Nothing ->
            property False & counterexample "address not is known addresses"
        Just (_, status,_) ->
            status === Used
  where
    addr = mkAddress rnd addrIx

{-------------------------------------------------------------------------------
                    Instances
-------------------------------------------------------------------------------}

instance Arbitrary Rnd where
    shrink _ = []  -- no shrinking
    arbitrary = do
        s <- SomeMnemonic <$> genMnemonic @12
        e <- genPassphrase @"encryption" (0, 16)
        let key = generateKeyFromSeed s e
        pure $ Rnd (mkRndState key 0) key e
      where
        genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
        genPassphrase range = do
            n <- choose range
            InfiniteList bytes _ <- arbitrary
            return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

mkAddress
    :: Rnd
    -> Index 'WholeDomain 'AddressK
    -> Address
mkAddress (Rnd (RndState _ accIx _ _ _) rk pwd) addrIx =
    let
        acctKey = deriveAccountPrivateKey pwd rk (liftIndex accIx)
        addrKey = deriveAddressPrivateKey pwd acctKey addrIx
    in
        paymentAddress @'Mainnet (publicKey addrKey)
