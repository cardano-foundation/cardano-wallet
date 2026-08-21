{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Address.Discovery.RandomSpec
    ( spec
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , toXPub
    )
import Cardano.Byron.Codec.Cbor
    ( encodeAddress
    , encodeDerivationPathAttr
    , encodeProtocolMagicAttr
    , reconstructAddress
    )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..)
    , SomeMnemonic (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , PaymentAddress (..)
    , liftIndex
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    )
import Cardano.Wallet.Address.DerivationSpec
    (
    )
import Cardano.Wallet.Address.Discovery
    ( GenChange (..)
    , IsOurs (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( DerivationPath
    , RndState (..)
    , candidatePaths
    , deriveCredFromKeyKeyFromPath
    , findUnusedPath
    , mkRndState
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( publicKey
    )
import Cardano.Wallet.Address.States.IsOwned
    ( isOwned
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (ByronKeyS)
    , WalletFlavorS (ByronWallet)
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    , AddressState (..)
    )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (..)
    )
import Control.Monad
    ( forM_
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Data.List
    ( find
    )
import Data.Maybe
    ( isJust
    , isNothing
    )
import Data.Word
    ( Word32
    )
import System.Random
    ( mkStdGen
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )
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
import Prelude

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    goldenSpecMainnet
    goldenSpecTestnet
    golden03Provenance
    mismatchedIndexSpec
    unresolvableAddressSpec
    propSpec

{-------------------------------------------------------------------------------
                   Addresses that no candidate derivation owns
-------------------------------------------------------------------------------}

-- | A second wallet, for addresses that do not belong to the first.
otherMnemonic :: SomeMnemonic
otherMnemonic =
    either (error . show) id
        $ mkSomeMnemonic @'[12]
            [ "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "abandon"
            , "about"
            ]

unresolvableAddressSpec :: Spec
unresolvableAddressSpec =
    describe "addresses that no candidate derivation reproduces" $ do
        let pwd = Passphrase ""
            rootK = generateKeyFromSeed arbitraryMnemonic pwd
            otherRootK = generateKeyFromSeed otherMnemonic pwd
            st = mkRndState @'Mainnet rootK 0
            accIx = liftIndex (accountIndex st)
            addrIx = Index 7 :: Index 'WholeDomain 'CredFromKeyK

            -- The derivation path is encrypted under this wallet's passphrase,
            -- so the address decrypts as ours, but its root comes from another
            -- wallet's key and no candidate can reproduce it.
            impostor =
                Address
                    $ CBOR.toStrictByteString
                    $ encodeAddress
                        ( toXPub
                            $ getKey
                            $ deriveCredFromKeyKeyFromPath
                                otherRootK
                                pwd
                                (accIx, addrIx)
                        )
                        [ encodeDerivationPathAttr
                            (payloadPassphrase rootK)
                            accIx
                            addrIx
                        ]

            foreign' =
                addressRecordingWith
                    otherRootK
                    pwd
                    (accIx, addrIx)
                    (accIx, addrIx)

        it "are still discovered, because their path decrypts"
            $ isJust (fst $ isOurs impostor st)
            `shouldBe` True

        it "yield no signing key"
            $ isOwned ByronWallet st (rootK, pwd) impostor
            `shouldBe` Nothing

        it "yield no signing key for another wallet's address, as before"
            $ isOwned ByronWallet st (rootK, pwd) foreign'
            `shouldBe` Nothing

{-------------------------------------------------------------------------------
              Addresses whose recorded index is not the key's index
-------------------------------------------------------------------------------}

-- | An address that records @recorded@ in its derivation-path attribute while
-- its root is built from the key at @actual@. Addresses created before
-- cardano-sl always hardened generated indexes have this shape.
addressRecording
    :: ByronKey 'RootK XPrv
    -> DerivationPath
    -- ^ the path written into the address
    -> DerivationPath
    -- ^ the path the key is really at
    -> Address
addressRecording rootK = addressRecordingWith rootK (Passphrase "")

-- | The hardened form of an index, as the fix must try it.
hardened :: Index 'WholeDomain level -> Index 'WholeDomain level
hardened (Index ix)
    | ix >= firstHardened = Index ix
    | otherwise = Index (ix + firstHardened)
  where
    firstHardened = getIndex (minBound :: Index 'Hardened 'AccountK)

mismatchedIndexSpec :: Spec
mismatchedIndexSpec =
    describe "addresses whose recorded index is not the key's index" $ do
        let pwd = Passphrase ""
            rootK = generateKeyFromSeed arbitraryMnemonic pwd
            st = mkRndState @'Mainnet rootK 0
            accIx = liftIndex (accountIndex st)
            addrIx = Index 42 :: Index 'WholeDomain 'CredFromKeyK
            recorded = (accIx, addrIx)
            owns actual =
                isOwned
                    ByronWallet
                    st
                    (rootK, pwd)
                    (addressRecording rootK recorded actual)
            keyAt actual =
                Just (deriveCredFromKeyKeyFromPath rootK pwd actual, pwd)

        it "resolves a soft address index to the key at its hardened form"
            $ owns (accIx, hardened addrIx)
            `shouldBe` keyAt (accIx, hardened addrIx)

        it "resolves an unaffected address to the key at its recorded path"
            $ owns recorded
            `shouldBe` keyAt recorded

        -- #1041 records that account indexes were produced by the same
        -- defective function, so the account level is covered as well.
        let softAccIx = Index 14 :: Index 'WholeDomain 'AccountK
            softRecorded = (softAccIx, addrIx)
            ownsSoftAcc actual =
                isOwned
                    ByronWallet
                    st
                    (rootK, pwd)
                    (addressRecording rootK softRecorded actual)

        it "resolves a soft account index to the key at its hardened form"
            $ ownsSoftAcc (hardened softAccIx, addrIx)
            `shouldBe` keyAt (hardened softAccIx, addrIx)

        it "resolves an address whose account and address index are both soft"
            $ ownsSoftAcc (hardened softAccIx, hardened addrIx)
            `shouldBe` keyAt (hardened softAccIx, hardened addrIx)

        it "tries one candidate when the recorded path is already hardened"
            $ candidatePaths (accIx, hardened addrIx)
            `shouldBe` [(accIx, hardened addrIx)]

        it "tries the recorded path first, then hardened forms"
            $ candidatePaths softRecorded
            `shouldBe` [ softRecorded
                       , (softAccIx, hardened addrIx)
                       , (hardened softAccIx, addrIx)
                       , (hardened softAccIx, hardened addrIx)
                       ]

{-------------------------------------------------------------------------------
                        Provenance of the golden03 address
-------------------------------------------------------------------------------}

-- The testnet 'golden03' address records a soft derivation path (14, 42), so it
-- is worth pinning down which key it actually commits to: a soft index does not
-- by itself mean the recorded index is the wrong one. Rebuilding the address
-- from the wallet's own key material shows that it commits to the key at the
-- recorded path, and not to the key at the hardened form of that path. It is
-- therefore an address whose recorded path must keep resolving on the first
-- attempt.
golden03Provenance :: Spec
golden03Provenance = describe "golden03 provenance" $ do
    it "commits to the key at its recorded soft path"
        $ hex (addressAt (Index 14, Index 42))
        `shouldBe` hex golden03Address

golden03Address :: Address
golden03Address =
    let Right bytes =
            convertFromBase @ByteString
                Base16
                "82d818584083581cf26d102b29332fd6c244a9915b6cad7890f5b54ac3\
                \4dcd62975b525aa201565522f6c70e9b236c753e50a3758e18e8bbf7c3\
                \f9e34e02451a2d964a09001a3993f9ea"
    in  Address bytes

-- | Rebuild the golden03 address from the key at a given path, keeping the
-- attributes the address itself records.
addressAt :: DerivationPath -> Address
addressAt path =
    Address
        $ CBOR.toStrictByteString
        $ encodeAddress
            (toXPub $ getKey $ deriveCredFromKeyKeyFromPath rootK pwd path)
            [ encodeDerivationPathAttr hdPwd (Index 14) (Index 42)
            , encodeProtocolMagicAttr (ProtocolMagic 764824073)
            ]
  where
    pwd = Passphrase ""
    rootK = generateKeyFromSeed arbitraryMnemonic pwd
    hdPwd = payloadPassphrase rootK

hex :: Address -> ByteString
hex (Address bytes) = convertToBase Base16 bytes

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpecMainnet :: Spec
goldenSpecMainnet =
    describe "Golden tests for Byron Addresses w/ random scheme (Mainnet)" $ do
        let goldenInitial =
                GoldenTest
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
        let goldenAnother =
                GoldenTest
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
        let goldenBogus =
                GoldenTest
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
        it "check isOurs for initial account"
            $ checkIsOurs goldenInitial
        it "check isOurs for another account"
            $ checkIsOurs goldenAnother
        it "check isOurs for bogus address"
            $ checkIsOurs goldenBogus
        it "check isOwned for initial account"
            $ checkIsOwned goldenInitial
        it "check isOwned for another account"
            $ checkIsOwned goldenAnother
        it "check isOwned for bogus address"
            $ checkIsOwned goldenBogus
        it "findUnusedPath: indexes are always in the 'hardened' realm"
            $ property prop_IndexesAlwaysHardened

prop_IndexesAlwaysHardened
    :: Int
    -> Index 'Hardened 'AccountK
    -> Property
prop_IndexesAlwaysHardened g accIx =
    let
        ((accIx', addrIx), _) = findUnusedPath (mkStdGen g) accIx Set.empty
    in
        accIx' >= liftIndex (minBound :: Index 'Hardened 'AccountK)
            .&&. addrIx >= liftIndex (minBound :: Index 'Hardened 'CredFromKeyK)

goldenSpecTestnet :: Spec
goldenSpecTestnet =
    describe "Golden tests forByron Addresses w/ random scheme (Testnet)" $ do
        let golden01 =
                GoldenTest
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

        let golden02 =
                GoldenTest
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

        let golden03 =
                GoldenTest
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
            it "isOurs Golden" (checkIsOurs test)
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
    }
    deriving (Show, Eq)

-- An arbitrary mnemonic sentence for the tests
arbitraryMnemonic :: SomeMnemonic
arbitraryMnemonic =
    either (error . show) id
        $ mkSomeMnemonic @'[12]
            [ "price"
            , "whip"
            , "bottom"
            , "execute"
            , "resist"
            , "library"
            , "entire"
            , "purse"
            , "assist"
            , "clock"
            , "still"
            , "noble"
            ]

checkIsOurs :: GoldenTest -> Expectation
checkIsOurs GoldenTest{..} = do
    isJust (fst $ isOurs addr' rndState) `shouldBe` expected
  where
    Right addr' = Address <$> convertFromBase Base16 addr
    (_, rndState) = rndStateFromMnem arbitraryMnemonic

checkIsOwned :: GoldenTest -> Expectation
checkIsOwned GoldenTest{..} = do
    isOwned ByronWallet st (rndKey, pwd) addr' `shouldBe` expectation
  where
    pwd = Passphrase ""
    Right addr' = Address <$> convertFromBase Base16 addr
    (rndKey, st) = rndStateFromMnem arbitraryMnemonic
    accXPrv = deriveAccountPrivateKey pwd rndKey (Index accIndex)
    addrXPrv = deriveAddressPrivateKey pwd accXPrv (Index addrIndex)
    expectation =
        if expected
            then
                Just (addrXPrv, pwd)
            else Nothing

rndStateFromMnem
    :: SomeMnemonic -> (ByronKey 'RootK XPrv, RndState 'Mainnet)
rndStateFromMnem mnemonic = (rootXPrv, mkRndState @'Mainnet rootXPrv 0)
  where
    rootXPrv = generateKeyFromSeed mnemonic (Passphrase "")

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

propSpec :: Spec
propSpec = describe "Random Address Discovery Properties" $ do
    it "isOurs works as expected during key derivation" $ do
        property prop_derivedKeysAreOurs
    it "isOwned works as expected during key derivation" $ do
        property prop_derivedKeysAreOwned
    it "every key isOwned returns reproduces the address it is for" $ do
        property prop_ownedKeysReproduceTheirAddress
    it "GenChange address always satisfies isOurs" $ do
        property prop_changeAddressesBelongToUs
    it
        "each address discovered by isOurs is in forbidden addresses and different than change address"
        $ do
            property prop_forbiddenAddresses
    it "address that are discovered via isOurs are marked as 'Used'" $ do
        property prop_oursAreUsed

-- | A pair of random address discovery state, and the encryption passphrase for
-- the RndState root key.
data Rnd
    = Rnd
        (RndState 'Mainnet)
        (ByronKey 'RootK XPrv)
        (Passphrase "encryption")
    deriving (Show)

prop_derivedKeysAreOurs
    :: Rnd
    -> Rnd
    -> Index 'WholeDomain 'CredFromKeyK
    -> Property
prop_derivedKeysAreOurs rnd@(Rnd st _ _) (Rnd st' _ _) addrIx =
    isJust (fst $ isOurs addr st) .&&. isNothing (fst $ isOurs addr st')
  where
    addr = mkAddress rnd addrIx

prop_derivedKeysAreOwned
    :: Rnd
    -> Rnd
    -> Index 'WholeDomain 'CredFromKeyK
    -> Property
prop_derivedKeysAreOwned (Rnd st rk pwd) (Rnd st' rk' pwd') addrIx =
    isOwned ByronWallet st (rk, pwd) addr === Just (addrKey, pwd)
        .&&. isOwned ByronWallet st' (rk', pwd') addr === Nothing
  where
    addr = paymentAddress SMainnet (publicKey ByronKeyS addrKey)
    addrKey = deriveAddressPrivateKey pwd acctKey addrIx
    acctKey = deriveAccountPrivateKey pwd rk (liftIndex $ accountIndex st)

-- | FR-001 / SC-005: a key is only ever returned when it reproduces the address
-- it was resolved for. Checked for an address whose recorded path is correct and
-- for one whose key is at the hardened form of its recorded address index.
prop_ownedKeysReproduceTheirAddress
    :: Rnd
    -> Index 'WholeDomain 'CredFromKeyK
    -> Property
prop_ownedKeysReproduceTheirAddress (Rnd st rk pwd) addrIx =
    conjoin $ reproduces <$> [recorded, (accIx, hardened addrIx)]
  where
    accIx = liftIndex (accountIndex st)
    recorded = (accIx, addrIx)
    reproduces actual =
        let addr = addressRecordingWith rk pwd recorded actual
        in  case isOwned ByronWallet st (rk, pwd) addr of
                Nothing ->
                    property False
                        & counterexample "isOwned returned no key"
                Just (k, _) ->
                    reconstructAddress (toXPub $ getKey k) addr === Just addr

-- | 'addressRecording' for a wallet whose encryption passphrase is not empty.
addressRecordingWith
    :: ByronKey 'RootK XPrv
    -> Passphrase "encryption"
    -> DerivationPath
    -> DerivationPath
    -> Address
addressRecordingWith rootK pwd (recAccIx, recAddrIx) actual =
    Address
        $ CBOR.toStrictByteString
        $ encodeAddress
            (toXPub $ getKey $ deriveCredFromKeyKeyFromPath rootK pwd actual)
            [ encodeDerivationPathAttr
                (payloadPassphrase rootK)
                recAccIx
                recAddrIx
            ]

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
    -> Index 'WholeDomain 'CredFromKeyK
    -> Property
prop_forbiddenAddresses rnd@(Rnd st rk pwd) addrIx =
    conjoin
        [ (Set.notMember addr (forbidden st))
        , (Set.member addr (forbidden isOursSt))
        , (Set.notMember changeAddr (forbidden isOursSt))
        , (Set.member changeAddr (forbidden changeSt))
        , (addr `elem` ((\(a, _, _) -> a) <$> knownAddresses isOursSt))
        , (changeAddr `elem` ((\(a, _, _) -> a) <$> knownAddresses changeSt))
        ]
  where
    (_ours, isOursSt) = isOurs addr st
    (changeAddr, changeSt) = genChange (rk, pwd) isOursSt
    addr = mkAddress rnd addrIx
    forbidden s =
        Set.fromList
            $ Map.elems
            $ (fst <$> discoveredAddresses s) <> pendingAddresses s

prop_oursAreUsed
    :: Rnd
    -> Index 'WholeDomain 'CredFromKeyK
    -> Property
prop_oursAreUsed rnd@(Rnd st _ _) addrIx = do
    case find (\(a, _, _) -> (a == addr))
        $ knownAddresses
        $ snd
        $ isOurs addr st of
        Nothing ->
            property False & counterexample "address not is known addresses"
        Just (_, status, _) ->
            status === Used
  where
    addr = mkAddress rnd addrIx

{-------------------------------------------------------------------------------
                    Instances
-------------------------------------------------------------------------------}

instance Arbitrary Rnd where
    shrink _ = [] -- no shrinking
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
    -> Index 'WholeDomain 'CredFromKeyK
    -> Address
mkAddress (Rnd (RndState _ accIx _ _ _) rk pwd) addrIx =
    let
        acctKey = deriveAccountPrivateKey pwd rk (liftIndex accIx)
        addrKey = deriveAddressPrivateKey pwd acctKey addrIx
    in
        paymentAddress SMainnet (publicKey ByronKeyS addrKey)
