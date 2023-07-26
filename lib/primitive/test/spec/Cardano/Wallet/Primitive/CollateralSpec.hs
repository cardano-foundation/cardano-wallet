{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Primitive.CollateralSpec where

import Prelude

import Cardano.Wallet.Primitive.Collateral
    ( AddressType (..)
    , Credential (..)
    , addressSuitableForCollateral
    , addressType
    , addressTypeFromHeaderNibble
    , addressTypeSuitableForCollateral
    , addressTypeToHeaderNibble
    , asCollateral
    , getAddressType
    , putAddressType
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode )
import Control.Monad
    ( guard, replicateM_ )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust, isJust, isNothing )
import Numeric
    ( showHex )
import Test.Cardano.Ledger.Core.Arbitrary
    ()
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , cover
    , coverTable
    , disjoin
    , forAll
    , forAllShrink
    , frequency
    , oneof
    , property
    , tabulate
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- To begin with, we will write our generators and tests for the @AddressType@
-- type.
--
-- First we write our generator, which customizes its frequency so that every
-- branch of the AddressType type is evenly covered:

-- | Generate an AddressType.
genAddressType :: Gen AddressType
genAddressType =
    frequency
        [ (4, BaseAddress <$> genCredential <*> genCredential)
        , (2, PointerAddress <$> genCredential)
        , (2, EnterpriseAddress <$> genCredential)
        , (2, StakeAddress <$> genCredential)
        , (1, pure BootstrapAddress)
        ]

-- | Generate a credential.
genCredential :: Gen Credential
genCredential = oneof
    [ pure CredentialKeyHash
    , pure CredentialScriptHash
    ]

-- | Test that our generator covers every type of address, so we know our
-- property tests are sensible.
prop_genAddressType_coverage :: Property
prop_genAddressType_coverage =
    withMaxSuccess 1000 $
    forAll genAddressType $ \addrType ->
    coverTable "Address types"
        [ ("BaseAddress CredentialKeyHash CredentialKeyHash"       , 5)
        , ("BaseAddress CredentialKeyHash CredentialScriptHash"    , 5)
        , ("BaseAddress CredentialScriptHash CredentialKeyHash"    , 5)
        , ("BaseAddress CredentialScriptHash CredentialScriptHash" , 5)
        , ("PointerAddress CredentialKeyHash"                      , 5)
        , ("PointerAddress CredentialScriptHash"                   , 5)
        , ("EnterpriseAddress CredentialKeyHash"                   , 5)
        , ("EnterpriseAddress CredentialScriptHash"                , 5)
        , ("StakeAddress CredentialKeyHash"                        , 5)
        , ("StakeAddress CredentialScriptHash"                     , 5)
        , ("BootstrapAddress"                                      , 5)
        ] $
        tabulate "Address types" [show addrType] $ property True

-- | Test that address type header nibble encoding & decoding roundtrips
-- successfully.
prop_addressTypeHeaderNibble_roundtrips :: Property
prop_addressTypeHeaderNibble_roundtrips =
    forAll genAddressType $ \t ->
        addressTypeFromHeaderNibble (addressTypeToHeaderNibble t) === Just t

-- | Test that putting then getting an AddressType results in the original
-- AddressType (we can roundtrip successfully).
prop_header_roundtrips :: Property
prop_header_roundtrips =
    forAll genAddressType $ \addrType ->
        B.runGet getAddressType (B.runPut $ putAddressType addrType)
        === addrType

-- We an also write properties for the general types of addresses, and ensure
-- that we are classifying them correctly.

-- | Test that for any Byron address, we classify it as a Byron (a.k.a.
-- bootstrap) address.
prop_addressType_byron :: Property
prop_addressType_byron =
    forAll genByronAddr $ \byronAddr -> do
        let (Address addrBytes) = asAddress byronAddr
        B.runGet getAddressType (BL.fromStrict addrBytes) === BootstrapAddress

-- | Test that for any stake address, we classify it as a stake address
-- (although not necessarily the correct one, as it's a bit difficult to assert
-- that the credential type is correct, we do at least test that each type is
-- chosen sometimes using the coverage check).
prop_addressType_stake :: Property
prop_addressType_stake =
    forAll genStakeAddr $ \stakeAddr -> do
        let
            (Address addrBytes) = asStakeAddress stakeAddr
            addrType = B.runGet getAddressType (BL.fromStrict addrBytes)
        coverTable "Address types"
            [ ("StakeAddress CredentialKeyHash"   , 30)
            , ("StakeAddress CredentialScriptHash", 30)
            ] $
            tabulate "Address types" [show addrType] $
            disjoin
                [ addrType === StakeAddress CredentialKeyHash
                , addrType === StakeAddress CredentialScriptHash
                ]

-- | Test that for any shelley keyhash address, we classify it as a shelley
-- keyhash address (although not necessarily the correct one, as it's a bit
-- difficult to assert that the exact type is correct, we do at least test that
-- each type is chosen sometimes using the coverage check).
{-
prop_addressType_shelleyKeyHash :: Property
prop_addressType_shelleyKeyHash =
    forAll genShelleyKeyHashAddr $ \shelleyKeyHashAddr -> do
        let
            (Address addrBytes) = asAddress shelleyKeyHashAddr
            addrType = B.runGet getAddressType (BL.fromStrict addrBytes)
        coverTable "Address types"
            [ ("BaseAddress CredentialKeyHash CredentialKeyHash", 10)
            , ("BaseAddress CredentialKeyHash CredentialScriptHash", 10)
            , ("PointerAddress CredentialKeyHash", 10)
            , ("EnterpriseAddress CredentialKeyHash", 10)
            ] $
            tabulate "Address types" [show addrType] $
            disjoin
                [ addrType === BaseAddress CredentialKeyHash CredentialKeyHash
                , addrType === BaseAddress CredentialKeyHash CredentialScriptHash
                , addrType === PointerAddress CredentialKeyHash
                , addrType === EnterpriseAddress CredentialKeyHash
                ]
-}

-- | Test that for any shelley scripthash address, we classify it as a shelley
-- scripthash address (although not necessarily the correct one, as it's a bit
-- difficult to assert that the exact type is correct, we do at least test that
-- each type is chosen sometimes using the coverage check).
{-
prop_addressType_shelleyScriptHash :: Property
prop_addressType_shelleyScriptHash =
    forAll genShelleyScriptHashAddr $ \shelleyScriptHashAddr -> do
        let
            (Address addrBytes) = asAddress shelleyScriptHashAddr
            addrType = B.runGet getAddressType (BL.fromStrict addrBytes)
        coverTable "Address types"
            [ ("BaseAddress CredentialScriptHash CredentialKeyHash", 10)
            , ("BaseAddress CredentialScriptHash CredentialScriptHash", 10)
            , ("PointerAddress CredentialScriptHash", 10)
            , ("EnterpriseAddress CredentialScriptHash", 10)
            ] $
            tabulate "Address types" [show addrType] $
            disjoin
                [ addrType
                  === BaseAddress CredentialScriptHash CredentialKeyHash
                , addrType
                  === BaseAddress CredentialScriptHash CredentialScriptHash
                , addrType
                  === PointerAddress CredentialScriptHash
                , addrType
                  === EnterpriseAddress CredentialScriptHash
                ]
-}

-- To be extra sure, we also test our code with some golden addresses we
-- generated with "cardano-addresses":

unit_addressType_byronGolden :: Expectation
unit_addressType_byronGolden =
    B.runGet getAddressType byronAddrGolden `shouldBe` BootstrapAddress

unit_addressType_shelleyEnterprisePaymentGolden :: Expectation
unit_addressType_shelleyEnterprisePaymentGolden =
    B.runGet getAddressType shelleyEnterprisePaymentAddrGolden
        `shouldBe` EnterpriseAddress CredentialKeyHash

unit_addressType_stakeAddrGolden :: Expectation
unit_addressType_stakeAddrGolden =
    B.runGet getAddressType stakeAddrGolden
        `shouldBe` StakeAddress CredentialKeyHash

unit_addressType_pointerAddrGolden :: Expectation
unit_addressType_pointerAddrGolden =
    B.runGet getAddressType pointerAddrGolden
        `shouldBe` PointerAddress CredentialKeyHash

unit_addressType_delegationAddrGolden :: Expectation
unit_addressType_delegationAddrGolden =
    B.runGet getAddressType delegationAddrGolden
        `shouldBe` BaseAddress CredentialKeyHash CredentialKeyHash

-- TODO generate more unit tests for each type of address.

-- | Assert that @addressType@ and @getAdressType@ are the same.
prop_addressType_equivalance :: Property
prop_addressType_equivalance =
    forAllShrink genAnyAddress shrinkAddress $ \addr@(Address addrBytes) ->
        let
            addrType =
                case B.runGetOrFail getAddressType (BL.fromStrict addrBytes) of
                    Left _ ->
                        Nothing
                    Right (_, _, x) ->
                        Just x
        in
            addressType addr === addrType

-- The funds associated with an address are considered suitable for use as
-- collateral iff the payment credential column of that address is "key hash".
--
-- So, there are a few properties we would like to assert about our
-- "addressSuitableForCollateral" function:
--
-- 1. That our classification function always considers addresses with a keyhash
--    payment credential as suitable for collateral.
-- 2. That our classification function always considers addresses without a
--    keyhash payment credential as unsuitable for collateral.
-- 3. That future format addresses are always rejected.
--
-- That's it really. We also want to assert that our generators cover the full
-- range of possible address types, including future and unknown formats. So we
-- start by creating a generator that customizes its frequency to ensure that
-- every type of address is evenly covered (i.e. "genByronAddr" only generates
-- one type of address so is given lower frequency than "genShelleyAddr", which
-- must generate eight types of addresses). We also include Addresses that are
-- just an arbitrary set of bytes (very likely to be invalid).

-- | Generate an Address, covers the full range of address types plus invalid
-- addresses.
genAnyAddress :: Gen Address
genAnyAddress = frequency
    [ (10, asAddress <$> arbitrary)
    , (2, asStakeAddress <$> genStakeAddr)
    , (3, Address <$> arbitrary)
    ]

-- | Check that @genAnyAddress@ has sufficient coverage.
prop_genAddress_coverage :: Property
prop_genAddress_coverage =
    withMaxSuccess 1000 $
    forAll genAnyAddress $ \(Address addrBytes) -> do
        let addrType = runGetMaybe getAddressType $ BL.fromStrict addrBytes
        coverTable "Address types"
            [ ("Just (BaseAddress CredentialKeyHash CredentialKeyHash)"
              , 5)
            , ("Just (BaseAddress CredentialKeyHash CredentialScriptHash)"
              , 5)
            , ("Just (BaseAddress CredentialScriptHash CredentialKeyHash)"
              , 5)
            , ("Just (BaseAddress CredentialScriptHash CredentialScriptHash)"
              , 5)
            , ("Just (PointerAddress CredentialKeyHash)"
              , 5)
            , ("Just (PointerAddress CredentialScriptHash)"
              , 5)
            , ("Just (EnterpriseAddress CredentialKeyHash)"
              , 5)
            , ("Just (EnterpriseAddress CredentialScriptHash)"
              , 5)
            , ("Just (StakeAddress CredentialKeyHash)"
              , 5)
            , ("Just (StakeAddress CredentialScriptHash)"
              , 5)
            , ("Just BootstrapAddress"
              , 5)
            , ("Nothing"
              , 5)
            ] $
            tabulate "Address types" [show addrType] $ True === True

-- Using real addresses for the generators is an important idea, as the domain
-- of the classification function is the set of all addresses (really all
-- ByteStrings, thanks to our loose representation of Addresses, but we also
-- account for that in our generator). However, real addresses make for awful
-- counterexamples, so after generating an address, we try to determine what
-- kind of address it is by inspecting the first four bits: If it is an address
-- format we recognize, we shrink towards an address that has the same first
-- four bits, but 0 bits everywhere else. This forms a valid address that is
-- easier on the eyes, except in two cases: stake addresses and bootstrap
-- addresses. We can offer no shrinkage for those two kinds of addresses, but
-- try to provide a good explanation using `counterExample`. This is a helpful
-- strategy because at this current point in time collateral suitability can be
-- determined simply by inspecting the first four bits, so that is the most
-- important piece of information for debugging.
--
-- Here is an example of test output without shrinking:
-- Address "p\r\148\225ts.\249\170\231?9Z\180E\a\191\169\131\214P#\193\SUB\149\US\f2\228\204u\NUL\SOH"
-- Address hex: 71d94e174732ef9aae73f395ab4457bfa983d65023c11a951fc32e4cc7501
-- AddressType: Just (EnterpriseAddress CredentialScriptHash)
--
-- And here is an example of test output with shrinking:
-- Address "p\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
-- Address hex: 700000000000000000000000000000
-- AddressType: Just (EnterpriseAddress CredentialScriptHash)

-- | Attempt to simplify an address. Only shelley addresses can be simplified
-- and are simplified towards a binary encoding of addrType appended to a list
-- of null bytes, varying in length depending on the type of address.
simplifyAddress :: Address -> Maybe Address
simplifyAddress (Address addrBytes) = do
    -- Don't try to simplify malformed addresses or stake addresses. Note that
    -- "deserialiseAddr" will not parse stake addresses.
    _ <- L.deserialiseAddr addrBytes :: Maybe (L.Addr CC.StandardCrypto)

    bytes <- case runGetMaybe getAddressType (BL.fromStrict addrBytes) of
        Just BootstrapAddress ->
            -- We cannot easily simplify bootstrap addresses
            Nothing
        Just (StakeAddress _) ->
            -- We cannot easily simplify stake addresses
            Nothing
        Just addr@(BaseAddress _ _) -> do
            Just $ B.runPut $ do
                putAddressType addr
                -- payload for base addr is two 28-byte bytestrings
                putNullBytes 28
                putNullBytes 28
        Just addr@(PointerAddress _) ->
            Just $ B.runPut $ do
                putAddressType addr
                -- payload for pointer addr is one 28-byte bytestring followed
                -- by three unsigned ints of variable size (in this case one
                -- byte each).
                putNullBytes 28
                putNullBytes 3
        Just addr@(EnterpriseAddress _) ->
            Just $ B.runPut $ do
                putAddressType addr
                -- payload for enterprise addr is one 28-byte bytestring
                putNullBytes 28
        Nothing ->
            Nothing

    pure $ Address $ BL.toStrict bytes

    where
        -- Put n bytes worth of null bytes
        putNullBytes :: Int -> B.Put
        putNullBytes n = replicateM_ n putNullByte

        -- Put a byte of unset bits
        putNullByte :: B.Put
        putNullByte = B.putWord8 0b00000000

-- Of course, there are some properties we want to assert about this function.
-- When we simplify an address:
--
--   Given the address can be simplified,
--   - the simplified address is still a valid address
--   - the type of the simplified address matches the type of the original
--     address (address type is preserved)

-- | Assert that if an address can be simplified, the simplified address is
-- still a valid address.
prop_simplifyAddress_validAddress :: Property
prop_simplifyAddress_validAddress =
    forAll genAnyAddress $ \addr@(Address addrBytes) ->
        checkCoverage $
            cover 30 (isNothing $ simplifyAddress addr)
                "couldn't simplify address"  $
            cover 30 (isJust $ simplifyAddress addr)
                "could simplify address"  $
            case simplifyAddress addr of
                Nothing ->
                    property True
                Just (Address simplifiedBytes) ->
                    let
                        originalAddress :: Maybe (L.Addr CC.StandardCrypto)
                        originalAddress = L.deserialiseAddr addrBytes

                        simplifiedAddress :: Maybe (L.Addr CC.StandardCrypto)
                        simplifiedAddress = L.deserialiseAddr simplifiedBytes

                        commonErrorOutput :: Testable prop => prop -> Property
                        commonErrorOutput prop =
                            prop
                            & counterexample
                                ( "Simplified address type: "
                                  <> show (runGetMaybe getAddressType (BL.fromStrict addrBytes))
                                )
                            & counterexample
                                ( "Simplified: " <> show simplifiedAddress
                                 <> ", bytes (hex): "
                                 <> BS.foldr showHex "" simplifiedBytes
                                )
                            & counterexample
                                ( "Original: " <> show originalAddress
                                 <> ", bytes (hex): "
                                 <> BS.foldr showHex "" addrBytes
                                )
                    in
                        case (originalAddress, simplifiedAddress) of
                            (Nothing, _) ->
                                False
                                & commonErrorOutput
                                & counterexample
                                    ("Generator failed to generate valid address, bytes (hex): "
                                     <> BS.foldr showHex "" addrBytes
                                    )
                            (_, Nothing) ->
                                False
                                & commonErrorOutput
                                & counterexample ("Failed to parse simplified address")
                            (Just _, Just _) ->
                                property True

-- | Assert that if an address can be simplified, the type of the simplified
-- address matches the type of the original address.
prop_simplifyAddress_typeMaintained :: Property
prop_simplifyAddress_typeMaintained =
    forAll genAnyAddress $ \addr@(Address addrBytes) ->
        checkCoverage $
            cover 30 (isNothing $ simplifyAddress addr)
                "couldn't simplify address"  $
            cover 30 (isJust $ simplifyAddress addr)
                "could simplify address"  $
            case simplifyAddress addr of
                Nothing ->
                    property True
                Just (Address simplifiedBytes) ->
                    let
                        originalAddressType =
                            B.runGet getAddressType (BL.fromStrict addrBytes)

                        simplifiedAddressType =
                            B.runGet getAddressType (BL.fromStrict simplifiedBytes)
                    in
                        originalAddressType === simplifiedAddressType

-- From this function we can generate a QuickCheck shrinker:

-- | Try to shrink an address by simplifying it.
shrinkAddress :: Address -> [Address]
shrinkAddress addr =
    case simplifyAddress addr of
        Nothing ->
            -- There are some address types we can't meaningfully shrink.
            []
        Just simplified ->
            -- Otherwise we can shrink to the simplified address, so long as we
            -- actually changed the address
            [simplified | simplified /= addr]

-- With that out the way we can write our property test to ensure that
-- classifyCollateral address behaves as expected:

-- | Assert that, for any valid address, we only classify addresses with a key
-- hash payment credential as being suitable for collateral.
prop_addressSuitableForCollateral :: Property
prop_addressSuitableForCollateral =
    withMaxSuccess 2000 $
    forAllShrink genAnyAddress shrinkAddress $ \addr@(Address addrBytes) -> do
        let
            addrType = runGetMaybe getAddressType $ BL.fromStrict addrBytes
            validAddress = isValidAddress addr

        checkCoverage $
            cover 30 validAddress "valid address" $
            cover 10 (not validAddress) "invalid address" $
            case addrType of
                -- Only unrecognized addresses are classified as malformed
                -- or unknown (i.e. we otherwise classify any known address
                -- according to its type)
                Nothing ->
                    addressSuitableForCollateral addr === False

                -- Stake addresses are not suitable for collateral
                Just (StakeAddress _) ->
                    addressSuitableForCollateral addr === False

                -- Script addresses are not suitable for collateral
                Just (BaseAddress CredentialScriptHash _) ->
                    addressSuitableForCollateral addr === False
                Just (PointerAddress CredentialScriptHash) ->
                    addressSuitableForCollateral addr === False
                Just (EnterpriseAddress CredentialScriptHash) ->
                    addressSuitableForCollateral addr === False

                -- The following addresses all have a key hash payment
                -- credential and are thus suitable for collateral
                Just (BaseAddress CredentialKeyHash _) ->
                    addressSuitableForCollateral addr === True
                Just (PointerAddress CredentialKeyHash) ->
                    addressSuitableForCollateral addr === True
                Just (EnterpriseAddress CredentialKeyHash) ->
                    addressSuitableForCollateral addr === True
                Just BootstrapAddress ->
                    addressSuitableForCollateral addr === True

            & counterexample ("AddressType: " <> show addrType)
            & counterexample ("Address hex: " <> asHex addrBytes)

-- | Returns True if the given address parses as a known address.
isValidAddress :: Address -> Bool
isValidAddress (Address addrBytes) =
    isJust (L.deserialiseAddr addrBytes
        :: Maybe (L.Addr CC.StandardCrypto))
    ||
    isJust (L.deserialiseRewardAcnt addrBytes
        :: Maybe (L.RewardAcnt CC.StandardCrypto))

-- To be extra sure, we also test addressSuitableForCollateral with some golden
-- addresses:

unit_addressSuitableForCollateral_byronGolden :: Expectation
unit_addressSuitableForCollateral_byronGolden =
    let
        addr = Address . BL.toStrict $ byronAddrGolden
    in
        addressSuitableForCollateral addr `shouldBe` True

unit_addressSuitableForCollateral_shelleyEnterprisePaymentGolden :: Expectation
unit_addressSuitableForCollateral_shelleyEnterprisePaymentGolden =
    let
        addr = Address . BL.toStrict $ shelleyEnterprisePaymentAddrGolden
    in
        addressSuitableForCollateral addr `shouldBe` True

unit_addressSuitableForCollateral_stakeAddrGolden :: Expectation
unit_addressSuitableForCollateral_stakeAddrGolden =
    let
        addr = Address . BL.toStrict $ stakeAddrGolden
    in
        addressSuitableForCollateral addr `shouldBe` False

unit_addressSuitableForCollateral_pointerAddrGolden :: Expectation
unit_addressSuitableForCollateral_pointerAddrGolden =
    let
        addr = Address . BL.toStrict $ pointerAddrGolden
    in
        addressSuitableForCollateral addr `shouldBe` True

unit_addressSuitableForCollateral_delegationAddrGolden :: Expectation
unit_addressSuitableForCollateral_delegationAddrGolden =
    let
        addr = Address . BL.toStrict $ delegationAddrGolden
    in
        addressSuitableForCollateral addr `shouldBe` True

-- We wish to extend these properties to the "addressType" function, so we write
-- a simple equivalence property:
prop_addressSuitableForCollateral_equivalence :: Property
prop_addressSuitableForCollateral_equivalence =
    forAllShrink genAnyAddress shrinkAddress $ \addr ->
        maybe False addressTypeSuitableForCollateral (addressType addr)
        === addressSuitableForCollateral addr

-- We want to assert many of the same properties about "asCollateral" as we do
-- for "addressSuitableForCollateral". Rather than testing these properties
-- twice, we use the following logic:
--
--   - Given that the implementation of "addressSuitableForCollateral" is
--     correct,
--   - Given that the implementation of "TokenBundle.toCoin" is correct,
--   - and that "asCollateral" is equivalent to a simple composition of
--     "addressSuitableForCollateral" and "TokenBundle.toCoin",
--
-- We can say that the implementation of "asCollateral" is also correct, so
-- long as the composition operator is guaranteed not to change the properties
-- we are interested in. We can prove the equivalence like so:

-- | Assert that if the "composition" of "addressSuitableForCollateral" and
-- "TokenBundle.toCoin" returns, "asCollateral" should also return.
prop_equivalence_bool :: TxOut -> Property
prop_equivalence_bool txOut@(TxOut addr toks _) =
    isJust (asCollateral txOut)
    ===
    (addressSuitableForCollateral addr && TokenBundle.isCoin toks)

-- | Assert that the "asCollateral" function is equivalent to the "composition"
-- of "addressSuitableForCollateral" and "TokenBundle.toCoin".
prop_equivalence :: TxOut -> Property
prop_equivalence txOut@(TxOut addr toks _) =
    asCollateral txOut
    ===
    (guard (addressSuitableForCollateral addr) >> TokenBundle.toCoin toks)

-- The composition operator we are using here is the Maybe instance of (>>). The
-- guard lifts the Boolean to a Maybe, maintaining the falsity of
-- "addressSuitableForCollateral" (Nothing = False, Just = True).
-- From here, the composition operator discards the value inside the Maybe, and
-- so the next argument can only depend on the falsity of the Maybe (indeed, it
-- must). Thus the falsity of the properties is maintained (i.e. "asCollateral"
-- will accept/reject an UTxO correctly, so long as
-- "addressSuitableForCollateral" assesses an address correctly, which is tested
-- above). "asCollateral" will return the correct coin value so long as
-- "TokenBundle.toCoin" is working correctly (tested elsewhere).
--
-- I wish I knew how to formally prove things using category theory concepts
-- like monadic composition...

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRangePositive
    shrink = shrinkTokenBundleSmallRangePositive

instance Arbitrary TxOut where
    arbitrary = TxOut <$> genAnyAddress <*> arbitrary <*> pure Nothing

-- Finally we group the tests for easy execution

spec :: Spec
spec = do
    describe "address types" $ do
        describe "generators" $
            it "generates values with sufficient coverage" $
                property prop_genAddressType_coverage
        it "property prop_addressTypeHeaderNibble_roundtrips" $
            property prop_addressTypeHeaderNibble_roundtrips
        it "serialise/deserialise roundtrips" $
            property prop_header_roundtrips
        it "classifies byron address type correctly" $
            property prop_addressType_byron
        it "classifies stake address type correctly" $
            property prop_addressType_stake
        -- it "classifies shelley key hash type correctly" $
        --     property prop_addressType_shelleyKeyHash
        -- it "classifies shelley script hash type correctly" $
        --     property prop_addressType_shelleyScriptHash
        it "golden" $ do
            unit_addressType_byronGolden
            unit_addressType_shelleyEnterprisePaymentGolden
            unit_addressType_stakeAddrGolden
            unit_addressType_pointerAddrGolden
            unit_addressType_delegationAddrGolden
        describe "addressType" $ do
            it "satisfies same properties as getAddressType" $
                property prop_addressType_equivalance
    describe "collateral suitability" $ do
        describe "generators and shrinkers" $ do
            it "generates values with sufficient coverage" $
                property prop_genAddress_coverage
            it "shrink maintains validity" $
                property prop_simplifyAddress_validAddress
            it "shrink maintains type" $
                property prop_simplifyAddress_typeMaintained
        describe "addressSuitableForCollateral" $ do
            it "assesses all addresses correctly" $
                property prop_addressSuitableForCollateral
            it "golden" $ do
                unit_addressSuitableForCollateral_byronGolden
                unit_addressSuitableForCollateral_shelleyEnterprisePaymentGolden
                unit_addressSuitableForCollateral_stakeAddrGolden
                unit_addressSuitableForCollateral_pointerAddrGolden
                unit_addressSuitableForCollateral_delegationAddrGolden
        describe "addressTypeSuitableForCollateral" $ do
            it "satisfies same properties as addressSuitableForCollateral" $
                property prop_addressSuitableForCollateral_equivalence
        describe "asCollateral" $ do
            it "satisfies same boolean properties as addressSuitableForCollateral" $
                property prop_equivalence_bool
            it "satisfies same properties as addressSuitableForCollateral" $
                property prop_equivalence

-- The following golden keys were generated from the recovery phrase:
-- [change twin tired knee syrup cover dog glare canvas canvas jump egg]

-- cat recovery-phrase.txt | cardano-address key from-recovery-phrase Byron > root.prv
-- cat root.prv | cardano-address key child 14H/42H | tee addr.prv | cardano-address key public --with-chain-code | cardano-address address bootstrap --root $(cat root.prv | cardano-address key public --with-chain-code) --network-tag testnet 14H/42H
byronAddrGolden :: BL.ByteString
byronAddrGolden = BL.fromStrict . fromJust . decodeBase58 bitcoinAlphabet $
    "37btjrVyb4KFsMoVwPRZ5aJko48uBFFUnJ46eV3vC3uBCC65mj5BfbGP6jYDfhojm8MAayHo4RPvWH4x852FcJq8SHazCx31FJM2TfDpV9Azrc8UKD"

-- cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
-- cat root.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv
-- cat stake.prv | cardano-address key public --with-chain-code | cardano-address address stake --network-tag testnet
stakeAddrGolden :: BL.ByteString
stakeAddrGolden = unsafeBech32Decode
    "stake_test1uztjkmlcknuv29pwuwd8wsk54q5eus56flqs4xy730yvnust8pvfj"

-- cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
-- cat root.prv | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv
-- cat addr.prv | cardano-address key public --with-chain-code | cardano-address address payment --network-tag 0 | cardano-address address pointer 42 14 0
pointerAddrGolden :: BL.ByteString
pointerAddrGolden  = unsafeBech32Decode
    "addr_test1gpdylg53ekxh2404mfgw4pt4gfm7dc9dkc74ck0gtrld8up2pcqqefucl2"

-- cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
-- cat root.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv
-- cat root.prv | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv
-- cat addr.prv | cardano-address key public --with-chain-code | cardano-address address payment --network-tag testnet | cardano-address address delegation $(cat stake.prv | cardano-address key public --with-chain-code)
delegationAddrGolden :: BL.ByteString
delegationAddrGolden = unsafeBech32Decode
    "addr_test1qpdylg53ekxh2404mfgw4pt4gfm7dc9dkc74ck0gtrld8uyh9dhl3d8cc52zacu6wapdf2pfnepf5n7pp2vfaz7ge8eqd4nn9s"

-- cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley > root.prv
-- cat root.prv | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv
-- cat addr.prv | cardano-address key public --with-chain-code | cardano-address address payment --network-tag testnet
shelleyEnterprisePaymentAddrGolden :: BL.ByteString
shelleyEnterprisePaymentAddrGolden = unsafeBech32Decode
    "addr_test1vpdylg53ekxh2404mfgw4pt4gfm7dc9dkc74ck0gtrld8uqewynck"

genByronAddr :: Gen (L.Addr CC.StandardCrypto)
genByronAddr = L.AddrBootstrap <$> arbitrary

genStakeAddr :: Gen (L.RewardAcnt CC.StandardCrypto)
genStakeAddr = arbitrary

-- Some helper functions

asAddress :: L.Addr CC.StandardCrypto -> Address
asAddress = Address . L.serialiseAddr

asStakeAddress :: L.RewardAcnt crypto -> Address
asStakeAddress = Address . L.serialiseRewardAcnt

runGetMaybe :: B.Get a -> BL.ByteString -> Maybe a
runGetMaybe parser x =
    either (const Nothing) (\(_, _, a) -> Just a) $ B.runGetOrFail parser x

asHex :: ByteString -> String
asHex = BS.foldr showHex ""
