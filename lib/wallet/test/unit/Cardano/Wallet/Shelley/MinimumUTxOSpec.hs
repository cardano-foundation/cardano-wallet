{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Shelley.MinimumUTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Api
    ( ShelleyBasedEra (..) )
import Cardano.Api.Gen
    ( genAddressAny )
import Cardano.Ledger.Core
    ( ppMinUTxOValueL )
import Cardano.Wallet.Address.Keys.BoundedAddressLength
    ( maxLengthAddressFor )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Constants
    ( maxLengthAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..)
    , MinimumUTxOForShelleyBasedEra (..)
    , minimumUTxOForShelleyBasedEra
    )
import Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( genMinimumUTxO
    , genMinimumUTxOForShelleyBasedEra
    , shrinkMinimumUTxO
    , shrinkMinimumUTxOForShelleyBasedEra
    , testParameter_coinsPerUTxOByte_Babbage
    , testParameter_coinsPerUTxOWord_Alonzo
    , testParameter_minUTxOValue_Allegra
    , testParameter_minUTxOValue_Mary
    , testParameter_minUTxOValue_Shelley
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( shrinkTokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName) )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( mkTokenPolicyId )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin, txOutMaxTokenQuantity, txOutMinTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutTokenBundle )
import Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO, isBelowMinimumCoinForUTxO )
import Control.Lens
    ( (.~) )
import Control.Monad
    ( forM_ )
import Data.Default
    ( Default (..) )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , conjoin
    , cover
    , elements
    , frequency
    , property
    , sized
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws, showLaws )
import Test.QuickCheck.Extra
    ( report, verify )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Api as Babbage
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Shelley.MinimumUTxO.Internal as Internal
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Class instances obey laws" $ do
        testLawsMany @MinimumUTxO
            [ eqLaws
            , showLaws
            ]

    describe "computeMinimumCoinForUTxO" $ do

        describe "Properties" $ do

            it "prop_computeMinimumCoinForUTxO_isBelowMinimumCoinForUTxO" $
                prop_computeMinimumCoinForUTxO_isBelowMinimumCoinForUTxO
                    & property
            it "prop_computeMinimumCoinForUTxO_bounds" $
                prop_computeMinimumCoinForUTxO_bounds
                    & property
            it "prop_computeMinimumCoinForUTxO_stability" $
                prop_computeMinimumCoinForUTxO_stability
                    & property

        describe "Golden Tests" $ do

            describe "Byron-style addresses" $ do

                goldenTests_computeMinimumCoinForUTxO
                    "Shelley"
                    goldenMinimumUTxO_ShelleyEra
                    goldenMinimumCoins_ByronAddress_ShelleyEra
                goldenTests_computeMinimumCoinForUTxO
                    "Allegra"
                    goldenMinimumUTxO_AllegraEra
                    goldenMinimumCoins_ByronAddress_AllegraEra
                goldenTests_computeMinimumCoinForUTxO
                    "Mary"
                    goldenMinimumUTxO_MaryEra
                    goldenMinimumCoins_ByronAddress_MaryEra
                goldenTests_computeMinimumCoinForUTxO
                    "Alonzo"
                    goldenMinimumUTxO_AlonzoEra
                    goldenMinimumCoins_ByronAddress_AlonzoEra
                goldenTests_computeMinimumCoinForUTxO
                    "Babbage"
                    goldenMinimumUTxO_BabbageEra
                    goldenMinimumCoins_ByronAddress_BabbageEra

            describe "Shelley-style addresses" $ do

                goldenTests_computeMinimumCoinForUTxO
                    "Shelley"
                    goldenMinimumUTxO_ShelleyEra
                    goldenMinimumCoins_ShelleyAddress_ShelleyEra
                goldenTests_computeMinimumCoinForUTxO
                    "Allegra"
                    goldenMinimumUTxO_AllegraEra
                    goldenMinimumCoins_ShelleyAddress_AllegraEra
                goldenTests_computeMinimumCoinForUTxO
                    "Mary"
                    goldenMinimumUTxO_MaryEra
                    goldenMinimumCoins_ShelleyAddress_MaryEra
                goldenTests_computeMinimumCoinForUTxO
                    "Alonzo"
                    goldenMinimumUTxO_AlonzoEra
                    goldenMinimumCoins_ShelleyAddress_AlonzoEra
                goldenTests_computeMinimumCoinForUTxO
                    "Babbage"
                    goldenMinimumUTxO_BabbageEra
                    goldenMinimumCoins_ShelleyAddress_BabbageEra

-- Tests the following composition:
--
-- >>> isBelowMinimumCoinForUTxO . computeMinimumCoinForUTxO
--
-- The composition should never be `True`.
--
prop_computeMinimumCoinForUTxO_isBelowMinimumCoinForUTxO
    :: MinimumUTxO -> Address -> TokenMap -> Property
prop_computeMinimumCoinForUTxO_isBelowMinimumCoinForUTxO minimumUTxO addr m =
    isBelowMinimumCoinForUTxO minimumUTxO addr
        (TokenBundle (computeMinimumCoinForUTxO minimumUTxO addr m) m)
    === False

-- Check that 'computeMinimumCoinForUTxO' produces a result that is within
-- bounds, as determined by the Cardano API function 'calculateMinimumUTxO'.
--
prop_computeMinimumCoinForUTxO_bounds
    :: TokenBundle
    -> Cardano.AddressAny
    -> MinimumUTxOForShelleyBasedEra
    -> Property
prop_computeMinimumCoinForUTxO_bounds
    tokenBundle addr minimumUTxO =
        let ourResult = ourComputeMinCoin
                (fromCardanoAddressAny addr)
                (TokenBundle.tokens tokenBundle)
            apiResultMinBound = apiComputeMinCoin
                (fromCardanoAddressAny addr)
                (tokenBundle)
            apiResultMaxBound = apiComputeMinCoin
                (maxLengthAddress)
                (TokenBundle.setCoin tokenBundle txOutMaxCoin)
        in
        property True
            & verify
                (ourResult >= apiResultMinBound)
                "ourResult >= apiResultMinBound"
            & verify
                (ourResult <= apiResultMaxBound)
                "ourResult <= apiResultMaxBound"
            & report
                (apiResultMinBound)
                "apiResultMinBound"
            & report
                (apiResultMaxBound)
                "apiResultMaxBound"
            & report
                (ourResult)
                "ourResult"
            & report
                (BS.length (Cardano.serialiseToRawBytes addr))
                "BS.length (Cardano.serialiseToRawBytes addr))"
            & report
                (BS.length (unAddress (fromCardanoAddressAny addr)))
                "BS.length (unAddress (fromCardanoAddressAny addr))"
            & report
                (BS.length (unAddress maxLengthAddress))
                "BS.length (unAddress maxLengthAddress)"
  where
    -- Uses the Cardano API function 'calculateMinimumUTxO' to compute a
    -- minimum 'Coin' value.
    --
    apiComputeMinCoin :: Address -> TokenBundle -> Coin
    apiComputeMinCoin a b = Internal.computeMinimumCoinForUTxO_CardanoApi
        minimumUTxO (TxOut a b)

    -- Uses the wallet function 'computeMinimumCoinForUTxO' to compute a
    -- minimum 'Coin' value.
    --
    ourComputeMinCoin :: Address -> TokenMap -> Coin
    ourComputeMinCoin = computeMinimumCoinForUTxO
        (MinimumUTxOForShelleyBasedEraOf minimumUTxO)

-- Compares the stability of:
--
-- - the Cardano API function 'calculateMinimumUTxO'
-- - the wallet function 'computeMinimumCoinForUTxO'
--
-- In particular, we:
--
-- Demonstrate that applying the Cardano API function to its own result can
-- lead to an increase in the ada quantity.
--
-- Demonstrate that applying the Cardano API function to the result of the
-- wallet function does not lead to an increase in the ada quantity.
--
prop_computeMinimumCoinForUTxO_stability
    :: TokenMap
    -> Cardano.AddressAny
    -> MinimumUTxOForShelleyBasedEra
    -> Property
prop_computeMinimumCoinForUTxO_stability
    tokenMap addr minimumUTxO =
        conjoin
            [ prop_apiFunctionStability
            , prop_ourFunctionStability
            ]
  where
    -- Demonstrate that applying the Cardano API function to its own result can
    -- lead to an increase in the ada quantity.
    --
    prop_apiFunctionStability :: Property
    prop_apiFunctionStability =
        let apiResult0 = apiComputeMinCoin $ TokenBundle (Coin 0)   tokenMap
            apiResult1 = apiComputeMinCoin $ TokenBundle apiResult0 tokenMap
        in
        property True
            & verify   (apiResult0 <= apiResult1) "apiResult0 <= apiResult1"
            & cover 10 (apiResult0 == apiResult1) "apiResult0 == apiResult1"
            & cover 10 (apiResult0  < apiResult1) "apiResult0  < apiResult1"
            & report apiResult0 "apiResult0"
            & report apiResult1 "apiResult1"
            & checkCoverage

    -- Demonstrate that applying the Cardano API function to the result of the
    -- wallet function does not lead to an increase in the ada quantity.
    --
    prop_ourFunctionStability :: Property
    prop_ourFunctionStability =
        let ourResult0 = ourComputeMinCoin                          tokenMap
            ourResult1 = apiComputeMinCoin $ TokenBundle ourResult0 tokenMap
        in
        property True
            & verify   (ourResult0 >= ourResult1) "ourResult0 >= ourResult1"
            & cover 10 (ourResult0 == ourResult1) "ourResult0 == ourResult1"
            & cover 10 (ourResult0  > ourResult1) "ourResult0  > ourResult1"
            & report ourResult0 "ourResult0"
            & report ourResult1 "ourResult1"
            & checkCoverage

    -- Uses the Cardano API function 'calculateMinimumUTxO' to compute a
    -- minimum 'Coin' value.
    --
    apiComputeMinCoin :: TokenBundle -> Coin
    apiComputeMinCoin b = Internal.computeMinimumCoinForUTxO_CardanoApi
        minimumUTxO (TxOut (fromCardanoAddressAny addr) b)

    -- Uses the wallet function 'computeMinimumCoinForUTxO' to compute a
    -- minimum 'Coin' value.
    --
    ourComputeMinCoin :: TokenMap -> Coin
    ourComputeMinCoin = computeMinimumCoinForUTxO
        (MinimumUTxOForShelleyBasedEraOf minimumUTxO)
        (fromCardanoAddressAny addr)

--------------------------------------------------------------------------------
-- Golden tests
--------------------------------------------------------------------------------

goldenTests_computeMinimumCoinForUTxO
    :: String
    -- ^ The era name.
    -> MinimumUTxO
    -- ^ The minimum UTxO function.
    -> [(Address, TokenMap, Coin)]
    -- ^ Mappings from 'TokenMap' values to expected minimum 'Coin' values.
    -> Spec
goldenTests_computeMinimumCoinForUTxO
    testName minimumUTxO expectedMinimumCoins =
        goldenTests title
            (\(minUTxO, addr, m) -> computeMinimumCoinForUTxO minUTxO addr m)
            (mkTest <$> expectedMinimumCoins)
  where
    mkTest
        :: (Address, TokenMap, Coin)
        -> GoldenTestData (MinimumUTxO, Address, TokenMap) Coin
    mkTest (addr, tokenMap, coinExpected) = GoldenTestData
        { params = (minimumUTxO, addr, tokenMap)
        , resultExpected = coinExpected
        }
    title = unwords
        ["goldenTests_computeMinimumCoinForUTxO:", testName]

--------------------------------------------------------------------------------
-- Golden 'MinimumUTxO' values
--------------------------------------------------------------------------------

goldenMinimumUTxO_ShelleyEra :: MinimumUTxO
goldenMinimumUTxO_ShelleyEra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraShelley $
        def & ppMinUTxOValueL .~ testParameter_minUTxOValue_Shelley

goldenMinimumUTxO_AllegraEra :: MinimumUTxO
goldenMinimumUTxO_AllegraEra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraAllegra $
        def & ppMinUTxOValueL .~ testParameter_minUTxOValue_Allegra

goldenMinimumUTxO_MaryEra :: MinimumUTxO
goldenMinimumUTxO_MaryEra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraMary $
        def & ppMinUTxOValueL .~ testParameter_minUTxOValue_Mary

goldenMinimumUTxO_AlonzoEra :: MinimumUTxO
goldenMinimumUTxO_AlonzoEra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraAlonzo
        $ def
        & Alonzo.ppCoinsPerUTxOWordL .~ testParameter_coinsPerUTxOWord_Alonzo

goldenMinimumUTxO_BabbageEra :: MinimumUTxO
goldenMinimumUTxO_BabbageEra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraBabbage
        $ def
        & Babbage.ppCoinsPerUTxOByteL .~ testParameter_coinsPerUTxOByte_Babbage

--------------------------------------------------------------------------------
-- Golden minimum 'Coin' values: Byron-style addresses
--------------------------------------------------------------------------------

maxLengthAddressBryon :: Address
maxLengthAddressBryon = maxLengthAddressFor ByronKeyS

goldenMinimumCoins_ByronAddress_ShelleyEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ByronAddress_ShelleyEra =
    [ (maxLengthAddressBryon, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_1, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_2, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_3, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_ByronAddress_AllegraEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ByronAddress_AllegraEra =
    [ (maxLengthAddressBryon, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_1, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_2, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_3, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_ByronAddress_MaryEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ByronAddress_MaryEra =
    [ (maxLengthAddressBryon, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressBryon, goldenTokenMap_1, Coin 1_444_443)
    , (maxLengthAddressBryon, goldenTokenMap_2, Coin 1_555_554)
    , (maxLengthAddressBryon, goldenTokenMap_3, Coin 1_740_739)
    , (maxLengthAddressBryon, goldenTokenMap_4, Coin 1_999_998)
    ]

goldenMinimumCoins_ByronAddress_AlonzoEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ByronAddress_AlonzoEra =
    [ (maxLengthAddressBryon, goldenTokenMap_0, Coin   999_978)
    , (maxLengthAddressBryon, goldenTokenMap_1, Coin 1_344_798)
    , (maxLengthAddressBryon, goldenTokenMap_2, Coin 1_448_244)
    , (maxLengthAddressBryon, goldenTokenMap_3, Coin 1_620_654)
    , (maxLengthAddressBryon, goldenTokenMap_4, Coin 1_862_028)
    ]

goldenMinimumCoins_ByronAddress_BabbageEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ByronAddress_BabbageEra =
    [ (maxLengthAddressBryon, goldenTokenMap_0, Coin 1_107_670)
    , (maxLengthAddressBryon, goldenTokenMap_1, Coin 1_262_830)
    , (maxLengthAddressBryon, goldenTokenMap_2, Coin 1_435_230)
    , (maxLengthAddressBryon, goldenTokenMap_3, Coin 1_435_230)
    , (maxLengthAddressBryon, goldenTokenMap_4, Coin 2_124_830)
    ]

--------------------------------------------------------------------------------
-- Golden minimum 'Coin' values: Shelley-style addresses
--------------------------------------------------------------------------------

maxLengthAddressShelley :: Address
maxLengthAddressShelley = maxLengthAddressFor ShelleyKeyS

goldenMinimumCoins_ShelleyAddress_ShelleyEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ShelleyAddress_ShelleyEra =
    [ (maxLengthAddressShelley, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_1, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_2, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_3, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_ShelleyAddress_AllegraEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ShelleyAddress_AllegraEra =
    [ (maxLengthAddressShelley, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_1, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_2, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_3, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_ShelleyAddress_MaryEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ShelleyAddress_MaryEra =
    [ (maxLengthAddressShelley, goldenTokenMap_0, Coin 1_000_000)
    , (maxLengthAddressShelley, goldenTokenMap_1, Coin 1_444_443)
    , (maxLengthAddressShelley, goldenTokenMap_2, Coin 1_555_554)
    , (maxLengthAddressShelley, goldenTokenMap_3, Coin 1_740_739)
    , (maxLengthAddressShelley, goldenTokenMap_4, Coin 1_999_998)
    ]

goldenMinimumCoins_ShelleyAddress_AlonzoEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ShelleyAddress_AlonzoEra =
    [ (maxLengthAddressShelley, goldenTokenMap_0, Coin   999_978)
    , (maxLengthAddressShelley, goldenTokenMap_1, Coin 1_344_798)
    , (maxLengthAddressShelley, goldenTokenMap_2, Coin 1_448_244)
    , (maxLengthAddressShelley, goldenTokenMap_3, Coin 1_620_654)
    , (maxLengthAddressShelley, goldenTokenMap_4, Coin 1_862_028)
    ]

goldenMinimumCoins_ShelleyAddress_BabbageEra :: [(Address, TokenMap, Coin)]
goldenMinimumCoins_ShelleyAddress_BabbageEra =
    [ (maxLengthAddressShelley, goldenTokenMap_0, Coin   995_610)
    , (maxLengthAddressShelley, goldenTokenMap_1, Coin 1_150_770)
    , (maxLengthAddressShelley, goldenTokenMap_2, Coin 1_323_170)
    , (maxLengthAddressShelley, goldenTokenMap_3, Coin 1_323_170)
    , (maxLengthAddressShelley, goldenTokenMap_4, Coin 2_012_770)
    ]

--------------------------------------------------------------------------------
-- Golden 'TokenMap' values
--------------------------------------------------------------------------------

goldenTokenMaps :: [TokenMap]
goldenTokenMaps =
    [ goldenTokenMap_0
    , goldenTokenMap_1
    , goldenTokenMap_2
    , goldenTokenMap_3
    , goldenTokenMap_4
    ]

goldenTokenMap_0 :: TokenMap
goldenTokenMap_0 = TokenMap.empty

goldenTokenMap_1 :: TokenMap
goldenTokenMap_1 = TokenMap.fromFlatList
    [ (goldenAssetId_A_1_short, txOutMinTokenQuantity)
    ]

goldenTokenMap_2 :: TokenMap
goldenTokenMap_2 = TokenMap.fromFlatList
    [ (goldenAssetId_A_1_long, txOutMaxTokenQuantity)
    ]

goldenTokenMap_3 :: TokenMap
goldenTokenMap_3 = TokenMap.fromFlatList
    [ (goldenAssetId_A_1_short, txOutMinTokenQuantity)
    , (goldenAssetId_A_2_short, txOutMinTokenQuantity)
    , (goldenAssetId_B_1_short, txOutMinTokenQuantity)
    , (goldenAssetId_B_2_short, txOutMinTokenQuantity)
    ]

goldenTokenMap_4 :: TokenMap
goldenTokenMap_4 = TokenMap.fromFlatList
    [ (goldenAssetId_A_1_long, txOutMaxTokenQuantity)
    , (goldenAssetId_A_2_long, txOutMaxTokenQuantity)
    , (goldenAssetId_B_1_long, txOutMaxTokenQuantity)
    , (goldenAssetId_B_2_long, txOutMaxTokenQuantity)
    ]

--------------------------------------------------------------------------------
-- Golden 'AssetId' values
--------------------------------------------------------------------------------

goldenAssetId_A_1_short :: AssetId
goldenAssetId_A_1_short = mkAssetId 'A' "1"

goldenAssetId_A_2_short :: AssetId
goldenAssetId_A_2_short = mkAssetId 'A' "2"

goldenAssetId_B_1_short :: AssetId
goldenAssetId_B_1_short = mkAssetId 'B' "1"

goldenAssetId_B_2_short :: AssetId
goldenAssetId_B_2_short = mkAssetId 'B' "2"

goldenAssetId_A_1_long :: AssetId
goldenAssetId_A_1_long = mkAssetId 'A' (replicate 32 '1')

goldenAssetId_A_2_long :: AssetId
goldenAssetId_A_2_long = mkAssetId 'A' (replicate 32 '2')

goldenAssetId_B_1_long :: AssetId
goldenAssetId_B_1_long = mkAssetId 'B' (replicate 32 '1')

goldenAssetId_B_2_long :: AssetId
goldenAssetId_B_2_long = mkAssetId 'B' (replicate 32 '2')

mkAssetId :: Char -> String -> AssetId
mkAssetId pid name = AssetId
    (mkTokenPolicyId pid)
    (UnsafeTokenName $ T.encodeUtf8 $ T.pack name)

--------------------------------------------------------------------------------
-- Golden test support
--------------------------------------------------------------------------------

data GoldenTestData params result = GoldenTestData
    { params :: params
    , resultExpected :: result
    }
    deriving (Eq, Show)

goldenTests
    :: (Eq result, Show result)
    => String
    -> (params -> result)
    -> [GoldenTestData params result]
    -> Spec
goldenTests title f goldenTestData =
    describe title $
    forM_ (zip testNumbers goldenTestData) $ \(testNumber, testData) -> do
        let subtitle = "golden test #" <> show testNumber
        it subtitle $ do
            let GoldenTestData {params, resultExpected} = testData
            let resultReturned = f params
            property True
                & verify
                    (resultReturned == resultExpected)
                    "resultReturned == resultExpected"
                & report resultReturned "resultReturned"
                & report resultExpected "resultExpected"
  where
    testNumbers :: [Int]
    testNumbers = [0 ..]

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

fromCardanoAddressAny :: Cardano.AddressAny -> Address
fromCardanoAddressAny =  Address . Cardano.serialiseToRawBytes

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Cardano.AddressAny where
    arbitrary = genAddressAny

instance Arbitrary Address where
    arbitrary = fromCardanoAddressAny <$> arbitrary

instance Arbitrary TokenBundle where
    arbitrary = sized genTxOutTokenBundle
    shrink = shrinkTokenBundle

instance Arbitrary MinimumUTxO where
    arbitrary = genMinimumUTxO
    shrink = shrinkMinimumUTxO

instance Arbitrary MinimumUTxOForShelleyBasedEra where
    arbitrary = genMinimumUTxOForShelleyBasedEra
    shrink = shrinkMinimumUTxOForShelleyBasedEra

instance Arbitrary TokenMap where
    arbitrary = frequency
        [ (4, genTokenMap)
        , (1, elements goldenTokenMaps)
        ]
    shrink = shrinkTokenMap
