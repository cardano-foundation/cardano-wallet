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
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO
    , MinimumUTxOForShelleyBasedEra (..)
    , minimumUTxOForShelleyBasedEra
    )
import Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( genMinimumUTxO
    , genMinimumUTxOForShelleyBasedEra
    , shrinkMinimumUTxO
    , shrinkMinimumUTxOForShelleyBasedEra
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
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..), txOutMaxTokenQuantity, txOutMinTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOutTokenBundle )
import Cardano.Wallet.Shelley.Compatibility
    ( toCardanoTxOut )
import Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO
    , maxLengthAddress
    , maxLengthCoin
    , unsafeLovelaceToWalletCoin
    , unsafeValueToLovelace
    )
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
    )
import Test.QuickCheck.Classes
    ( eqLaws, showLaws )
import Test.QuickCheck.Extra
    ( report, verify )
import Test.Utils.Laws
    ( testLawsMany )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
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

            it "prop_computeMinimumCoinForUTxO_evaluation" $
                prop_computeMinimumCoinForUTxO_evaluation
                    & property
            it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds" $
                prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds
                    & property
            it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability" $
                prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability
                    & property

        describe "Golden Tests" $ do

            goldenTests_computeMinimumCoinForUTxO "Shelley"
                goldenMinimumUTxO_Shelley
                goldenMinimumCoins_Shelley
            goldenTests_computeMinimumCoinForUTxO "Allegra"
                goldenMinimumUTxO_Allegra
                goldenMinimumCoins_Allegra
            goldenTests_computeMinimumCoinForUTxO "Mary"
                goldenMinimumUTxO_Mary
                goldenMinimumCoins_Mary
            goldenTests_computeMinimumCoinForUTxO "Alonzo"
                goldenMinimumUTxO_Alonzo
                goldenMinimumCoins_Alonzo
            goldenTests_computeMinimumCoinForUTxO "Babbage"
                goldenMinimumUTxO_Babbage
                goldenMinimumCoins_Babbage

-- Check that it's possible to evaluate 'computeMinimumCoinForUTxO' without
-- any run-time error.
--
prop_computeMinimumCoinForUTxO_evaluation
    :: MinimumUTxO -> TokenMap -> Property
prop_computeMinimumCoinForUTxO_evaluation minimumUTxO m = property $
    -- Use an arbitrary test to force evaluation of the result:
    computeMinimumCoinForUTxO minimumUTxO m >= Coin 0

-- Check that 'computeMinimumCoinForUTxO' produces a result that is within
-- bounds, as determined by the Cardano API function 'calculateMinimumUTxO'.
--
prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds
    :: TokenBundle
    -> Cardano.AddressAny
    -> MinimumUTxOForShelleyBasedEra
    -> Property
prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds
    tokenBundle addr (MinimumUTxOForShelleyBasedEra era pp) =
        let ourResult = ourComputeMinCoin
                (TokenBundle.tokens tokenBundle)
            apiResultMinBound = apiComputeMinCoin
                (fromCardanoAddressAny addr)
                (tokenBundle)
            apiResultMaxBound = apiComputeMinCoin
                (maxLengthAddress)
                (TokenBundle.setCoin tokenBundle maxLengthCoin)
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
                "BS.length (unAddress maxLengthAddress))"
  where
    -- Uses the Cardano API function 'calculateMinimumUTxO' to compute a
    -- minimum 'Coin' value.
    --
    apiComputeMinCoin :: Address -> TokenBundle -> Coin
    apiComputeMinCoin a b
        = either raiseApiError unsafeValueToWalletCoin
        $ Cardano.calculateMinimumUTxO era (toApiTxOut b)
        $ Cardano.fromLedgerPParams era pp
      where
        raiseApiError e = error $ unwords
            ["Failed to obtain result from Cardano API:", show e]
        toApiTxOut = toCardanoTxOut era . TxOut a
        unsafeValueToWalletCoin =
            (unsafeLovelaceToWalletCoin . unsafeValueToLovelace)

    -- Uses the wallet function 'computeMinimumCoinForUTxO' to compute a
    -- minimum 'Coin' value.
    --
    ourComputeMinCoin :: TokenMap -> Coin
    ourComputeMinCoin =
        computeMinimumCoinForUTxO (minimumUTxOForShelleyBasedEra era pp)

-- Compares the stability of:
--
-- - the Cardano API function 'calculateMinimumUTxO'
-- - the wallet function 'computeMinimumCoinForUTxO'
--
prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability
    :: TokenMap
    -> Cardano.AddressAny
    -> MinimumUTxOForShelleyBasedEra
    -> Property
prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability
    tokenMap addr (MinimumUTxOForShelleyBasedEra era pp) =
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
    apiComputeMinCoin b
        = either raiseApiError unsafeValueToWalletCoin
        $ Cardano.calculateMinimumUTxO era (toApiTxOut b)
        $ Cardano.fromLedgerPParams era pp
      where
        raiseApiError e = error $ unwords
            ["Failed to obtain result from Cardano API:", show e]
        toApiTxOut = toCardanoTxOut era . TxOut (fromCardanoAddressAny addr)
        unsafeValueToWalletCoin =
            (unsafeLovelaceToWalletCoin . unsafeValueToLovelace)

    -- Uses the wallet function 'computeMinimumCoinForUTxO' to compute a
    -- minimum 'Coin' value.
    --
    ourComputeMinCoin :: TokenMap -> Coin
    ourComputeMinCoin =
        computeMinimumCoinForUTxO (minimumUTxOForShelleyBasedEra era pp)

--------------------------------------------------------------------------------
-- Golden tests
--------------------------------------------------------------------------------

goldenTests_computeMinimumCoinForUTxO
    :: String
    -- ^ The era name.
    -> MinimumUTxO
    -- ^ The minimum UTxO function.
    -> [(TokenMap, Coin)]
    -- ^ Mappings from 'TokenMap' values to expected minimum 'Coin' values.
    -> Spec
goldenTests_computeMinimumCoinForUTxO
    eraName minimumUTxO expectedMinimumCoins =
        goldenTests title
            (uncurry computeMinimumCoinForUTxO)
            (mkTest <$> expectedMinimumCoins)
  where
    mkTest
        :: (TokenMap, Coin) -> GoldenTestData (MinimumUTxO, TokenMap) Coin
    mkTest (tokenMap, coinExpected) = GoldenTestData
        { params = (minimumUTxO, tokenMap)
        , resultExpected = coinExpected
        }
    title = unwords
        ["goldenTests_computeMinimumCoinForUTxO", eraName]

--------------------------------------------------------------------------------
-- Golden 'MinimumUTxO' values
--------------------------------------------------------------------------------

goldenMinimumUTxO_Shelley :: MinimumUTxO
goldenMinimumUTxO_Shelley =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraShelley
        -- Value derived from 'mainnet-shelley-genesis.json':
        def {Shelley._minUTxOValue = Ledger.Coin 1_000_000}

goldenMinimumUTxO_Allegra :: MinimumUTxO
goldenMinimumUTxO_Allegra =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraAllegra
        -- Value derived from 'mainnet-shelley-genesis.json':
        def {Shelley._minUTxOValue = Ledger.Coin 1_000_000}

goldenMinimumUTxO_Mary :: MinimumUTxO
goldenMinimumUTxO_Mary =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraMary
        -- Value derived from 'mainnet-shelley-genesis.json':
        def {Shelley._minUTxOValue = Ledger.Coin 1_000_000}

goldenMinimumUTxO_Alonzo :: MinimumUTxO
goldenMinimumUTxO_Alonzo =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraAlonzo
        -- Value derived from 'mainnet-alonzo-genesis.json':
        def {Alonzo._coinsPerUTxOWord = Ledger.Coin 34_482}

goldenMinimumUTxO_Babbage :: MinimumUTxO
goldenMinimumUTxO_Babbage =
    minimumUTxOForShelleyBasedEra ShelleyBasedEraBabbage
        -- Value derived from 'mainnet-alonzo-genesis.json':
        def {Babbage._coinsPerUTxOByte = Ledger.Coin 4_310}

--------------------------------------------------------------------------------
-- Golden minimum 'Coin' values
--------------------------------------------------------------------------------

goldenMinimumCoins_Shelley :: [(TokenMap, Coin)]
goldenMinimumCoins_Shelley =
    [ (goldenTokenMap_0, Coin 1_000_000)
    , (goldenTokenMap_1, Coin 1_000_000)
    , (goldenTokenMap_2, Coin 1_000_000)
    , (goldenTokenMap_3, Coin 1_000_000)
    , (goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_Allegra :: [(TokenMap, Coin)]
goldenMinimumCoins_Allegra =
    [ (goldenTokenMap_0, Coin 1_000_000)
    , (goldenTokenMap_1, Coin 1_000_000)
    , (goldenTokenMap_2, Coin 1_000_000)
    , (goldenTokenMap_3, Coin 1_000_000)
    , (goldenTokenMap_4, Coin 1_000_000)
    ]

goldenMinimumCoins_Mary :: [(TokenMap, Coin)]
goldenMinimumCoins_Mary =
    [ (goldenTokenMap_0, Coin 1_000_000)
    , (goldenTokenMap_1, Coin 1_444_443)
    , (goldenTokenMap_2, Coin 1_555_554)
    , (goldenTokenMap_3, Coin 1_740_739)
    , (goldenTokenMap_4, Coin 1_999_998)
    ]

goldenMinimumCoins_Alonzo :: [(TokenMap, Coin)]
goldenMinimumCoins_Alonzo =
    [ (goldenTokenMap_0, Coin   999_978)
    , (goldenTokenMap_1, Coin 1_344_798)
    , (goldenTokenMap_2, Coin 1_448_244)
    , (goldenTokenMap_3, Coin 1_620_654)
    , (goldenTokenMap_4, Coin 1_862_028)
    ]

goldenMinimumCoins_Babbage :: [(TokenMap, Coin)]
goldenMinimumCoins_Babbage =
    [ (goldenTokenMap_0, Coin   995_610)
    , (goldenTokenMap_1, Coin 1_150_770)
    , (goldenTokenMap_2, Coin 1_323_170)
    , (goldenTokenMap_3, Coin 1_323_170)
    , (goldenTokenMap_4, Coin 2_012_770)
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
