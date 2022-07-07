{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.MinimumUTxOSpec
    ( spec
    ) where

import Prelude

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
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
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
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Class instances obey laws" $ do
        testLawsMany @MinimumUTxO
            [ eqLaws
            , showLaws
            ]

    describe "computeMinimumCoinForUTxO" $ do
        it "prop_computeMinimumCoinForUTxO_evaluation" $
            prop_computeMinimumCoinForUTxO_evaluation
                & property
        it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds" $
            prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds
                & property
        it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability" $
            prop_computeMinimumCoinForUTxO_shelleyBasedEra_stability
                & property

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
    arbitrary = genTokenMap
    shrink = shrinkTokenMap
