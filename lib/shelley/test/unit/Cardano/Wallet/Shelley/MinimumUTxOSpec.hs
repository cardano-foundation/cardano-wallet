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
    ( Arbitrary (..), Property, property, sized )
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
        case apiResultBoundsM of
            Left e -> error $ unwords
                [ "Failed to obtain result from Cardano API:"
                , show e
                ]
            Right apiResultBounds -> prop_inner apiResultBounds
  where
    prop_inner :: (Coin, Coin) -> Property
    prop_inner (apiResultMinBound, apiResultMaxBound) = property True
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

    apiResultBoundsM :: Either Cardano.MinimumUTxOError (Coin, Coin)
    apiResultBoundsM = (,)
        <$> apiCalculateMinimumUTxO apiTxOutMinBound
        <*> apiCalculateMinimumUTxO apiTxOutMaxBound
      where
        apiCalculateMinimumUTxO tx =
            fmap (unsafeLovelaceToWalletCoin . unsafeValueToLovelace) $
            Cardano.calculateMinimumUTxO era tx $
            Cardano.fromLedgerPParams era pp

        apiTxOutMinBound =
            toCardanoTxOut era $ TxOut (fromCardanoAddressAny addr) tokenBundle

        apiTxOutMaxBound =
            toCardanoTxOut era $ TxOut maxLengthAddress $
            TokenBundle.setCoin tokenBundle maxLengthCoin

    ourResult :: Coin
    ourResult = computeMinimumCoinForUTxO
        (minimumUTxOForShelleyBasedEra era pp)
        (TokenBundle.tokens tokenBundle)

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
