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
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( chooseCoin, shrinkCoin )
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
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
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
import Data.IntCast
    ( intCast )
import Data.Word
    ( Word64 )
import Generics.SOP
    ( NP (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, property )
import Test.QuickCheck.Classes
    ( eqLaws, showLaws )
import Test.QuickCheck.Extra
    ( genericRoundRobinShrink, report, verify, (<:>), (<@>) )
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
        it "prop_computeMinimumCoinForUTxO" $
            prop_computeMinimumCoinForUTxO
                & property
        it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds" $
            prop_computeMinimumCoinForUTxO_shelleyBasedEra_bounds
                & property

prop_computeMinimumCoinForUTxO :: MinimumUTxO -> TokenMap -> Property
prop_computeMinimumCoinForUTxO minimumUTxO m = property $
    computeMinimumCoinForUTxO minimumUTxO m >= Coin 0

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
    arbitrary = TokenBundle
        <$> chooseCoin (Coin 0, Coin $ intCast @Word64 @Natural $ maxBound)
        <*> genTokenMap
    shrink = genericRoundRobinShrink
        <@> shrinkCoin
        <:> shrinkTokenMap
        <:> Nil

instance Arbitrary MinimumUTxO where
    arbitrary = genMinimumUTxO
    shrink = shrinkMinimumUTxO

instance Arbitrary MinimumUTxOForShelleyBasedEra where
    arbitrary = genMinimumUTxOForShelleyBasedEra
    shrink = shrinkMinimumUTxOForShelleyBasedEra

instance Arbitrary TokenMap where
    arbitrary = genTokenMap
    shrink = shrinkTokenMap
