{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.MinimumUTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Api.Gen
    ( genAddressShelley )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( chooseCoin, shrinkCoin )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO
    , ProtocolParametersForShelleyBasedEra (..)
    , minimumUTxOForShelleyBasedEra
    )
import Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( genMinimumUTxO
    , genProtocolParametersForShelleyBasedEra
    , shrinkMinimumUTxO
    , shrinkProtocolParametersForShelleyBasedEra
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
    ( fromCardanoAddress, toCardanoTxOut )
import Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO
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
    ( genericRoundRobinShrink, report, (<:>), (<@>) )
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
        it "prop_computeMinimumCoinForUTxO_shelleyBasedEra_lowerBound" $
            prop_computeMinimumCoinForUTxO_shelleyBasedEra_lowerBound
                & property

prop_computeMinimumCoinForUTxO :: MinimumUTxO -> TokenMap -> Property
prop_computeMinimumCoinForUTxO minimumUTxO m = property $
    computeMinimumCoinForUTxO minimumUTxO m >= Coin 0

prop_computeMinimumCoinForUTxO_shelleyBasedEra_lowerBound
    :: TokenBundle
    -> Cardano.Address Cardano.ShelleyAddr
    -> ProtocolParametersForShelleyBasedEra
    -> Property
prop_computeMinimumCoinForUTxO_shelleyBasedEra_lowerBound
    tokenBundle addr (ProtocolParametersForShelleyBasedEra era pp) =
        case apiResultMaybe of
            Left e -> error $ unwords
                [ "Failed to obtain result from Cardano API:"
                , show e
                ]
            Right value -> prop_inner
                $ unsafeLovelaceToWalletCoin
                $ unsafeValueToLovelace value
  where
    prop_inner :: Coin -> Property
    prop_inner apiResult =
        ourResult >= apiResult
            & report
                (apiResult)
                "apiResult"
            & report
                (ourResult)
                "ourResult"
            & report
                (BS.length (Cardano.serialiseToRawBytes addr))
                "BS.length (Cardano.serialiseToRawBytes addr))"
            & report
                (BS.length (unAddress (fromCardanoAddress addr)))
                "BS.length (unAddress (fromCardanoAddress addr))"

    apiResultMaybe :: Either Cardano.MinimumUTxOError Cardano.Value
    apiResultMaybe =
        Cardano.calculateMinimumUTxO era apiTxOut apiProtocolParameters
      where
        apiTxOut =
            toCardanoTxOut era $
            TxOut (fromCardanoAddress addr) tokenBundle

        apiProtocolParameters :: Cardano.ProtocolParameters
        apiProtocolParameters =
            Cardano.fromLedgerPParams era pp

    ourResult :: Coin
    ourResult = computeMinimumCoinForUTxO
        (minimumUTxOForShelleyBasedEra era pp)
        (TokenBundle.tokens tokenBundle)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary (Cardano.Address Cardano.ShelleyAddr) where
    arbitrary = genAddressShelley

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

instance Arbitrary ProtocolParametersForShelleyBasedEra where
    arbitrary = genProtocolParametersForShelleyBasedEra
    shrink = shrinkProtocolParametersForShelleyBasedEra

instance Arbitrary TokenMap where
    arbitrary = genTokenMap
    shrink = shrinkTokenMap
