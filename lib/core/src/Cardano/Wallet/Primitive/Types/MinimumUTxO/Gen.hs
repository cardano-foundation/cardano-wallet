{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Defines generators and shrinkers for the 'MinimumUTxO' data type.
--
module Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( genMinimumUTxO
    , genMinimumUTxOForShelleyBasedEra
    , shrinkMinimumUTxO
    , shrinkMinimumUTxOForShelleyBasedEra
    )
    where

import Prelude

import Cardano.Api
    ( ShelleyBasedEra (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..), MinimumUTxOForShelleyBasedEra (..) )
import Data.Bits
    ( Bits )
import Data.Default
    ( Default (..) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen, choose, frequency, oneof )

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Shelley

--------------------------------------------------------------------------------
-- Generating 'MinimumUTxO' values
--------------------------------------------------------------------------------

genMinimumUTxO :: Gen MinimumUTxO
genMinimumUTxO = frequency
    [ (1, genMinimumUTxONone)
    , (1, genMinimumUTxOConstant)
    , (8, MinimumUTxOForShelleyBasedEraOf <$> genMinimumUTxOForShelleyBasedEra)
    ]
  where
    genMinimumUTxONone :: Gen MinimumUTxO
    genMinimumUTxONone = pure MinimumUTxONone

    genMinimumUTxOConstant :: Gen MinimumUTxO
    genMinimumUTxOConstant = MinimumUTxOConstant . Coin
        <$> genInterestingCoinValue

shrinkMinimumUTxO :: MinimumUTxO -> [MinimumUTxO]
shrinkMinimumUTxO = const []

--------------------------------------------------------------------------------
-- Generating 'MinimumUTxOForShelleyBasedEra' values
--------------------------------------------------------------------------------

genMinimumUTxOForShelleyBasedEra
    :: Gen MinimumUTxOForShelleyBasedEra
genMinimumUTxOForShelleyBasedEra = oneof
    [ genShelley
    , genAllegra
    , genMary
    , genAlonzo
    , genBabbage
    ]
  where
    genShelley :: Gen MinimumUTxOForShelleyBasedEra
    genShelley = do
        minUTxOValue <- genInterestingLedgerCoin
        pure $ MinimumUTxOForShelleyBasedEra ShelleyBasedEraShelley
            def {Shelley._minUTxOValue = minUTxOValue}

    genAllegra :: Gen MinimumUTxOForShelleyBasedEra
    genAllegra = do
        minUTxOValue <- genInterestingLedgerCoin
        pure $ MinimumUTxOForShelleyBasedEra ShelleyBasedEraAllegra
            def {Shelley._minUTxOValue = minUTxOValue}

    genMary :: Gen MinimumUTxOForShelleyBasedEra
    genMary = do
        minUTxOValue <- genInterestingLedgerCoin
        pure $ MinimumUTxOForShelleyBasedEra ShelleyBasedEraMary
            def {Shelley._minUTxOValue = minUTxOValue}

    genAlonzo :: Gen MinimumUTxOForShelleyBasedEra
    genAlonzo = do
        coinsPerUTxOWord <- genInterestingLedgerCoin
        pure $ MinimumUTxOForShelleyBasedEra ShelleyBasedEraAlonzo
            def {Alonzo._coinsPerUTxOWord = coinsPerUTxOWord}

    genBabbage :: Gen MinimumUTxOForShelleyBasedEra
    genBabbage = do
        coinsPerUTxOByte <- genInterestingLedgerCoin
        pure $ MinimumUTxOForShelleyBasedEra ShelleyBasedEraBabbage
            def {Babbage._coinsPerUTxOByte = coinsPerUTxOByte}

shrinkMinimumUTxOForShelleyBasedEra
    :: MinimumUTxOForShelleyBasedEra -> [MinimumUTxOForShelleyBasedEra]
shrinkMinimumUTxOForShelleyBasedEra = const []

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

genInterestingCoinValue :: Gen Natural
genInterestingCoinValue = do
    base <- (1_000_000 *) <$> choose (0, 8)
    offset <- choose @Integer (-10, 10)
    pure $ intCastMaybeZero $ base + offset

genInterestingLedgerCoin :: Gen Ledger.Coin
genInterestingLedgerCoin = Ledger.Coin . intCast
    <$> genInterestingCoinValue

intCastMaybeZero :: (Integral a, Integral b, Bits a, Bits b) => a -> b
intCastMaybeZero = fromMaybe 0 . intCastMaybe
