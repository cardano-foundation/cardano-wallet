{-# LANGUAGE GADTs #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values.
--
module Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO
    , isBelowMinimumCoinForUTxO
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..), txOutMaxCoin )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Shelley.MinimumUTxO.Internal as Internal

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
computeMinimumCoinForUTxO
    :: MinimumUTxO
    -> Address
    -> TokenMap
    -> Coin
computeMinimumCoinForUTxO minimumUTxO addr tokenMap =
    case minimumUTxO of
        MinimumUTxONone ->
            Coin 0
        MinimumUTxOConstant c ->
            c
        MinimumUTxOForShelleyBasedEraOf minimumUTxOShelley ->
            Internal.computeMinimumCoinForUTxOCardanoLedger minimumUTxOShelley
                (TxOut addr $ TokenBundle txOutMaxCoin tokenMap)

-- | Returns 'True' if and only if the given 'TokenBundle' has a 'Coin' value
--   that is below the minimum acceptable 'Coin' value.
--
isBelowMinimumCoinForUTxO
    :: MinimumUTxO
    -> Address
    -> TokenBundle
    -> Bool
isBelowMinimumCoinForUTxO minimumUTxO addr tokenBundle =
    case minimumUTxO of
        MinimumUTxONone ->
            False
        MinimumUTxOConstant c ->
            TokenBundle.getCoin tokenBundle < c
        MinimumUTxOForShelleyBasedEraOf minimumUTxOShelley ->
            TokenBundle.getCoin tokenBundle <
                Internal.computeMinimumCoinForUTxOCardanoLedger
                    minimumUTxOShelley (TxOut addr tokenBundle)
