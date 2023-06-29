{-# LANGUAGE GADTs #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values.
module Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO
    , isBelowMinimumCoinForUTxO
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Shelley.MinimumUTxO.Internal as Internal

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
-- The value returned is a /safe/ minimum, in the sense that any value above
-- the minimum should also satisfy the minimum UTxO rule. Consequently, when
-- assigning ada quantities to outputs, it should be safe to assign any value
-- that is greater than or equal to the value returned by this function.
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
            -- It's very important that we do not underestimate minimum UTxO
            -- quantities, as this may result in the creation of transactions
            -- that are unacceptable to the ledger.
            --
            -- In the cases of change generation and wallet balance migration,
            -- any underestimation would be particularly problematic, as
            -- outputs are generated automatically, and users do not have
            -- direct control over the ada quantities generated.
            --
            -- However, while we cannot underestimate minimum UTxO quantities,
            -- we are at liberty to moderately overestimate them.
            --
            -- Since the minimum UTxO function is monotonically increasing
            -- w.r.t. the size of the ada quantity, if we supply a 'TxOut' with
            -- an ada quantity whose serialized length is the maximum possible
            -- length, we can be confident that the resultant value can always
            -- safely be increased.
            --
            Internal.computeMinimumCoinForUTxO_CardanoLedger
                minimumUTxOShelley
                (TxOut addr $ TokenBundle txOutMaxCoin tokenMap)

-- | Returns 'True' if and only if the given 'TokenBundle' has a 'Coin' value
--   that is below the minimum acceptable 'Coin' value.
--
-- This function should /only/ be used to validate existing 'Coin' values that
-- do not need to be modified in any way.
--
-- Increasing the 'Coin' value of an output can lead to an increase in the
-- serialized length of that output, which can in turn lead to an increase in
-- the minimum required 'Coin' value, since the minimum required 'Coin' value
-- is dependent on an output's serialized length.
--
-- Therefore, even if this function indicates that a given value 'Coin' value
-- 'c' satisfies the minimum UTxO rule, it should not be taken to imply that
-- all values greater than 'c' will also satisfy the minimum UTxO rule.
--
-- If you need to generate a value that can always safely be increased, use
-- the 'computeMinimumCoinForUTxO' function instead.
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
            TokenBundle.getCoin tokenBundle
                < Internal.computeMinimumCoinForUTxO_CardanoLedger
                    minimumUTxOShelley
                    (TxOut addr tokenBundle)
