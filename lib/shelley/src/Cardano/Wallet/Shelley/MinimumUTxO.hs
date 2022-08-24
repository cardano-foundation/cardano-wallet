{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

import Cardano.Ledger.Babbage.Rules.Utxo
    ( babbageMinUTxOValue )
import Cardano.Ledger.Serialization
    ( mkSized )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..), MinimumUTxOForShelleyBasedEra (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..), txOutMaxCoin )
import Cardano.Wallet.Shelley.Compatibility
    ( toCardanoTxOut )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toBabbageTxOut, toWalletCoin )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.Cardano.Block
    ( StandardBabbage )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Shelley.MinimumUTxO.Internal as Internal

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
computeMinimumCoinForUTxO
    :: HasCallStack
    => MinimumUTxO
    -> Address
    -> TokenMap
    -> Coin
computeMinimumCoinForUTxO = \case
    MinimumUTxONone ->
        \_addr _tokenMap -> Coin 0
    MinimumUTxOConstant c ->
        \_addr _tokenMap -> c
    MinimumUTxOForShelleyBasedEraOf minUTxO ->
        computeMinimumCoinForUTxOShelleyBasedEra minUTxO

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
-- This function returns a value that is specific to a given Shelley-based era.
-- Importantly, a value that is valid in one era will not necessarily be valid
-- in another era.
--
computeMinimumCoinForUTxOShelleyBasedEra
    :: HasCallStack
    => MinimumUTxOForShelleyBasedEra
    -> Address
    -> TokenMap
    -> Coin
computeMinimumCoinForUTxOShelleyBasedEra
    (MinimumUTxOForShelleyBasedEra era pp) addr tokenMap = case era of
        -- Here we treat the Babbage era specially and use the ledger
        -- to compute the minimum ada quantity, bypassing the Cardano
        -- API. This appears to be significantly faster.
        Cardano.ShelleyBasedEraBabbage ->
            computeLedgerMinimumCoinForBabbage pp addr
                (TokenBundle txOutMaxCoin tokenMap)
        _ ->
            Internal.unsafeCoinFromCardanoApiCalculateMinimumUTxOResult $
            Cardano.calculateMinimumUTxO era
                (embedTokenMapWithinPaddedTxOut era addr tokenMap)
                (Cardano.fromLedgerPParams era pp)

-- | Returns 'True' if and only if the given 'TokenBundle' has a 'Coin' value
--   that is below the minimum acceptable 'Coin' value.
--
isBelowMinimumCoinForUTxO
    :: MinimumUTxO
    -> Address
    -> TokenBundle
    -> Bool
isBelowMinimumCoinForUTxO = \case
    MinimumUTxONone ->
        \_addr _tokenBundle ->
            False
    MinimumUTxOConstant c ->
        \_addr tokenBundle ->
            TokenBundle.getCoin tokenBundle < c
    MinimumUTxOForShelleyBasedEraOf minUTxO ->
        isBelowMinimumCoinForUTxOShelleyBasedEra minUTxO

-- | Returns 'True' if and only if the given 'TokenBundle' has a 'Coin' value
--   that is below the minimum acceptable 'Coin' value for a Shelley-based
--   era.
--
isBelowMinimumCoinForUTxOShelleyBasedEra
    :: MinimumUTxOForShelleyBasedEra
    -> Address
    -> TokenBundle
    -> Bool
isBelowMinimumCoinForUTxOShelleyBasedEra
    (MinimumUTxOForShelleyBasedEra era pp) addr tokenBundle =
        TokenBundle.getCoin tokenBundle <
            -- Here we treat the Babbage era specially and use the ledger
            -- to compute the minimum ada quantity, bypassing the Cardano
            -- API. This appears to be significantly faster.
            case era of
                Cardano.ShelleyBasedEraBabbage ->
                    computeLedgerMinimumCoinForBabbage pp addr tokenBundle
                _ ->
                    cardanoApiMinimumCoin
  where
    cardanoApiMinimumCoin :: Coin
    cardanoApiMinimumCoin =
        Internal.unsafeCoinFromCardanoApiCalculateMinimumUTxOResult $
        Cardano.calculateMinimumUTxO era
            (toCardanoTxOut era $ TxOut addr tokenBundle)
            (Cardano.fromLedgerPParams era pp)

-- | Embeds a 'TokenMap' within a padded 'Cardano.TxOut' value.
--
-- When computing the minimum UTxO quantity for a given 'TokenMap', we do not
-- have access to an address or to an ada quantity.
--
-- However, in order to compute a minimum UTxO quantity through the Cardano
-- API, we must supply a 'TxOut' value with a valid address and ada quantity.
--
-- It's imperative that we do not underestimate minimum UTxO quantities, as
-- this may result in the creation of transactions that are unacceptable to
-- the ledger. In the case of change generation, this would be particularly
-- problematic, as change outputs are generated automatically, and users do
-- not have direct control over the ada quantities generated.
--
-- However, while we cannot underestimate minimum UTxO quantities, we are at
-- liberty to moderately overestimate them.
--
-- Since the minimum UTxO quantity function is monotonically increasing w.r.t.
-- the size of the address and ada quantity, if we supply a 'TxOut' with an
-- address and ada quantity whose serialized lengths are the maximum possible
-- lengths, we can be confident that the resultant value will not be an
-- underestimate.
--
embedTokenMapWithinPaddedTxOut
    :: Cardano.ShelleyBasedEra era
    -> Address
    -> TokenMap
    -> Cardano.TxOut Cardano.CtxTx era
embedTokenMapWithinPaddedTxOut era addr m =
    toCardanoTxOut era $ TxOut addr $ TokenBundle txOutMaxCoin m

-- | Uses the ledger to compute a minimum ada quantity for the Babbage era.
--
computeLedgerMinimumCoinForBabbage
    :: Babbage.PParams StandardBabbage
    -> Address
    -> TokenBundle
    -> Coin
computeLedgerMinimumCoinForBabbage pp addr tokenBundle =
    toWalletCoin
        $ babbageMinUTxOValue pp
        $ mkSized
        $ toBabbageTxOut (TxOut addr tokenBundle) Nothing
