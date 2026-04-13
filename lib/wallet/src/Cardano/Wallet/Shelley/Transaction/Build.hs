{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Build ledger transactions directly using @mkBasicTxBody@ and lenses,
-- bypassing @Cardano.Api.TxBodyContent@.
module Cardano.Wallet.Shelley.Transaction.Build
    ( mkLedgerTx
    ) where

import Cardano.Balance.Tx.Eras
    ( IsRecentEra
    , RecentEra (..)
    )
import Cardano.Balance.Tx.Tx
    ( Tx
    )
import Cardano.Ledger.Address
    ( Withdrawals
    )
import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Api.Tx
    ( auxDataTxL
    , mkBasicTx
    )
import Cardano.Ledger.Api.Tx.Body
    ( feeTxBodyL
    , inputsTxBodyL
    , mintTxBodyL
    , mkBasicTxBody
    , outputsTxBodyL
    , vldtTxBodyL
    , withdrawalsTxBodyL
    )
import Cardano.Ledger.BaseTypes
    ( StrictMaybe (SJust, SNothing)
    )
import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Core
    ( TxCert
    , TxOut
    , auxDataHashTxBodyL
    , certsTxBodyL
    , hashTxAuxData
    , metadataTxAuxDataL
    , mkBasicTxAuxData
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Ledger.Metadata
    ( Metadatum
    )
import Cardano.Ledger.TxIn
    ( TxIn
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Sequence.Strict
    ( StrictSeq
    )
import Data.Set
    ( Set
    )
import Data.Word
    ( Word64
    )
import Prelude

import qualified Data.Map.Strict as Map

-- | Build a ledger 'Tx' directly from its components, using
-- @mkBasicTxBody@ and ledger lenses.
mkLedgerTx
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Set TxIn
    -- ^ Inputs
    -> StrictSeq (TxOut era)
    -- ^ Outputs
    -> Coin
    -- ^ Fee
    -> ValidityInterval
    -- ^ Validity interval
    -> Withdrawals
    -- ^ Withdrawals
    -> StrictSeq (TxCert era)
    -- ^ Certificates
    -> MultiAsset
    -- ^ Minting
    -> Map Word64 Metadatum
    -- ^ Metadata
    -> Tx era
mkLedgerTx era ins outs fee validity wdrls certs mint metadata =
    case era of
        RecentEraConway -> go
        RecentEraDijkstra ->
            error "mkLedgerTx: Dijkstra era not yet supported"
  where
    go =
        let
            auxData
                | Map.null metadata = SNothing
                | otherwise =
                    SJust
                        $ mkBasicTxAuxData
                        & metadataTxAuxDataL .~ metadata
            body =
                mkBasicTxBody
                    & inputsTxBodyL .~ ins
                    & outputsTxBodyL .~ outs
                    & feeTxBodyL .~ fee
                    & vldtTxBodyL .~ validity
                    & withdrawalsTxBodyL .~ wdrls
                    & certsTxBodyL .~ certs
                    & mintTxBodyL .~ mint
                    & auxDataHashTxBodyL
                        .~ fmap hashTxAuxData auxData
        in
            mkBasicTx body
                & auxDataTxL .~ auxData
