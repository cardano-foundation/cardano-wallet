{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx
    )
    where

import Prelude

import Cardano.Api
    ( AlonzoEra )
import Cardano.Wallet.Read.Eras
    ( alonzo, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( alonzoMint )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromCardanoValue )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyAddress
    , fromShelleyCoin
    , fromShelleyMD
    , fromShelleyTxIn
    , fromShelleyWdrl
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( alonzoTxHash )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , emptyWitnessCount
    )
import Data.Foldable
    ( toList )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

fromAlonzoTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromAlonzoTx tx@(Alonzo.ValidatedTx bod wits (Alonzo.IsValid isValid) aux) =
    ( W.Tx
        { txId =
            W.Hash $ alonzoTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject alonzo $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList collateral)
        , outputs =
            map fromAlonzoTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Alonzo.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe aux
        , scriptValidity =
            validity
        }
    , anyEraCerts certs
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval ttl
    , emptyWitnessCount
    )
  where
    Alonzo.TxBody
        ins
        collateral
        outs
        certs
        wdrls
        fee
        ttl
        _upd
        _reqSignerHashes
        mint
        _wwpHash
        _adHash
        _network
        = bod
    (assetsToMint, assetsToBurn) = alonzoMint mint wits

    fromAlonzoTxOut
        :: Alonzo.TxOut (Cardano.ShelleyLedgerEra AlonzoEra)
        -> W.TxOut
    fromAlonzoTxOut (Alonzo.TxOut addr value _) =
        W.TxOut (fromShelleyAddress addr) $
        fromCardanoValue $ Cardano.fromMaryValue value

    toSLMetadata (Alonzo.AuxiliaryData blob _scripts) = SL.Metadata blob

    validity =
        if isValid
        then Just W.TxScriptValid
        else Just W.TxScriptInvalid
