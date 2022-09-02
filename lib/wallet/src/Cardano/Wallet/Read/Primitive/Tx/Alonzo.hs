{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Read.Primitive.Tx.Alonzo
    (alonzoTxHash, fromAlonzoTx)
 where

import Prelude

import Cardano.Api
    ( AlonzoEra )
import Cardano.Ledger.Shelley.TxBody
    ( EraIndependentTxBody )
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
    ( fromShelleyTxId )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..), ValidityIntervalExplicit (..) )
import Data.Foldable
    ( toList )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage hiding
    ( ScriptIntegrityHash, TxBody )
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.TxIn as TxIn
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

alonzoTxHash
    :: ( Crypto.HashAlgorithm (SL.HASH crypto)
       , SafeHash.HashAnnotated
             (SL.Core.TxBody era)
             EraIndependentTxBody
             crypto)
    => Babbage.ValidatedTx era
    -> W.Hash "Tx"
alonzoTxHash (Alonzo.ValidatedTx bod _ _ _) = fromShelleyTxId $ TxIn.txid bod

fromAlonzoTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra AlonzoEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       )
fromAlonzoTx tx@(Alonzo.ValidatedTx bod wits (Alonzo.IsValid isValid) aux) =
    ( W.Tx
        { txId =
            alonzoTxHash tx
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
