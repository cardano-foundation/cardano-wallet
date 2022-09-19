{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Types.Read.Primitive.Tx.Alonzo
    (alonzoTxHash, fromAlonzoTx)
 where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Api
    ( AlonzoEra, CardanoEra (..) )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Shelley.TxBody
    ( EraIndependentTxBody )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenPolicyId )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    )
import Cardano.Wallet.Types.Read.Primitive.Tx.Allegra
    ( fromLedgerTxValidity )
import Cardano.Wallet.Types.Read.Primitive.Tx.Mary
    ( fromCardanoValue, fromLedgerMintValue, getScriptMap )
import Cardano.Wallet.Types.Read.Primitive.Tx.Shelley
    ( fromShelleyAddress
    , fromShelleyCert
    , fromShelleyCoin
    , fromShelleyMD
    , fromShelleyTxIn
    , fromShelleyWdrl
    )
import Cardano.Wallet.Types.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Types.Read.Tx.CBOR
    ( getTxCBOR )
import Cardano.Wallet.Types.Read.Tx.Hash
    ( fromShelleyTxId )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Ouroboros.Consensus.Cardano.Block
    ( StandardAlonzo )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage hiding
    ( ScriptIntegrityHash, TxBody )
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.TxIn as TxIn
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map

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
            Just $ getTxCBOR $ Tx AlonzoEra tx
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
    , map fromShelleyCert (toList certs)
    , TokenMapWithScripts assetsToMint mintScriptMap
    , TokenMapWithScripts assetsToBurn burnScriptMap
    , Just (fromLedgerTxValidity ttl)
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
    (assetsToMint, assetsToBurn) = fromLedgerMintValue mint
    scriptMap = fromAlonzoScriptMap $ Alonzo.txscripts' wits
    mintScriptMap = getScriptMap scriptMap assetsToMint
    burnScriptMap = getScriptMap scriptMap assetsToBurn

    fromAlonzoScriptMap
        :: Map
            (SL.ScriptHash (Crypto StandardAlonzo))
            (SL.Core.Script StandardAlonzo)
        -> Map TokenPolicyId AnyScript
    fromAlonzoScriptMap =
        Map.map toAnyScript .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
      where
        toAnyScript (Alonzo.TimelockScript script) =
            NativeScript $ toWalletScript Policy script
        toAnyScript (Alonzo.PlutusScript ver _) =
            PlutusScript (PlutusScriptInfo (toPlutusVer ver))

        toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
        toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

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
