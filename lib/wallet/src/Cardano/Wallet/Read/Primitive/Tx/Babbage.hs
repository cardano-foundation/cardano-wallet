{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTx
    )
    where

import Prelude

import Cardano.Api
    ( BabbageEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Serialization
    ( sizedValue )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Read.Eras
    ( babbage, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( babbageMint )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Mary
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
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenPolicyId )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , TokenMapWithScripts (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , WitnessCountCtx
    , toKeyRole
    )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Ouroboros.Consensus.Cardano.Block
    ( StandardBabbage )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromBabbageTx
    :: Alonzo.ValidatedTx (Cardano.ShelleyLedgerEra BabbageEra)
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromBabbageTx tx@(Alonzo.ValidatedTx bod wits (Alonzo.IsValid isValid) aux) witCtx =
    ( W.Tx
        { txId =
            W.Hash $ alonzoTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject babbage $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList inps)
        , resolvedCollateralInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList collateralInps)
        , outputs =
            map (fromBabbageTxOut . sizedValue) (toList outs)
        , collateralOutput =
            case fmap (fromBabbageTxOut . sizedValue) collateralReturn of
                SNothing -> Nothing
                SJust txout -> Just txout
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
    , countWits
    )
  where
    Babbage.TxBody
        inps
        collateralInps
        _refInps
        outs
        collateralReturn
        _collateralTotal
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
    (assetsToMint, assetsToBurn) = babbageMint mint wits
    scriptMap = fromBabbageScriptMap $ Alonzo.txscripts' wits

    countWits = WitnessCount
        (fromIntegral $ Set.size $ Alonzo.txwitsVKey' wits)
        (Map.elems scriptMap)
        (fromIntegral $ Set.size $ Alonzo.txwitsBoot' wits)

    fromBabbageScriptMap
        :: Map
            (SL.ScriptHash (Crypto StandardBabbage))
            (SL.Core.Script StandardBabbage)
        -> Map TokenPolicyId AnyScript
    fromBabbageScriptMap =
        Map.map toAnyScript .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
      where
        toAnyScript (Alonzo.TimelockScript script) =
            NativeScript $ toWalletScript (toKeyRole witCtx) script
        toAnyScript (Alonzo.PlutusScript ver _) =
            PlutusScript (PlutusScriptInfo (toPlutusVer ver))

        toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
        toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

    fromBabbageTxOut
        :: Babbage.TxOut (Cardano.ShelleyLedgerEra BabbageEra)
        -> W.TxOut
    fromBabbageTxOut (Babbage.TxOut addr value _datum _refScript) =
        W.TxOut (fromShelleyAddress addr) $
        fromCardanoValue $ Cardano.fromMaryValue value

    toSLMetadata (Alonzo.AuxiliaryData blob _scripts) = SL.Metadata blob

    validity =
        if isValid
        then Just W.TxScriptValid
        else Just W.TxScriptInvalid
