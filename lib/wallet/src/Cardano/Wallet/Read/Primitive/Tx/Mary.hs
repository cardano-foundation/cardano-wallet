{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx
    )
    where

import Prelude

import Cardano.Api
    ( MaryEra )
import Cardano.Ledger.Core
    ( auxDataTxL
    , bodyTxL
    , feeTxBodyL
    , inputsTxBodyL
    , mintTxBodyL
    , outputsTxBodyL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Shelley.TxBody
    ( certsTxBodyL, wdrlsTxBodyL )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Read.Eras
    ( inject, mary )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromMaryMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( maryMint )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromMaryTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromShelleyWdrl )
import Cardano.Wallet.Read.Tx
    ( Tx (Tx) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenPolicyId )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , ScriptReference (..)
    , TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , WitnessCountCtx
    , toKeyRole
    )
import Control.Lens
    ( (^.) )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromMaryTx
    :: SL.ShelleyTx (Cardano.ShelleyLedgerEra MaryEra)
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromMaryTx tx witCtx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject mary $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            []
        , outputs =
            map fromMaryTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Mary.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromMaryMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval vldt
    , countWits
    )
  where
    bod = tx ^. bodyTxL
    wits = tx ^. witsTxL
    mad = tx ^. auxDataTxL
    ins = bod ^. inputsTxBodyL
    outs = bod ^. outputsTxBodyL
    certs = bod ^. certsTxBodyL
    wdrls = bod ^. wdrlsTxBodyL
    fee = bod ^. feeTxBodyL
    vldt = bod ^. vldtTxBodyL
    mint = bod ^. mintTxBodyL
    (assetsToMint, assetsToBurn) = maryMint mint wits
    scriptMap = fromMaryScriptMap $ Shelley.scriptWits wits
    countWits = WitnessCount
        (fromIntegral $ Set.size $ Shelley.addrWits wits)
        (Map.elems scriptMap)
        (fromIntegral $ Set.size $ Shelley.bootWits wits)

    fromMaryScriptMap
        :: Map
            (SL.ScriptHash (Crypto (MA.ShelleyMAEra 'MA.Mary Crypto.StandardCrypto)))
            (SL.Core.Script (MA.ShelleyMAEra 'MA.Mary Crypto.StandardCrypto))
        -> Map TokenPolicyId AnyExplicitScript
    fromMaryScriptMap =
        Map.map (flip NativeExplicitScript ViaSpending . toWalletScript (toKeyRole witCtx)) .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
