{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTx
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Api
    ( ShelleyEra )
import Cardano.Ledger.Core
    ( auxDataTxL, bodyTxL, feeTxBodyL, inputsTxBodyL, outputsTxBodyL, witsTxL )
import Cardano.Ledger.Shelley.TxBody
    ( certsTxBodyL, ttlTxBodyL, wdrlsTxBodyL )
import Cardano.Wallet.Read.Eras
    ( inject, shelley )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts, fromStakeCredential )
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromShelleyMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromShelleyTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( shelleyValidityInterval )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId, shelleyTxHash )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScriptFromShelley )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..), ScriptReference (..), TokenMapWithScripts (..),
    ValidityIntervalExplicit (..), WitnessCount (..), emptyTokenMapWithScripts )
import Control.Lens
    ( (^.) )
import Data.Bifunctor
    ( bimap )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Word
    ( Word16, Word32, Word64 )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Tx as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (TxIn) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fromShelleyTxIn
    :: SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid (SL.TxIx ix)) =
    W.TxIn (W.Hash $ fromShelleyTxId txid) (unsafeCast ix)
  where
    -- During the Vasil hard-fork the cardano-ledger team moved from
    -- representing transaction indices with Word16s, to using Word64s (see
    -- commit
    -- https://github.com/input-output-hk/cardano-ledger/commit/4097a9055e6ea57161755e6a8cbfcf719b65e9ab).
    -- However, the valid range is still 0 <= x <= (maxBound :: Word16), so we
    -- reflect that here.
    unsafeCast :: Word64 -> Word32
    unsafeCast txIx =
        if txIx > fromIntegral (maxBound :: Word16)
        then error $ "Value for wallet TxIx is out of a valid range: " <> show txIx
        else fromIntegral txIx

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromShelleyTx
    :: SL.ShelleyTx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromShelleyTx tx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject shelley $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> toList ins
        , resolvedCollateralInputs =
            []
        , outputs =
            map fromShelleyTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Shelley.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ shelleyValidityInterval ttl
    , countWits
    )
  where
    ins = tx ^. bodyTxL . inputsTxBodyL
    outs = tx ^. bodyTxL . outputsTxBodyL
    certs = tx ^. bodyTxL . certsTxBodyL
    wdrls = tx ^. bodyTxL . wdrlsTxBodyL
    fee = tx ^. bodyTxL . feeTxBodyL
    ttl = tx ^. bodyTxL . ttlTxBodyL
    wits = tx ^. witsTxL
    mmd = tx ^. auxDataTxL
    -- SL.Tx (SL.TxBody _ins _outs _certs _wdrls _fee _ttl _ _) _wits _mmd = tx
    countWits = WitnessCount
        (fromIntegral $ Set.size $ SL.addrWits wits)
        (fmap (flip NativeExplicitScript ViaSpending . toWalletScriptFromShelley Payment)
            $ Map.elems $ SL.scriptWits wits)
        (fromIntegral $ Set.size $ SL.bootWits wits)

fromShelleyWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromShelleyWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromShelleyCoin
        <$> Map.toList wdrl
