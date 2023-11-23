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
    , fromShelleyTx'
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..)
    )
import Cardano.Api
    ( ShelleyEra
    )
import Cardano.Ledger.Core
    ( addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
    , feeTxBodyL
    , inputsTxBodyL
    , outputsTxBodyL
    , scriptTxWitsL
    , witsTxL
    )
import Cardano.Ledger.Shelley
    ( ShelleyTx
    )
import Cardano.Ledger.Shelley.TxBody
    ( certsTxBodyL
    , ttlTxBodyL
    )
import Cardano.Wallet.Primitive.Convert
    ( toWalletScriptFromShelley
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (NativeExplicitScript)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (..)
    , TokenMapWithScripts (..)
    , emptyTokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (..)
    )
import Cardano.Wallet.Read.Eras
    ( inject
    , shelley
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromShelleyMetadata
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromShelleyTxOut
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( shelleyValidityInterval
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId
    , shelleyTxHash
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( shelleyWithdrawals
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
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
    :: ShelleyTx (Cardano.ShelleyLedgerEra ShelleyEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromShelleyTx tx =
    ( fromShelleyTx' tx
    , anyEraCerts $ tx ^. bodyTxL . certsTxBodyL
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ shelleyValidityInterval $ tx ^. bodyTxL.ttlTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        ((`NativeExplicitScript` ViaSpending)
         . toWalletScriptFromShelley Payment
            <$> tx ^.. witsTxL.scriptTxWitsL.folded)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )

fromShelleyTx' :: SL.ShelleyTx (Cardano.ShelleyLedgerEra ShelleyEra) -> W.Tx
fromShelleyTx' tx =
    W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject shelley $ Tx tx
        , fee =
            Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL . inputsTxBodyL . folded
        , resolvedCollateralInputs =
            []
        , outputs =
            fromShelleyTxOut <$> tx ^.. bodyTxL . outputsTxBodyL . folded
        , collateralOutput =
            Nothing -- Collateral outputs are not supported in Shelley.
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromShelleyMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Nothing
        }
