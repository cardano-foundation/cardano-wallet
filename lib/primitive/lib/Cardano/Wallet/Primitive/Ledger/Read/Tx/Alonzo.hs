{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Alonzo
    ( fromAlonzoTx
    , fromAlonzoTx'
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( Alonzo
    , addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
    , collateralInputsTxBodyL
    , feeTxBodyL
    , inputsTxBodyL
    , isValidTxL
    , mintTxBodyL
    , outputsTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( fromAlonzoMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( alonzoMint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromAlonzoTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Scripts
    ( alonzoAnyExplicitScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( TokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
    , WitnessCountCtx
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( shelleyWithdrawals
    )
import Control.Lens
    ( folded
    , (^.)
    , (^..)
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

fromAlonzoTx
    :: Alonzo.AlonzoTx Alonzo
    -> WitnessCountCtx
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromAlonzoTx tx witCtx =
    ( fromAlonzoTx' tx
    , anyEraCerts @Alonzo $ Read.Tx tx
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL.vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        (alonzoAnyExplicitScript witCtx <$> tx ^.. witsTxL.scriptTxWitsL.folded)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )
  where
    (assetsToMint, assetsToBurn) =
        alonzoMint (tx ^. bodyTxL.mintTxBodyL) (tx ^. witsTxL)

fromAlonzoTx' :: Alonzo.AlonzoTx Alonzo -> W.Tx
fromAlonzoTx' tx =
    W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ Read.inject Read.alonzo $ Read.Tx tx
        , fee =
            Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL . inputsTxBodyL . folded
        , resolvedCollateralInputs =
            (,Nothing) . fromShelleyTxIn
                <$> tx ^.. bodyTxL . collateralInputsTxBodyL . folded
        , outputs =
            fromAlonzoTxOut <$> tx ^.. bodyTxL . outputsTxBodyL . folded
        , collateralOutput =
            Nothing -- Collateral outputs are not supported in Alonzo.
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromAlonzoMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Just $ case tx ^. isValidTxL of
                Alonzo.IsValid True -> W.TxScriptValid
                Alonzo.IsValid False -> W.TxScriptInvalid
        }
