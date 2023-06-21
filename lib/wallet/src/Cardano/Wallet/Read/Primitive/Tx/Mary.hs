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
import Cardano.Ledger.Api
    ( addrTxWitsL, auxDataTxL, bodyTxL, bootAddrTxWitsL, certsTxBodyL,
    feeTxBodyL, inputsTxBodyL, mintTxBodyL, outputsTxBodyL, scriptTxWitsL,
    vldtTxBodyL, witsTxL )
import Cardano.Wallet.Read.Eras
    ( inject, mary )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
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
    ( fromLedgerWithdrawals )
import Cardano.Wallet.Read.Tx
    ( Tx (Tx) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( shelleyWithdrawals )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..), ScriptReference (..), TokenMapWithScripts (..),
    ValidityIntervalExplicit (..), WitnessCount (..), WitnessCountCtx,
    toKeyRole )
import Control.Lens
    ( folded, (^.), (^..) )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read.Primitive.Coin as Coin
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
            Just $ Coin.unsafeFromLedger $ tx ^. bodyTxL.feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL.inputsTxBodyL.folded
        , resolvedCollateralInputs =
            []
        , outputs =
            fromMaryTxOut <$> tx ^.. bodyTxL.outputsTxBodyL.folded
        , collateralOutput =
            Nothing -- Collateral outputs are not supported in Mary.
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromMaryMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Nothing
        }
    , anyEraCerts $ tx ^. bodyTxL.certsTxBodyL
    , assetsToMint
    , assetsToBurn
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL.vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        ((`NativeExplicitScript` ViaSpending)
         . toWalletScript (toKeyRole witCtx)
            <$> tx ^.. witsTxL.scriptTxWitsL.folded)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )
  where
    (assetsToMint, assetsToBurn) =
        maryMint (tx ^. bodyTxL.mintTxBodyL) (tx ^. witsTxL)
