{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Api
    ( AllegraEra )
import Cardano.Ledger.Api
    ( addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
    , certsTxBodyL
    , feeTxBodyL
    , inputsTxBodyL
    , outputsTxBodyL
    , scriptTxWitsL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Core
    ()
import Cardano.Ledger.Shelley.Tx
    ( ShelleyTx )
import Cardano.Wallet.Read.Eras
    ( allegra, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromAllegraMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromAllegraTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
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
    ValidityIntervalExplicit (..), WitnessCount (..), emptyTokenMapWithScripts )
import Control.Lens
    ( folded, (^.), (^..) )
import Data.Foldable
    ( toList )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read.Primitive.Coin as Coin
import qualified Data.Set as Set

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: ShelleyTx (Cardano.ShelleyLedgerEra AllegraEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromAllegraTx tx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject allegra $ Tx tx
        , fee =
            Just $ Coin.unsafeFromLedger $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL.inputsTxBodyL.folded
        , resolvedCollateralInputs =
            [] -- TODO: (ADP-957)
        , outputs =
            fromAllegraTxOut <$> toList (tx ^. bodyTxL . outputsTxBodyL)
        , collateralOutput =
            Nothing -- Collateral outputs are not supported in Allegra.
        , withdrawals =
            fromLedgerWithdrawals . shelleyWithdrawals $ tx
        , metadata =
            fromAllegraMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Nothing
        }
    , anyEraCerts $ tx ^. bodyTxL . certsTxBodyL
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL.vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL.addrTxWitsL)
        ((`NativeExplicitScript` ViaSpending)
         . toWalletScript (\_vkey -> Payment)
            <$> tx ^.. witsTxL.scriptTxWitsL.folded)
        (fromIntegral $ Set.size $ tx ^. witsTxL.bootAddrTxWitsL)
    )
