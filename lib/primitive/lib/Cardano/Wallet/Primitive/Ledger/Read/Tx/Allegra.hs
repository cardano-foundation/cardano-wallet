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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Allegra
    ( fromAllegraTx
    , fromAllegraTx'
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..)
    )
import Cardano.Ledger.Api
    ( Allegra
    , addrTxWitsL
    , auxDataTxL
    , bodyTxL
    , bootAddrTxWitsL
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
    ( ShelleyTx
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toWalletScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromShelleyTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( fromAllegraMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromAllegraTxOut
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( afterShelleyValidityInterval
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( fromLedgerWithdrawals
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript (NativeExplicitScript)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( ScriptReference (ViaSpending)
    , TokenMapWithScripts
    , emptyTokenMapWithScripts
    )
import Cardano.Wallet.Primitive.Types.ValidityIntervalExplicit
    ( ValidityIntervalExplicit
    )
import Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (WitnessCount)
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
import Data.Foldable
    ( toList
    )

import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Certificates as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: ShelleyTx Allegra
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       , WitnessCount
       )
fromAllegraTx tx =
    ( fromAllegraTx' tx
    , anyEraCerts @Allegra $ Read.Tx tx
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

fromAllegraTx' :: ShelleyTx Allegra -> W.Tx
fromAllegraTx' tx =
    W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ Read.inject Read.allegra $ Read.Tx tx
        , fee =
            Just $ Ledger.toWalletCoin $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn <$> tx ^.. bodyTxL . inputsTxBodyL . folded
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
