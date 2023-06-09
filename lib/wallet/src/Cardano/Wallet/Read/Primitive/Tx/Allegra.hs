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
    , withdrawalsTxBodyL
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
import Cardano.Wallet.Read.Primitive.Tx.Features.Fee
    ( fromShelleyCoin )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( fromAllegraMetadata )
import Cardano.Wallet.Read.Primitive.Tx.Features.Outputs
    ( fromAllegraTxOut )
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity
    ( afterShelleyValidityInterval )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( fromLedgerWithdrawals )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..), ScriptReference (..), TokenMapWithScripts (..),
    ValidityIntervalExplicit (..), WitnessCount (..), emptyTokenMapWithScripts )
import Control.Lens
    ( (^.) )
import Data.Foldable
    ( toList )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
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
            Just $ fromShelleyCoin $ tx ^. bodyTxL . feeTxBodyL
        , resolvedInputs =
            (,Nothing) . fromShelleyTxIn
                <$> toList (tx ^. bodyTxL . inputsTxBodyL)
        , resolvedCollateralInputs =
            [] -- TODO: (ADP-957)
        , outputs =
            fromAllegraTxOut <$> toList (tx ^. bodyTxL . outputsTxBodyL)
        , collateralOutput =
            Nothing -- Collateral outputs are not supported in Allegra.
        , withdrawals =
            fromLedgerWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
        , metadata =
            fromAllegraMetadata <$> SL.strictMaybeToMaybe (tx ^. auxDataTxL)
        , scriptValidity =
            Nothing
        }
    , anyEraCerts $ tx ^. bodyTxL . certsTxBodyL
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ afterShelleyValidityInterval $ tx ^. bodyTxL . vldtTxBodyL
    , WitnessCount
        (fromIntegral $ Set.size $ tx ^. witsTxL . addrTxWitsL)
        ((`NativeExplicitScript` ViaSpending)
         . toWalletScript (\_vkey -> Payment)
            <$> Map.elems (tx ^. witsTxL . scriptTxWitsL))
        (fromIntegral $ Set.size $ tx ^. witsTxL . bootAddrTxWitsL)
    )
