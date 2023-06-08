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
import Cardano.Ledger.Core
    ( auxDataTxL
    , bodyTxL
    , feeTxBodyL
    , inputsTxBodyL
    , outputsTxBodyL
    , vldtTxBodyL
    , witsTxL
    )
import Cardano.Ledger.Shelley.TxBody
    ( certsTxBodyL, wdrlsTxBodyL )
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
import Cardano.Wallet.Read.Primitive.Tx.Features.Withdrawals
    ( fromShelleyWdrl )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
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
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Tx as SL
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: SL.ShelleyTx (Cardano.ShelleyLedgerEra AllegraEra)
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
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,Nothing) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            -- TODO: (ADP-957)
            []
        , outputs =
            map fromAllegraTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Allegra.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromAllegraMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just $ afterShelleyValidityInterval vldl
    , countWits
    )
  where
    ins = tx ^. bodyTxL . inputsTxBodyL
    outs = tx ^. bodyTxL . outputsTxBodyL
    certs = tx ^. bodyTxL . certsTxBodyL
    wdrls = tx ^. bodyTxL . wdrlsTxBodyL
    fee = tx ^. bodyTxL . feeTxBodyL
    vldl = tx ^. bodyTxL . vldtTxBodyL
    wits = tx ^. witsTxL
    mmd = tx ^. auxDataTxL

    scriptMap =
        Map.map (flip NativeExplicitScript ViaSpending . toWalletScript (const Payment))
        $ SL.scriptWits wits
    countWits = WitnessCount
        (fromIntegral $ Set.size $ SL.addrWits wits)
        (Map.elems scriptMap)
        (fromIntegral $ Set.size $ SL.bootWits wits)
