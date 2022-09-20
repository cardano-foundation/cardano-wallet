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
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Shelley.
module Cardano.Wallet.Read.Primitive.Tx.Allegra
    (fromAllegraTx, fromLedgerTxValidity)
    where

import Prelude

import Cardano.Api
    ( AllegraEra )
import Cardano.Wallet.Read.Eras
    ( allegra, inject )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyCert
    , fromShelleyCoin
    , fromShelleyMD
    , fromShelleyTxIn
    , fromShelleyTxOut
    , fromShelleyWdrl
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , emptyTokenMapWithScripts
    )
import Data.Foldable
    ( toList )
import Data.Quantity
    ( Quantity (..) )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Ouroboros.Network.Block as O


-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra AllegraEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       )
fromAllegraTx tx =
    ( W.Tx
        { txId =
            shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject allegra $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            -- TODO: (ADP-957)
            []
        , outputs =
            map fromShelleyTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Allegra.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , map fromShelleyCert (toList certs)
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just (fromLedgerTxValidity ttl)
    )
  where
    SL.Tx (MA.TxBody ins outs certs wdrls fee ttl _ _ _) _ mmd = tx

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

fromLedgerTxValidity
    :: MA.ValidityInterval
    -> ValidityIntervalExplicit
fromLedgerTxValidity (MA.ValidityInterval from to) =
    case (from, to) of
        (MA.SNothing, MA.SJust (O.SlotNo s)) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity s)
        (MA.SNothing, MA.SNothing) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity maxBound)
        (MA.SJust (O.SlotNo s1), MA.SJust (O.SlotNo s2)) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity s2)
        (MA.SJust (O.SlotNo s1), MA.SNothing) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity maxBound)
