{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Byron
    ( fromTxAux
    )
    where

import Prelude

import Cardano.Chain.UTxO
    ( ATxAux (..)
    , Tx (..)
    , taTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromByronTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromByronTxOut
    )
import Cardano.Wallet.Read.Eras
    ( eraValue
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Byron
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash
    )
import Control.Monad
    ( void
    )

import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.List.NonEmpty as NE

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
    UnsafeTx inputs outputs _attributes -> W.Tx
        { txId = W.Hash $ byronTxHash txAux

        , txCBOR = Just $ renderTxToCBOR $ eraValue @Byron $ Tx $ void txAux

        , fee = Nothing

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, Nothing) . fromByronTxIn <$> NE.toList inputs

        , resolvedCollateralInputs = []

        , outputs =
            fromByronTxOut <$> NE.toList outputs

        , collateralOutput =
            Nothing

        , withdrawals =
            mempty

        , metadata =
            Nothing

        , scriptValidity =
            Nothing
        }
