{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Byron
    ( fromTxAux
    , fromTxOut
    )
    where

import Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Common
    ( unsafeGetLovelace )
import Cardano.Chain.UTxO
    ( ATxAux (..), Tx (..), TxOut (..), taTx )
import Cardano.Wallet.Read.Eras
    ( byron, inject )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromByronTxIn )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash )
import Control.Monad
    ( void )

import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut) )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Data.List.NonEmpty as NE

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
    UnsafeTx inputs outputs _attributes -> W.Tx
        { txId = W.Hash $ byronTxHash txAux

        , txCBOR = Just $ renderTxToCBOR $ inject byron $ Tx $ void txAux

        , fee = Nothing

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, Nothing) . fromByronTxIn <$> NE.toList inputs

        , resolvedCollateralInputs = []

        , outputs =
            fromTxOut <$> NE.toList outputs

        , collateralOutput =
            Nothing

        , withdrawals =
            mempty

        , metadata =
            Nothing

        , scriptValidity =
            Nothing
        }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
    }
