{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.

module Cardano.Wallet.Read.Primitive.Tx.Byron
    (
    fromTxAux
    , fromTxIn
    , fromTxOut
    ) where

import Prelude

import Cardano.Api
    ( CardanoEra (ByronEra) )
import Cardano.Binary
    ( serialize' )
import Cardano.Chain.Common
    ( unsafeGetLovelace )
import Cardano.Chain.UTxO
    ( ATxAux (..), Tx (..), TxIn (..), TxOut (..), taTx )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( getTxCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash )

import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.List.NonEmpty as NE

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
    UnsafeTx inputs outputs _attributes -> W.Tx
        { txId = byronTxHash txAux

        , txCBOR = Just $ getTxCBOR $ Tx ByronEra $ () <$ txAux

        , fee = Nothing

        -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
        , resolvedInputs =
            (, W.Coin 0) . fromTxIn <$> NE.toList inputs

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

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = fromIntegral ix
    }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) = W.TxOut
    { address = W.Address (serialize' addr)
    , tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
    }
