{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Binary serialization of transactions.
-}
module Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    , renderTxToCBOR
    , parseTxFromCBOR
    , roundTripTxCBOR
    )
    where

import Prelude

import Cardano.Ledger.Binary.Decoding
    ( DecoderError
    )
import Cardano.Read.Ledger.Tx.CBOR
    ( deserializeTx
    , serializeTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( EraValue (..)
    , K (..)
    , applyEraFunValue
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.ByteString.Lazy
    ( toStrict
    )
import Data.Text.Class
    ( ToText
    )
import Data.Text.Encoding
    ( decodeUtf8
    )
import Fmt
    ( Buildable (..)
    )

import qualified Data.ByteString.Lazy as BL

-- | Serialized version of a transaction. Deserializing should at least expose
-- enough information to compute the 'TxId'.
type TxCBOR = EraValue (K BL.ByteString)

instance Buildable TxCBOR where
    build (EraValue (K bytes)) =
        build . decodeUtf8 . convertToBase Base16 $ toStrict bytes

instance ToText TxCBOR

-- | Render a tx into its cbor, it just applies 'serializeTx'.
renderTxToCBOR :: EraValue Tx -> TxCBOR
renderTxToCBOR = applyEraFunValue (K . serializeTx)

-- | Parse CBOR into a transaction in any eras
-- , smart application  of `deserializeTx`.
parseTxFromCBOR :: TxCBOR -> Either DecoderError (EraValue Tx)
parseTxFromCBOR (EraValue (K bytes :: K BL.ByteString era)) =
    EraValue <$> (deserializeTx bytes :: Either DecoderError (Tx era))

roundTripTxCBOR :: TxCBOR -> Either DecoderError TxCBOR
roundTripTxCBOR = fmap renderTxToCBOR . parseTxFromCBOR
