{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- CBOR operations for era dependent transactions.
--

module Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    , renderTxToCBOR
    , parseTxFromCBOR
    , serializeTx
    , deserializeTx
    )
    where

import Prelude

import Cardano.Api
    ( ToCBOR )
import Cardano.Ledger.Binary
    ( toCBOR )
import Cardano.Ledger.Binary.Decoding
    ( DecCBOR (decCBOR)
    , DecoderError
    , byronProtVer
    , decodeFullAnnotator
    , shelleyProtVer, decodeFull
    )
import Cardano.Wallet.Read.Eras
    ( (:.:) (..)
    , EraFun (..)
    , EraValue
    , K (..)
    , applyEraFun
    , extractEraValue
    , sequenceEraValue
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..), TxT )
import Codec.CBOR.Write
    ( toLazyByteString )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString.Lazy
    ( toStrict )
import Data.Text.Class
    ( ToText )
import Data.Text.Encoding
    ( decodeUtf8 )
import Fmt
    ( Buildable (..) )

import qualified Data.ByteString.Lazy as BL

-- | Serialized version of a transaction. Deserializing should at least expose
-- enough information to compute the `TxId`.
type TxCBOR = EraValue (K BL.ByteString)

instance Buildable TxCBOR where
    build
        = build . decodeUtf8 . convertToBase Base16 . toStrict . extractEraValue

instance ToText TxCBOR

-- | Render a tx into its cbor, it just applies 'serializeTx'.
renderTxToCBOR :: EraValue Tx -> EraValue (K BL.ByteString)
renderTxToCBOR = applyEraFun serializeTx

-- | CBOR serialization of a tx in any era.
serializeTx :: EraFun Tx (K BL.ByteString)
serializeTx = EraFun
    { byronFun = f
    , shelleyFun = f
    , allegraFun = f
    , maryFun = f
    , alonzoFun = f
    , babbageFun = f
    , conwayFun = f
    }
  where
    f :: ToCBOR (TxT era) => Tx era -> K BL.ByteString  era
    f = K . toLazyByteString . toCBOR . unTx

-- | Parse CBOR into a transaction in any eras
-- , smart application  of `deserializeTx`.
parseTxFromCBOR :: TxCBOR -> Either DecoderError (EraValue Tx)
parseTxFromCBOR = sequenceEraValue . applyEraFun deserializeTx

-- | CBOR deserialization of a tx in any era.
deserializeTx :: EraFun (K BL.ByteString) (Either DecoderError :.: Tx)
deserializeTx =
    EraFun
        { byronFun = \(K txCBOR) ->
            Comp $ Tx <$> decodeFull byronProtVer txCBOR
        , shelleyFun = decodeTx "ShelleyTx"
        , allegraFun = decodeTx "AllegraTx"
        , maryFun = decodeTx "MaryTx"
        , alonzoFun = decodeTx "AlonzoTx"
        , babbageFun = decodeTx "BabbageTx"
        , conwayFun = decodeTx "ConwayTx"
        }
  where
    decodeTx label (K txCBOR) =
        Comp $ Tx <$> decodeFullAnnotator shelleyProtVer label decCBOR txCBOR
