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
    ( FromCBOR, ToCBOR )
import Cardano.Binary
    ( Annotator (runAnnotator)
    , FromCBOR (fromCBOR)
    , FullByteString (Full)
    , toCBOR
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
import Codec.CBOR.Read
    ( DeserialiseFailure, deserialiseFromBytes )
import Codec.CBOR.Write
    ( toLazyByteString )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString.Lazy
    ( toStrict )
import Data.Functor.Identity
    ( Identity (..) )
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
parseTxFromCBOR :: TxCBOR -> Either DeserialiseFailure (EraValue Tx)
parseTxFromCBOR = sequenceEraValue . applyEraFun deserializeTx

-- | CBOR deserialization of a tx in any era.
deserializeTx :: EraFun (K BL.ByteString) (Either DeserialiseFailure :.: Tx)
deserializeTx = EraFun
    { byronFun = deserialize $ const runIdentity
    , shelleyFun = deserialize runA
    , allegraFun = deserialize runA
    , maryFun = deserialize runA
    , alonzoFun = deserialize runA
    , babbageFun = deserialize runA
    , conwayFun = deserialize runA
    }
    where
    deserialize
        :: FromCBOR (k (TxT x))
        => (BL.ByteString -> k (TxT x) -> TxT x)
        -> K BL.ByteString x
        -> (Either DeserialiseFailure :.: Tx) x
    deserialize untag (K txCBOR) = Comp $ Tx .  untag txCBOR . snd
        <$> deserialiseFromBytes fromCBOR txCBOR
    runA :: BL.ByteString -> Annotator x -> x
    runA txCBOR x = runAnnotator x $ Full txCBOR

instance FromCBOR a => FromCBOR (Identity a) where
    fromCBOR = fmap Identity fromCBOR
