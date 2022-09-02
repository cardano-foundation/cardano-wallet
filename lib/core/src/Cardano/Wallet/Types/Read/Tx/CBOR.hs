{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Types.Read.Tx.CBOR
    ( TxCBOR
    , getTxCBOR
    , parseCBOR
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
import Cardano.Wallet.Types.Read.Eras
    ( (:.:) (..)
    , EraFun
    , EraFunR (..)
    , EraValue
    , K (..)
    , applyEraFun
    , fromEraFunR
    , sequenceEraValue
    )

import Cardano.Wallet.Types.Read.Tx
    ( Tx (..), TxT )
import Codec.CBOR.Read
    ( DeserialiseFailure, deserialiseFromBytes )
import Codec.CBOR.Write
    ( toLazyByteString )
import Data.Functor.Identity
    ( Identity (..) )

import qualified Data.ByteString.Lazy as BL

-- | Serialized version of a transaction. Deserializing should at least expose
-- enough information to compute the `TxId`.
type TxCBOR = EraValue (K BL.ByteString)

-- | Compute the CBOR representation of a transaction.
--
-- This CBOR includes the transaction body in its era
getTxCBOR :: EraValue Tx -> TxCBOR
getTxCBOR  = applyEraFun serializeTx

serializeTx :: EraFun Tx (K BL.ByteString)
serializeTx = fromEraFunR $ EraFunR
    { byronFun = f
    , shelleyFun = f
    , allegraFun = f
    , maryFun = f
    , alonzoFun = f
    , babbageFun = f
    }
    where
        f :: ToCBOR(TxT era) => Tx era -> K BL.ByteString  era
        f = K . toLazyByteString . toCBOR . unTx

-- | Parse CBOR into a transaction of any known eras
parseCBOR :: TxCBOR -> Either DeserialiseFailure (EraValue Tx)
parseCBOR = sequenceEraValue . applyEraFun deserializeTx


deserializeTx :: EraFun (K BL.ByteString) (Either DeserialiseFailure :.: Tx)
deserializeTx = fromEraFunR $ EraFunR
    { byronFun = deserialize $ const runIdentity
    , shelleyFun = deserialize runA
    , allegraFun = deserialize runA
    , maryFun = deserialize runA
    , alonzoFun = deserialize runA
    , babbageFun = deserialize runA
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
