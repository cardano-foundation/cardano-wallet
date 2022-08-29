{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR (..)
    , mkTxCBOR
    , getTxCBOR
    , parseCBOR
    )
    where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..), FromCBOR, ToCBOR )
import Cardano.Binary
    ( Annotator (runAnnotator)
    , FromCBOR (fromCBOR)
    , FullByteString (Full)
    , toCBOR
    )
import Codec.CBOR.Read
    ( DeserialiseFailure, deserialiseFromBytes )
import Codec.CBOR.Write
    ( toLazyByteString )
import Control.DeepSeq
    ( NFData, rnf )
import Data.Functor.Identity
    ( Identity (..) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Api as Api

import qualified Cardano.Wallet.Types.Read.Tx as Read
import qualified Data.ByteString.Lazy as BL

-- | Serialized version of a transaction. Deserializing should at least expose
-- enough information to compute the `TxId`.
data TxCBOR =
    TxCBOR
    { -- | Serialized transaction as expected from mempool on submission.
      txCBOR :: !BL.ByteString
      -- | Era of the transaction, to identify the right codec.
    , txEra :: !AnyCardanoEra
    } deriving (Show, Generic, Eq)

mkTxCBOR :: ToCBOR a => a -> AnyCardanoEra -> TxCBOR
mkTxCBOR = TxCBOR . toLazyByteString . toCBOR

instance Ord TxCBOR where
    compare (TxCBOR cb _) (TxCBOR cb' _) = compare cb cb'

instance NFData TxCBOR where
    rnf (TxCBOR cb _) = rnf cb -- missing instance NFData AnyCardanoEra

-- | Compute the CBOR representation of a transaction.
--
-- This CBOR includes the transaction body, but also witnesses.
getTxCBOR :: Read.Tx -> TxCBOR
getTxCBOR (Read.Tx era tx) = TxCBOR cbor (AnyCardanoEra era)
  where
    -- See Note [SeeminglyRedundantPatternMatches]
    cbor = toLazyByteString $ case era of
        ByronEra -> toCBOR tx
        ShelleyEra -> toCBOR tx
        MaryEra -> toCBOR tx
        AllegraEra -> toCBOR tx
        AlonzoEra -> toCBOR tx
        BabbageEra -> toCBOR tx

instance FromCBOR a => FromCBOR (Identity a) where
    fromCBOR = fmap Identity fromCBOR

-- | Parse CBOR into a transaction.
parseCBOR
    :: TxCBOR -> Either DeserialiseFailure Read.Tx
parseCBOR TxCBOR{txEra=AnyCardanoEra era,txCBOR} = case era of
    ByronEra -> boxEra era runIdentity
    ShelleyEra -> boxEra era runA
    AllegraEra -> boxEra era runA
    MaryEra -> boxEra era runA
    AlonzoEra -> boxEra era runA
    BabbageEra -> boxEra era runA
  where
    runA :: Annotator x -> x
    runA x = runAnnotator x $ Full txCBOR
    boxEra
        ::  ( FromCBOR (k (Read.TxEra era))
            , Api.IsCardanoEra era
            )
        => CardanoEra era
        -> (k (Read.TxEra era) -> Read.TxEra era)
        -> Either DeserialiseFailure Read.Tx
    boxEra era_ f =
        Read.Tx era_ . f . snd
            <$> deserialiseFromBytes fromCBOR txCBOR

