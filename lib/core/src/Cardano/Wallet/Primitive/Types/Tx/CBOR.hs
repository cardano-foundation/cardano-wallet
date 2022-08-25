{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR (..)
    , mkTxCBOR
    , parseCBOR
    , EraTx (..)
    , ParsedTx
    , ParsedTxShelley
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra
    , AnyCardanoEra (AnyCardanoEra)
    , BabbageEra
    , ByronEra
    , CardanoEra (..)
    , FromCBOR
    , MaryEra
    , ShelleyEra
    , ToCBOR
    )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )
import Cardano.Binary
    ( Annotator (runAnnotator)
    , FromCBOR (fromCBOR)
    , FullByteString (Full)
    , toCBOR
    )
import Cardano.Chain.UTxO
    ( ATxAux )
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

import qualified Cardano.Ledger.Shelley.API as SLAPI
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

type ParsedTxShelley x = SLAPI.Tx (ShelleyLedgerEra x)

type family ParsedTx x where
    ParsedTx ByronEra = ATxAux ()
    ParsedTx ShelleyEra = ParsedTxShelley ShelleyEra
    ParsedTx MaryEra = ParsedTxShelley MaryEra
    ParsedTx AllegraEra = ParsedTxShelley AllegraEra
    ParsedTx AlonzoEra = ParsedTxShelley AlonzoEra
    ParsedTx BabbageEra = ParsedTxShelley BabbageEra

instance FromCBOR a => FromCBOR (Identity a) where
    fromCBOR = fmap Identity fromCBOR

data EraTx = forall era . EraTx (CardanoEra era) (ParsedTx era)

parseCBOR
    :: TxCBOR -> Either DeserialiseFailure EraTx
parseCBOR TxCBOR{..} = case txEra of
    AnyCardanoEra ByronEra -> boxEra ByronEra runIdentity
    AnyCardanoEra ShelleyEra -> boxEra ShelleyEra runA
    AnyCardanoEra AllegraEra -> boxEra AllegraEra runA
    AnyCardanoEra MaryEra -> boxEra MaryEra runA
    AnyCardanoEra AlonzoEra -> boxEra AlonzoEra runA
    AnyCardanoEra BabbageEra -> boxEra BabbageEra runA
    where
    runA :: Annotator x -> x
    runA x = runAnnotator x $ Full txCBOR
    boxEra :: FromCBOR (k (ParsedTx era))
        => CardanoEra era
        -> (k (ParsedTx era) -> ParsedTx era)
        -> Either DeserialiseFailure EraTx
    boxEra era f =
        EraTx era . f . snd
            <$> deserialiseFromBytes fromCBOR txCBOR

