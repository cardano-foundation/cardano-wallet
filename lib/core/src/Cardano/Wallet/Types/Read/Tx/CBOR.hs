{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Types.Read.Tx.CBOR
    ( TxCBOR (..)
    , getTxCBOR
    , parseCBOR
    , ValidEra (..)
    )
    where

import Prelude

import Cardano.Api
    ( CardanoEra (..), FromCBOR )
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
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Types.Read.Tx as Read
import qualified Data.ByteString.Lazy as BL

data ValidEra = forall era . Read.IsKnownEra era => ValidEra (CardanoEra era)

deriving instance Show ValidEra

instance Enum ValidEra where

   fromEnum = \case
      ValidEra ByronEra    -> 0
      ValidEra ShelleyEra  -> 1
      ValidEra AllegraEra  -> 2
      ValidEra MaryEra     -> 3
      ValidEra AlonzoEra   -> 4
      ValidEra BabbageEra  -> 5

   toEnum = \case
      0 -> ValidEra ByronEra
      1 -> ValidEra ShelleyEra
      2 -> ValidEra AllegraEra
      3 -> ValidEra MaryEra
      4 -> ValidEra AlonzoEra
      5 -> ValidEra BabbageEra
      n ->
         error $
            "ValidEra.toEnum: " <> show n
            <> " does not correspond to any known enumerated era."

instance Eq ValidEra where
    ValidEra era == ValidEra era' =
      case testEquality era era' of
        Nothing   -> False
        Just Refl -> True -- since no constructors share types

-- | Serialized version of a transaction. Deserializing should at least expose
-- enough information to compute the `TxId`.
data TxCBOR =
    TxCBOR
    { -- | Serialized transaction as expected from mempool on submission.
      txCBOR :: !BL.ByteString
      -- | Era of the transaction, to identify the right codec.
    , txEra :: ValidEra
    } deriving (Show, Generic, Eq)

instance Ord TxCBOR where
    compare (TxCBOR cb _) (TxCBOR cb' _) = compare cb cb'

instance NFData TxCBOR where
    rnf (TxCBOR cb _) = rnf cb -- missing instance NFData ValidEraEra

-- | Compute the CBOR representation of a transaction.
--
-- This CBOR includes the transaction body, but also witnesses.
getTxCBOR :: Read.Tx -> TxCBOR
getTxCBOR (Read.Tx era tx) = TxCBOR cbor (ValidEra era)
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
parseCBOR TxCBOR{txEra=ValidEra (era :: CardanoEra era) ,txCBOR}
    = case era of
        ByronEra -> boxEra runIdentity
        ShelleyEra -> boxEra runA
        AllegraEra -> boxEra runA
        MaryEra -> boxEra runA
        AlonzoEra -> boxEra runA
        BabbageEra -> boxEra runA
  where
    runA :: Annotator x -> x
    runA x = runAnnotator x $ Full txCBOR
    boxEra
        :: (FromCBOR (k (Read.TxEra era)), Show (Read.TxEra era)
           )
        => (k (Read.TxEra era) -> Read.TxEra era)
        -> Either DeserialiseFailure Read.Tx
    boxEra f =
        Read.Tx era . f . snd
            <$> deserialiseFromBytes fromCBOR txCBOR
