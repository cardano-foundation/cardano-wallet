{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
    , roundTripTxCBor
    )
    where

import Prelude

import Cardano.Ledger.Api
    ( eraProtVerLow
    )
import Cardano.Ledger.Binary
    ( EncCBOR
    )
import Cardano.Ledger.Binary.Decoding
    ( DecCBOR (decCBOR)
    , DecoderError
    , byronProtVer
    , decodeFull
    , decodeFullAnnotator
    , shelleyProtVer
    )
import Cardano.Wallet.Read.Eras
    ( EraValue
    , IsEra
    , K (..)
    , applyEraFunValue
    , extractEraValue
    , sequenceEraValue
    , unK
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    , TxT
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
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardMary
    , StandardShelley
    )

import qualified Cardano.Ledger.Binary.Encoding as Ledger
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
renderTxToCBOR = applyEraFunValue serializeTx

-- | CBOR serialization of a tx in any era.
serializeTx :: forall era . IsEra era => Tx era -> K BL.ByteString era
serializeTx = case theEra @era of
    Byron -> f byronProtVer
    Shelley -> f (eraProtVerLow @StandardShelley)
    Allegra -> f (eraProtVerLow @StandardAllegra)
    Mary -> f (eraProtVerLow @StandardMary)
    Alonzo -> f (eraProtVerLow @StandardAlonzo)
    Babbage -> f (eraProtVerLow @StandardBabbage)
    Conway -> f (eraProtVerLow @StandardConway)

  where
    f :: EncCBOR (TxT era) => Ledger.Version -> Tx era -> K BL.ByteString era
    f protVer = K . Ledger.serialize protVer . unTx

-- | Parse CBOR into a transaction in any eras
-- , smart application  of `deserializeTx`.
parseTxFromCBOR :: TxCBOR -> Either DecoderError (EraValue Tx)
parseTxFromCBOR = sequenceEraValue
    . applyEraFunValue deserializeTx

-- | CBOR deserialization of a tx in any era.
deserializeTx :: forall era . IsEra era => K BL.ByteString era -> (Either DecoderError :.: Tx) era
deserializeTx = case theEra @era of
    Byron -> \txCBOR ->
        Comp $ Tx <$> decodeFull byronProtVer (unK txCBOR)
    Shelley -> decodeTx shelleyProtVer "ShelleyTx"
    Allegra -> decodeTx (eraProtVerLow @StandardAllegra) "AllegraTx"
    Mary -> decodeTx (eraProtVerLow @StandardMary) "MaryTx"
    Alonzo -> decodeTx (eraProtVerLow @StandardAlonzo) "AlonzoTx"
    Babbage -> decodeTx (eraProtVerLow @StandardBabbage) "BabbageTx"
    Conway -> decodeTx (eraProtVerLow @StandardConway) "ConwayTx"
    where
    decodeTx protVer label (K txCBOR) =
        Comp $ Tx <$> decodeFullAnnotator protVer label decCBOR txCBOR

roundTripTxCBor :: TxCBOR -> Either DecoderError TxCBOR
roundTripTxCBor = fmap renderTxToCBOR . parseTxFromCBOR
