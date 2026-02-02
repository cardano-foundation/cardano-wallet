{- |
Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Binary serialization of transactions.
-}
module Cardano.Read.Ledger.Tx.CBOR
    ( -- * Serialization
      serializeTx

      -- * Deserialization
    , deserializeTx
    )
where

import Prelude

import Cardano.Ledger.Api
    ( eraProtVerLow
    )
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , DecoderError
    , EncCBOR
    , byronProtVer
    , decodeFull
    , decodeFullAnnotator
    )
import Cardano.Ledger.Binary.Encoding qualified as Ledger
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    , TxT
    )
import Data.ByteString.Lazy qualified as BL

{-# INLINEABLE serializeTx #-}

-- | CBOR serialization of a tx in any era.
serializeTx :: forall era. IsEra era => Tx era -> BL.ByteString
serializeTx = case era of
    Byron -> f (versionForEra era)
    Shelley -> f (versionForEra era)
    Allegra -> f (versionForEra era)
    Mary -> f (versionForEra era)
    Alonzo -> f (versionForEra era)
    Babbage -> f (versionForEra era)
    Conway -> f (versionForEra era)
  where
    era = theEra :: Era era

    f :: EncCBOR (TxT era) => Ledger.Version -> Tx era -> BL.ByteString
    f protVer = Ledger.serialize protVer . unTx

{-# INLINEABLE deserializeTx #-}

-- | CBOR deserialization of a tx in any era.
deserializeTx
    :: forall era
     . IsEra era
    => BL.ByteString -> Either DecoderError (Tx era)
deserializeTx = case era of
    Byron -> fmap Tx . decodeFull (versionForEra era)
    Shelley -> decodeTx (versionForEra era) "ShelleyTx"
    Allegra -> decodeTx (versionForEra era) "AllegraTx"
    Mary -> decodeTx (versionForEra era) "MaryTx"
    Alonzo -> decodeTx (versionForEra era) "AlonzoTx"
    Babbage -> decodeTx (versionForEra era) "BabbageTx"
    Conway -> decodeTx (versionForEra era) "ConwayTx"
  where
    era = theEra :: Era era
    decodeTx protVer label =
        fmap Tx . decodeFullAnnotator protVer label decCBOR

{-# INLINE versionForEra #-}

-- | Protocol version that we use for encoding and decoding.
versionForEra :: forall era. Era era -> Ledger.Version
versionForEra era = case era of
    Byron -> byronProtVer
    Shelley -> eraProtVerLow @era
    Allegra -> eraProtVerLow @era
    Mary -> eraProtVerLow @era
    Alonzo -> eraProtVerLow @era
    Babbage -> eraProtVerLow @era
    Conway -> eraProtVerLow @era
