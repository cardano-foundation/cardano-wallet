{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright: © 2020-2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

Binary serialization of transactions.
-}
module Cardano.Read.Ledger.Tx.CBOR
    ( serializeTx
    , deserializeTx
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
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Eras
    ( K (..)
    , unK
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    , TxT
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

{-# INLINABLE serializeTx #-}
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

{-# INLINABLE deserializeTx #-}
-- | CBOR deserialization of a tx in any era.
deserializeTx
    :: forall era . IsEra era
    => K BL.ByteString era -> (Either DecoderError :.: Tx) era
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
