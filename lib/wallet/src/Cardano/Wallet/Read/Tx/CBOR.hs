{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    ( EraFun (..)
    , EraValue
    , K (..)
    , applyEraFun
    , extractEraValue
    , sequenceEraValue
    , (:.:) (..)
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
    build =
        build . decodeUtf8 . convertToBase Base16 . toStrict . extractEraValue

instance ToText TxCBOR

-- | Render a tx into its cbor, it just applies 'serializeTx'.
renderTxToCBOR :: EraValue Tx -> EraValue (K BL.ByteString)
renderTxToCBOR = applyEraFun serializeTx

-- | CBOR serialization of a tx in any era.
serializeTx :: EraFun Tx (K BL.ByteString)
serializeTx =
    EraFun
        { byronFun = f byronProtVer
        , shelleyFun = f (eraProtVerLow @StandardShelley)
        , allegraFun = f (eraProtVerLow @StandardAllegra)
        , maryFun = f (eraProtVerLow @StandardMary)
        , alonzoFun = f (eraProtVerLow @StandardAlonzo)
        , babbageFun = f (eraProtVerLow @StandardBabbage)
        , conwayFun = f (eraProtVerLow @StandardConway)
        }
  where
    f :: (EncCBOR (TxT era)) => Ledger.Version -> Tx era -> K BL.ByteString era
    f protVer = K . Ledger.serialize protVer . unTx

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
        , shelleyFun = decodeTx shelleyProtVer "ShelleyTx"
        , allegraFun = decodeTx (eraProtVerLow @StandardAllegra) "AllegraTx"
        , maryFun = decodeTx (eraProtVerLow @StandardMary) "MaryTx"
        , alonzoFun = decodeTx (eraProtVerLow @StandardAlonzo) "AlonzoTx"
        , babbageFun = decodeTx (eraProtVerLow @StandardBabbage) "BabbageTx"
        , conwayFun = decodeTx (eraProtVerLow @StandardConway) "ConwayTx"
        }
  where
    decodeTx protVer label (K txCBOR) =
        Comp $ Tx <$> decodeFullAnnotator protVer label decCBOR txCBOR

roundTripTxCBor :: TxCBOR -> Either DecoderError TxCBOR
roundTripTxCBor = fmap renderTxToCBOR . parseTxFromCBOR
