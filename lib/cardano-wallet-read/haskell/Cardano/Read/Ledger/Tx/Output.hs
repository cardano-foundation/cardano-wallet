{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed transaction output.
-}
module Cardano.Read.Ledger.Tx.Output
    ( -- * Output type
      OutputType
    , Output (..)

      -- * Accessors
    , getEraCompactAddr
    , getEraValue

      -- * Era upgrades
    , upgradeToOutputBabbage
    , upgradeToOutputConway

      -- * Serialization
    , deserializeOutput
    , serializeOutput
    )
where

import Prelude

import Cardano.Chain.UTxO qualified as BY
import Cardano.Ledger.Alonzo.TxOut
    ( AlonzoTxOut
    )
import Cardano.Ledger.Api
    ( Addr (AddrBootstrap)
    , BootstrapAddress (..)
    , eraProtVerLow
    , mkBasicTxOut
    , upgradeTxOut
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut
    )
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , DecoderError
    , EncCBOR
    , byronProtVer
    , decodeFull
    , decodeFullDecoder
    , shelleyProtVer
    )
import Cardano.Ledger.Binary.Encoding qualified as Ledger
import Cardano.Ledger.Core
    ( compactAddrTxOutL
    , valueTxOutL
    )
import Cardano.Ledger.Shelley.TxOut
    ( ShelleyTxOut
    )
import Cardano.Read.Ledger.Address
    ( CompactAddr (..)
    , CompactAddrType
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Read.Ledger.Value
    ( Value (..)
    , ValueType
    , maryValueFromByronValue
    )
import Control.Lens
    ( view
    )
import Data.ByteString.Lazy qualified as BL
import Data.Text
    ( Text
    )

{-----------------------------------------------------------------------------
    Output
------------------------------------------------------------------------------}

-- | Era-specific single output type.
type family OutputType era where
    OutputType Byron = BY.TxOut
    OutputType Shelley = ShelleyTxOut Shelley
    OutputType Allegra = ShelleyTxOut Allegra
    OutputType Mary = ShelleyTxOut Mary
    OutputType Alonzo = AlonzoTxOut Alonzo
    OutputType Babbage = BabbageTxOut Babbage
    OutputType Conway = BabbageTxOut Conway

-- | Era-indexed single transaction output wrapper.
newtype Output era = Output (OutputType era)

deriving instance Show (OutputType era) => Show (Output era)
deriving instance Eq (OutputType era) => Eq (Output era)

{-----------------------------------------------------------------------------
    Eliminators
------------------------------------------------------------------------------}

{-# INLINEABLE getEraCompactAddr #-}

-- | Extract the compact address from an output in any era.
getEraCompactAddr
    :: forall era. IsEra era => Output era -> CompactAddr era
getEraCompactAddr = case theEra :: Era era of
    Byron -> address $ (\(BY.CompactTxOut a _) -> a) . BY.toCompactTxOut
    Shelley -> address (view compactAddrTxOutL)
    Allegra -> address (view compactAddrTxOutL)
    Mary -> address (view compactAddrTxOutL)
    Alonzo -> address (view compactAddrTxOutL)
    Babbage -> address (view compactAddrTxOutL)
    Conway -> address (view compactAddrTxOutL)

-- | Helper function for type inference in 'getEraCompactAddr'.
address
    :: (OutputType era -> CompactAddrType era)
    -> Output era
    -> CompactAddr era
address f (Output x) = CompactAddr (f x)

{-# INLINEABLE getEraValue #-}

-- | Extract the value from an output in any era.
getEraValue :: forall era. IsEra era => Output era -> Value era
getEraValue = case theEra :: Era era of
    Byron -> value BY.txOutValue
    Shelley -> value (view valueTxOutL)
    Allegra -> value (view valueTxOutL)
    Mary -> value (view valueTxOutL)
    Alonzo -> value (view valueTxOutL)
    Babbage -> value (view valueTxOutL)
    Conway -> value (view valueTxOutL)

-- | Helper function for type inference in 'getEraValue'.
value :: (OutputType era -> ValueType era) -> Output era -> Value era
value f (Output x) = Value (f x)

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}

{-# INLINEABLE upgradeToOutputBabbage #-}

{- | Upgrade an 'Output' to the 'Babbage' era if possibile.

Hardfork: Update this function to the new era.
-}
upgradeToOutputBabbage
    :: forall era
     . IsEra era
    => Output era
    -> Maybe (Output Babbage)
upgradeToOutputBabbage = case theEra :: Era era of
    Byron ->
        Just
            . onOutput
                ( \(BY.TxOut addr lovelace) ->
                    mkBasicTxOut
                        (AddrBootstrap (BootstrapAddress addr))
                        (maryValueFromByronValue lovelace)
                )
    Shelley ->
        Just
            . onOutput
                (upgradeTxOut . upgradeTxOut . upgradeTxOut . upgradeTxOut)
    Allegra ->
        Just
            . onOutput
                (upgradeTxOut . upgradeTxOut . upgradeTxOut)
    Mary ->
        Just
            . onOutput
                (upgradeTxOut . upgradeTxOut)
    Alonzo -> Just . onOutput upgradeTxOut
    Babbage -> Just
    Conway -> const Nothing

{-# INLINEABLE upgradeToOutputConway #-}

{- | Upgrade an 'Output' to the 'Conway' era.

Hardfork: Update this function to the next era.
-}
upgradeToOutputConway
    :: forall era. IsEra era => Output era -> Output Conway
upgradeToOutputConway = case theEra :: Era era of
    Byron -> onOutput
        $ \(BY.TxOut addr lovelace) ->
            mkBasicTxOut
                (AddrBootstrap (BootstrapAddress addr))
                (maryValueFromByronValue lovelace)
    Shelley ->
        onOutput
            $ upgradeTxOut
                . upgradeTxOut
                . upgradeTxOut
                . upgradeTxOut
                . upgradeTxOut
    Allegra ->
        onOutput
            $ upgradeTxOut . upgradeTxOut . upgradeTxOut . upgradeTxOut
    Mary ->
        onOutput
            $ upgradeTxOut . upgradeTxOut . upgradeTxOut
    Alonzo ->
        onOutput
            $ upgradeTxOut . upgradeTxOut
    Babbage -> onOutput upgradeTxOut
    Conway -> id

-- | Helper function for type inference in era upgrade operations.
onOutput
    :: (OutputType era1 -> OutputType era2)
    -> Output era1
    -> Output era2
onOutput f (Output x) = Output (f x)

{-----------------------------------------------------------------------------
    Serialization
------------------------------------------------------------------------------}

{-# INLINEABLE serializeOutput #-}

-- | Serialize an 'Output' in binary format, e.g. for storing in a database.
serializeOutput
    :: forall era. IsEra era => Output era -> BL.ByteString
serializeOutput = case theEra :: Era era of
    Byron -> encode byronProtVer
    Shelley -> encode (eraProtVerLow @Shelley)
    Allegra -> encode (eraProtVerLow @Allegra)
    Mary -> encode (eraProtVerLow @Mary)
    Alonzo -> encode (eraProtVerLow @Alonzo)
    Babbage -> encode (eraProtVerLow @Babbage)
    Conway -> encode (eraProtVerLow @Conway)
  where
    encode
        :: EncCBOR (OutputType era)
        => Ledger.Version
        -> Output era
        -> BL.ByteString
    encode protVer (Output out) = Ledger.serialize protVer out

{-# INLINEABLE deserializeOutput #-}

{- | Deserialize an 'Output' from the binary format.

prop> ∀ o.  deserializeOutput (serializeOutput o) == Just o
-}
deserializeOutput
    :: forall era
     . IsEra era
    => BL.ByteString
    -> Either DecoderError (Output era)
deserializeOutput = case theEra :: Era era of
    Byron -> fmap Output . decodeFull byronProtVer
    Shelley -> decode shelleyProtVer "ShelleyTxOut"
    Allegra -> decode (eraProtVerLow @Allegra) "AllegraTxOut"
    Mary -> decode (eraProtVerLow @Mary) "MaryTxOut"
    Alonzo -> decode (eraProtVerLow @Alonzo) "AlonzoTxOut"
    Babbage -> decode (eraProtVerLow @Babbage) "BabbageTxOut"
    Conway -> decode (eraProtVerLow @Conway) "ConwayTxOut"
  where
    decode
        :: DecCBOR (OutputType era)
        => Ledger.Version
        -> Text
        -> BL.ByteString
        -> Either DecoderError (Output era)
    decode protVer label =
        fmap Output . decodeFullDecoder protVer label decCBOR
