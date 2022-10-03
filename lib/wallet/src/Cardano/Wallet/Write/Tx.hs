{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.Write.Tx
    (
    -- * RecentEra
      RecentEra (..)
    , IsRecentEra (..)
    , isRecentEra
    , LatestEra
    , LatestLedgerEra
    , StandardBabbage

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra

    -- ** Existential wrapper
    , InAnyRecentEra (..)
    , asAnyRecentEra
    , withRecentEra

    -- * TxOut
    , Babbage.TxOut (..)
    , TxOutInRecentEra (..)
    , unwrapTxOutInRecentEra
    , ErrInvalidTxOutInEra (..)

    -- ** Address
    , Address
    , unsafeAddressFromBytes

    -- ** Value
    , Value

    -- ** Datum
    , Datum (..)
    , DatumHash
    , BinaryData
    , datumFromBytes
    , datumToBytes
    , datumFromCardanoScriptData
    , datumToCardanoScriptData

    , datumHashFromBytes
    , datumHashToBytes

    -- ** Script
    , Script (..)
    , scriptFromBytes
    , scriptFromCardanoScriptInAnyLang
    , scriptToCardanoScriptInAnyLang
    , scriptToCardanoEnvelopeJSON
    , scriptFromCardanoEnvelopeJSON
    , Alonzo.isPlutusScript

    -- * TxIn
    , TxIn
    , unsafeMkTxIn

    -- * UTxO
    , Shelley.UTxO
    , utxoFromTxOutsInRecentEra
    , toCardanoUTxO
    , fromCardanoUTxO
    , utxoFromTxOuts

    )
    where

import Prelude

import Cardano.Api
    ( AlonzoEra, BabbageEra )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )
import Cardano.Crypto.Hash
    ( Hash (UnsafeHash) )
import Cardano.Ledger.Alonzo.Data
    ( BinaryData, Datum (..) )
import Cardano.Ledger.Alonzo.Scripts
    ( Script (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Crypto )
import Cardano.Ledger.Mary
    ( Value )
import Cardano.Ledger.SafeHash
    ( SafeHash, extractHash, unsafeMakeSafeHash )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Lazy
    ( fromStrict )
import Data.ByteString.Short
    ( toShort )
import Data.Maybe
    ( fromMaybe )
import Data.Maybe.Strict
    ( StrictMaybe (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Extra as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

type LatestEra = BabbageEra

type LatestLedgerEra = Cardano.ShelleyLedgerEra LatestEra

--------------------------------------------------------------------------------
-- RecentEra
--------------------------------------------------------------------------------

-- NOTE: We /could/ let 'era' refer to eras from the ledger rather than from
-- cardano-api.
data RecentEra era where
    RecentEraBabbage :: RecentEra BabbageEra
    RecentEraAlonzo :: RecentEra AlonzoEra

class Cardano.IsShelleyBasedEra era => IsRecentEra era where
    recentEra :: RecentEra era

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
isRecentEra :: Cardano.CardanoEra era -> Maybe (RecentEra era)
isRecentEra Cardano.BabbageEra = Just RecentEraBabbage
isRecentEra Cardano.AlonzoEra = Just RecentEraAlonzo
isRecentEra Cardano.MaryEra = Nothing
isRecentEra Cardano.AllegraEra = Nothing
isRecentEra Cardano.ShelleyEra = Nothing
isRecentEra Cardano.ByronEra = Nothing

instance IsRecentEra BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra AlonzoEra where
    recentEra = RecentEraAlonzo

cardanoEraFromRecentEra :: RecentEra era -> Cardano.CardanoEra era
cardanoEraFromRecentEra =
    Cardano.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra :: RecentEra era -> Cardano.ShelleyBasedEra era
shelleyBasedEraFromRecentEra RecentEraBabbage = Cardano.ShelleyBasedEraBabbage
shelleyBasedEraFromRecentEra RecentEraAlonzo = Cardano.ShelleyBasedEraAlonzo

-- | For convenience working with 'IsRecentEra'. Similar to 'Cardano.cardanoEra,
-- but with a 'IsRecentEra era' constraint instead of 'Cardano.IsCardanoEra.
cardanoEra :: forall era. IsRecentEra era => Cardano.CardanoEra era
cardanoEra = cardanoEraFromRecentEra $ recentEra @era

-- | For convenience working with 'IsRecentEra'. Similar to
-- 'Cardano.shelleyBasedEra, but with a 'IsRecentEra era' constraint instead of
-- 'Cardano.IsShelleyBasedEra'.
shelleyBasedEra :: forall era. IsRecentEra era => Cardano.ShelleyBasedEra era
shelleyBasedEra = shelleyBasedEraFromRecentEra $ recentEra @era

data InAnyRecentEra thing where
     InAnyRecentEra :: IsRecentEra era -- Provide class constraint
                    => RecentEra era   -- and explicit value.
                    -> thing era
                    -> InAnyRecentEra thing

withRecentEra
    :: InAnyRecentEra thing
    -> (forall era. IsRecentEra era => thing era -> a)
    -> a
withRecentEra (InAnyRecentEra _era tx) f
    = f tx

-- | "Downcast" something existentially wrapped in 'Cardano.InAnyCardanoEra'.
asAnyRecentEra
    :: Cardano.InAnyCardanoEra a
    -> Maybe (InAnyRecentEra a)
asAnyRecentEra = \case
    Cardano.InAnyCardanoEra Cardano.BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    Cardano.InAnyCardanoEra Cardano.AlonzoEra a ->
        Just $ InAnyRecentEra RecentEraAlonzo a
    _ -> Nothing

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

type TxIn = Ledger.TxIn StandardCrypto

-- | Useful for testing
unsafeMkTxIn :: ByteString -> Word -> TxIn
unsafeMkTxIn hash ix = Ledger.mkTxInPartial
    (toTxId hash)
    (fromIntegral ix)
  where
    toTxId :: ByteString -> Ledger.TxId StandardCrypto
    toTxId h =
        (Ledger.TxId (unsafeMakeSafeHash $ UnsafeHash $ toShort h))

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

type TxOut era = Core.TxOut era

type Address = Ledger.Addr StandardCrypto

unsafeAddressFromBytes :: ByteString -> Address
unsafeAddressFromBytes bytes = case Ledger.deserialiseAddr bytes of
    Just addr -> addr
    Nothing -> error "unsafeAddressFromBytes: failed to deserialise"

scriptFromBytes
    :: ByteString
    -> Either CBOR.DecoderError (Script LatestLedgerEra)
scriptFromBytes bs = CBOR.decodeAnnotator "Script" CBOR.fromCBOR (fromStrict bs)

scriptFromCardanoScriptInAnyLang
    :: Cardano.ScriptInAnyLang
    -> (Script LatestLedgerEra)
scriptFromCardanoScriptInAnyLang
    = Cardano.toShelleyScript
    . fromMaybe (error "all valid scripts should be valid in latest era")
    . Cardano.toScriptInEra latestEra
  where
    latestEra = Cardano.BabbageEra

scriptToCardanoScriptInAnyLang
    :: Script LatestLedgerEra
    -> Cardano.ScriptInAnyLang
scriptToCardanoScriptInAnyLang =
    rewrap
    . Cardano.fromShelleyBasedScript latestEra
  where
    rewrap (Cardano.ScriptInEra _ s) = Cardano.toScriptInAnyLang s
    latestEra = Cardano.ShelleyBasedEraBabbage

scriptToCardanoEnvelopeJSON :: Script LatestLedgerEra -> Aeson.Value
scriptToCardanoEnvelopeJSON = scriptToJSON . scriptToCardanoScriptInAnyLang
  where
    scriptToJSON
        :: Cardano.ScriptInAnyLang
        -> Aeson.Value
    scriptToJSON (Cardano.ScriptInAnyLang l s) = Aeson.toJSON
            $ obtainScriptLangConstraint l
            $ Cardano.serialiseToTextEnvelope Nothing s
      where
        obtainScriptLangConstraint
          :: Cardano.ScriptLanguage lang
          -> (Cardano.IsScriptLanguage lang => a)
          -> a
        obtainScriptLangConstraint lang f = case lang of
            (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV1) -> f
            (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV2) -> f
            (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV1) -> f
            (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV2) -> f

scriptFromCardanoEnvelopeJSON
    :: Aeson.Value
    -> Aeson.Parser (Script LatestLedgerEra)
scriptFromCardanoEnvelopeJSON v = fmap scriptFromCardanoScriptInAnyLang $ do
    envelope <- Aeson.parseJSON v
    case textEnvelopeToScript envelope of
      Left textEnvErr -> fail $ Cardano.displayError textEnvErr
      Right (Cardano.ScriptInAnyLang l s) -> pure $ Cardano.ScriptInAnyLang l s
  where
    textEnvelopeToScript
        :: Cardano.TextEnvelope
        -> Either Cardano.TextEnvelopeError Cardano.ScriptInAnyLang
    textEnvelopeToScript =
        Cardano.deserialiseFromTextEnvelopeAnyOf textEnvTypes

    textEnvTypes
      :: [Cardano.FromSomeType Cardano.HasTextEnvelope Cardano.ScriptInAnyLang]
    textEnvTypes =
      [ Cardano.FromSomeType
          (Cardano.AsScript Cardano.AsSimpleScriptV1)
          (Cardano.ScriptInAnyLang
              (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV1))
      , Cardano.FromSomeType
          (Cardano.AsScript Cardano.AsSimpleScriptV2)
          (Cardano.ScriptInAnyLang
              (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV2))
      , Cardano.FromSomeType
          (Cardano.AsScript Cardano.AsPlutusScriptV1)
          (Cardano.ScriptInAnyLang
              (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV1))
      , Cardano.FromSomeType
          (Cardano.AsScript Cardano.AsPlutusScriptV2)
          (Cardano.ScriptInAnyLang
              (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV2))
      ]

-- NOTE on binary format: There are a couple of related types in the ledger each
-- with their own binary encoding. 'Plutus.Data' seems to be the type with the
-- least amount of wrapping tags in the encoding.
--
-- - 'Plutus.Data' - the simplest encoding of the following options
-- - 'Alonzo.BinaryData' - adds a preceding @24@ tag
-- - 'Alonzo.Data' - n/a; doesn't have a ToCBOR
-- - 'Alonzo.Datum' - adds tags to differentiate between e.g. inline datums and
-- datum hashes. We could add helpers for this roundtrip, but they would be
-- separate from the existing 'datum{From,To}Bytes' pair.
datumFromBytes
    :: ByteString
    -> Either String (BinaryData LatestLedgerEra)
datumFromBytes =
    Alonzo.makeBinaryData . toShort

datumToBytes :: BinaryData LatestLedgerEra -> ByteString
datumToBytes = CBOR.serialize' . Alonzo.getPlutusData . Alonzo.binaryDataToData

datumFromCardanoScriptData
    :: Cardano.ScriptData
    -> BinaryData LatestLedgerEra
datumFromCardanoScriptData =
    Alonzo.dataToBinaryData
    . Cardano.toAlonzoData

datumToCardanoScriptData
    :: BinaryData LatestLedgerEra
    -> Cardano.ScriptData
datumToCardanoScriptData =
    Cardano.fromAlonzoData
    . Alonzo.binaryDataToData

type DatumHash = Alonzo.DataHash StandardCrypto

datumHashFromBytes :: ByteString -> Maybe DatumHash
datumHashFromBytes =
    fmap unsafeMakeSafeHash <$> Crypto.hashFromBytes

datumHashToBytes :: SafeHash crypto a -> ByteString
datumHashToBytes = Crypto.hashToBytes . extractHash

-- | Type representing a TxOut in the latest or previous era.
--
-- The underlying respresentation is isomorphic to 'TxOut LatestLedgerEra'.
--
-- Can be unwrapped using 'unwrapTxOutInRecentEra' or
-- 'utxoFromTxOutsInRecentEra'.
--
-- Implementation assumes @TxOut latestEra ⊇ TxOut prevEra@ in the sense that
-- the latest era has not removed information from the @TxOut@. This is allows
-- e.g. @ToJSON@ / @FromJSON@ instances to be written for two eras using only
-- one implementation.
data TxOutInRecentEra
    = TxOutInRecentEra
        Address
        (Value LatestLedgerEra)
        (Datum LatestLedgerEra)
        (Maybe (Script LatestLedgerEra))
        -- Same contents as 'TxOut LatestLedgerEra'.

data ErrInvalidTxOutInEra
    = ErrInlineDatumNotSupportedInAlonzo
    | ErrInlineScriptNotSupportedInAlonzo

unwrapTxOutInRecentEra
    :: RecentEra era
    -> TxOutInRecentEra
    -> Either ErrInvalidTxOutInEra (TxOut (ShelleyLedgerEra era))
unwrapTxOutInRecentEra era recentEraTxOut = case era of
    RecentEraBabbage -> pure $ castTxOut recentEraTxOut
    RecentEraAlonzo -> downcastTxOut recentEraTxOut
  where
    castTxOut
        :: TxOutInRecentEra
        -> TxOut (ShelleyLedgerEra BabbageEra)
    castTxOut (TxOutInRecentEra addr val datum mscript) =
        (Babbage.TxOut addr val datum (toStrict mscript))
      where
        toStrict (Just a) = SJust a
        toStrict Nothing = SNothing

    downcastTxOut
        :: TxOutInRecentEra
        -> Either
            ErrInvalidTxOutInEra
            (Core.TxOut (ShelleyLedgerEra AlonzoEra))
    downcastTxOut (TxOutInRecentEra _addr _val _datum (Just _script))
        = Left ErrInlineScriptNotSupportedInAlonzo
    downcastTxOut (TxOutInRecentEra _addr _val (Alonzo.Datum _) _script)
        = Left ErrInlineDatumNotSupportedInAlonzo
    downcastTxOut (TxOutInRecentEra addr val Alonzo.NoDatum Nothing)
        = Right $ Alonzo.TxOut addr val SNothing
    downcastTxOut (TxOutInRecentEra addr val (Alonzo.DatumHash dh) Nothing)
        = Right $ Alonzo.TxOut addr val (SJust dh)

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Construct a 'UTxO era' using 'TxIn's and 'TxOut's in said era.
utxoFromTxOuts
    :: RecentEra era
    -> [(TxIn, Core.TxOut (ShelleyLedgerEra era))]
    -> (Shelley.UTxO (ShelleyLedgerEra era))
utxoFromTxOuts era = withStandardCryptoConstraint era $
    Shelley.UTxO . Map.fromList

-- | Construct a 'UTxO era' using 'TxOutInRecentEra'. Fails if any output is
-- invalid in the targeted 'era'.
utxoFromTxOutsInRecentEra
    :: forall era. RecentEra era
    -> [(TxIn, TxOutInRecentEra)]
    -> Either ErrInvalidTxOutInEra (Shelley.UTxO (ShelleyLedgerEra era))
utxoFromTxOutsInRecentEra era = withStandardCryptoConstraint era $
    fmap (Shelley.UTxO . Map.fromList) . mapM downcast
  where
    downcast
        :: (TxIn, TxOutInRecentEra)
        -> Either
            ErrInvalidTxOutInEra
            (TxIn, TxOut (ShelleyLedgerEra era))
    downcast (i, o) = do
        o' <- unwrapTxOutInRecentEra era o
        pure (i, o')

--------------------------------------------------------------------------------
-- Compatibility
--------------------------------------------------------------------------------

toCardanoUTxO
    :: forall era. IsRecentEra era
    => Shelley.UTxO (ShelleyLedgerEra era)
    -> Cardano.UTxO era
toCardanoUTxO = withConstraints $
    Cardano.UTxO
    . Map.mapKeys Cardano.fromShelleyTxIn
    . Map.map (Cardano.fromShelleyTxOut (shelleyBasedEra @era))
    . unUTxO
  where
    unUTxO (Shelley.UTxO m) = m

    withConstraints
        :: ((Crypto (Cardano.ShelleyLedgerEra era) ~ StandardCrypto) => a)
        -> a
    withConstraints a = case recentEra @era of
        RecentEraBabbage -> a
        RecentEraAlonzo -> a

fromCardanoUTxO
    :: forall era. IsRecentEra era
    => Cardano.UTxO era
    -> Shelley.UTxO (Cardano.ShelleyLedgerEra era)
fromCardanoUTxO = withStandardCryptoConstraint (recentEra @era) $
    Shelley.UTxO
    . Map.mapKeys Cardano.toShelleyTxIn
    . Map.map (Cardano.toShelleyTxOut (shelleyBasedEra @era))
    . unCardanoUTxO
  where
    unCardanoUTxO (Cardano.UTxO m) = m

--------------------------------------------------------------------------------
-- Module-internal helpers
--------------------------------------------------------------------------------

withStandardCryptoConstraint
    :: RecentEra era
    -> ((Crypto (Cardano.ShelleyLedgerEra era) ~ StandardCrypto) => a)
    -> a
withStandardCryptoConstraint era a = case era of
    RecentEraBabbage -> a
    RecentEraAlonzo -> a
