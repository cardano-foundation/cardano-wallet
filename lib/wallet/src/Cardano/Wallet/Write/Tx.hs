{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.Write.Tx
    (
    -- * RecentEra
      RecentEra (..)
    , IsRecentEra (..)
    , isRecentEra

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra

    -- ** Existential wrapper
    , InAnyRecentEra (..)
    , asAnyRecentEra
    , withRecentEra

    -- * TxOut
    , Babbage.TxOut (..) -- FIXME!
    , TxOutInRecentEra -- opaque
    , wrapTxOutInRecentEra
    , unwrapTxOutInRecentEra
    , ErrInvalidTxOutInEra (..)

    -- ** Address
    , Address
    , unsafeAddressFromBytes

    -- ** Value
    , Value

    -- ** Datum
    , type Datum
    , type DatumHash
    , type BinaryData
    , pattern Alonzo.Datum
    , pattern Alonzo.DatumHash
    , pattern Alonzo.NoDatum
    , datumFromBytes
    , datumToBytes
    , datumFromCardanoScriptData
    , datumToCardanoScriptData

    , datumHashFromBytes
    , datumHashToBytes

    -- ** Script
    , type Script
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
    ( AlonzoEra, BabbageEra, CardanoEra (..) )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )
import Cardano.Crypto.Hash
    ( Hash (UnsafeHash) )
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
import Ouroboros.Consensus.Cardano.Block
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
isRecentEra :: CardanoEra era -> Maybe (RecentEra era)
isRecentEra BabbageEra = Just RecentEraBabbage
isRecentEra AlonzoEra = Just RecentEraAlonzo
isRecentEra MaryEra = Nothing
isRecentEra AllegraEra = Nothing
isRecentEra ShelleyEra = Nothing
isRecentEra ByronEra = Nothing

instance IsRecentEra Cardano.BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra Cardano.AlonzoEra where
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
    Cardano.InAnyCardanoEra BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    Cardano.InAnyCardanoEra AlonzoEra a ->
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

type Script = Alonzo.Script (Babbage.BabbageEra StandardCrypto)
-- NOTE: Era only used for its 'Crypto era'

scriptFromBytes :: ByteString -> Either CBOR.DecoderError Script
scriptFromBytes bs = CBOR.decodeAnnotator "Script" CBOR.fromCBOR (fromStrict bs)

scriptFromCardanoScriptInAnyLang
    :: Cardano.ScriptInAnyLang
    -> Script
scriptFromCardanoScriptInAnyLang
    = Cardano.toShelleyScript
    . fromMaybe (error "all valid scripts should be valid in latest era")
    . Cardano.toScriptInEra latestEra
  where
    latestEra = Cardano.BabbageEra

scriptToCardanoScriptInAnyLang
    :: Script
    -> Cardano.ScriptInAnyLang
scriptToCardanoScriptInAnyLang =
    rewrap
    . Cardano.fromShelleyBasedScript latestEra
  where
    rewrap (Cardano.ScriptInEra _ s) = Cardano.toScriptInAnyLang s
    latestEra = Cardano.ShelleyBasedEraBabbage

scriptToCardanoEnvelopeJSON :: Script -> Aeson.Value
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

scriptFromCardanoEnvelopeJSON :: Aeson.Value -> Aeson.Parser Script
scriptFromCardanoEnvelopeJSON v = fmap scriptFromCardanoScriptInAnyLang $ do
    envelope <- Aeson.parseJSON v
    case textEnvelopeToScript envelope of
      Left textEnvErr -> fail $ Cardano.displayError textEnvErr
      Right (Cardano.ScriptInAnyLang l s) -> pure $ Cardano.ScriptInAnyLang l s
  where
    textEnvelopeToScript :: Cardano.TextEnvelope -> Either Cardano.TextEnvelopeError Cardano.ScriptInAnyLang
    textEnvelopeToScript = Cardano.deserialiseFromTextEnvelopeAnyOf textEnvTypes
     where
      textEnvTypes :: [Cardano.FromSomeType Cardano.HasTextEnvelope Cardano.ScriptInAnyLang]
      textEnvTypes =
        [ Cardano.FromSomeType (Cardano.AsScript Cardano.AsSimpleScriptV1)
                       (Cardano.ScriptInAnyLang (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV1))
        , Cardano.FromSomeType (Cardano.AsScript Cardano.AsSimpleScriptV2)
                       (Cardano.ScriptInAnyLang (Cardano.SimpleScriptLanguage Cardano.SimpleScriptV2))
        , Cardano.FromSomeType (Cardano.AsScript Cardano.AsPlutusScriptV1)
                       (Cardano.ScriptInAnyLang (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV1))
        , Cardano.FromSomeType (Cardano.AsScript Cardano.AsPlutusScriptV2)
                       (Cardano.ScriptInAnyLang (Cardano.PlutusScriptLanguage Cardano.PlutusScriptV2))
        ]

type Datum = Alonzo.Datum (Babbage.BabbageEra StandardCrypto)

type BinaryData = Alonzo.BinaryData (Babbage.BabbageEra StandardCrypto)

type DatumHash = Alonzo.DataHash StandardCrypto

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
datumFromBytes :: ByteString -> Either String (Alonzo.BinaryData StandardBabbage)
datumFromBytes =
    Alonzo.makeBinaryData . toShort

datumToBytes :: Alonzo.BinaryData StandardBabbage -> ByteString
datumToBytes = CBOR.serialize' . Alonzo.getPlutusData . Alonzo.binaryDataToData

datumFromCardanoScriptData
    :: Cardano.ScriptData
    -> Alonzo.BinaryData StandardBabbage
datumFromCardanoScriptData =
    Alonzo.dataToBinaryData
    . Cardano.toAlonzoData

datumToCardanoScriptData
    :: Alonzo.BinaryData StandardBabbage
    -> Cardano.ScriptData
datumToCardanoScriptData =
    Cardano.fromAlonzoData
    . Alonzo.binaryDataToData

datumHashFromBytes :: ByteString -> Maybe DatumHash
datumHashFromBytes =
    fmap unsafeMakeSafeHash <$> Crypto.hashFromBytes

datumHashToBytes :: SafeHash crypto a -> ByteString
datumHashToBytes = Crypto.hashToBytes . extractHash

-- | Type representing a TxOut in the latest **and/or** previous era. I.e. the
-- type is similar to @Data.These latestEraTxOut prevEraTxOut@.
--
-- Assumes @TxOut latestEra ⊇ TxOut prevEra@ in the sense that the latest era
-- has not removed information from the @TxOut@. This is allows e.g.
-- @ToJSON@ / @FromJSON@ instances to be written for two eras using only one
-- implementation.
newtype TxOutInRecentEra
    = TxOutInRecentEra (Core.TxOut (ShelleyLedgerEra BabbageEra))
        -- NOTE: Assuming @TxOut BabbageEra@ contains the same
        -- information as @TxOut AlonzoEra@ or more.

data ErrInvalidTxOutInEra
    = ErrInlineDatumNotSupportedInAlonzo
    | ErrInlineScriptNotSupportedInAlonzo

wrapTxOutInRecentEra
    :: Core.TxOut (ShelleyLedgerEra BabbageEra)
    -> TxOutInRecentEra
wrapTxOutInRecentEra = TxOutInRecentEra

unwrapTxOutInRecentEra
    :: RecentEra era
    -> TxOutInRecentEra
    -> Either ErrInvalidTxOutInEra (Core.TxOut (ShelleyLedgerEra era))
unwrapTxOutInRecentEra era (TxOutInRecentEra babbageTxOut) = case era of
    RecentEraBabbage -> pure babbageTxOut
    RecentEraAlonzo -> downcastTxOut babbageTxOut
  where
    downcastTxOut
        :: Core.TxOut (ShelleyLedgerEra BabbageEra)
        -> Either
            ErrInvalidTxOutInEra
            (Core.TxOut (ShelleyLedgerEra AlonzoEra))
    downcastTxOut (Babbage.TxOut _addr _val _datum (SJust _script))
        = Left ErrInlineScriptNotSupportedInAlonzo
    downcastTxOut (Babbage.TxOut _addr _val (Alonzo.Datum _) _script)
        = Left ErrInlineDatumNotSupportedInAlonzo
    downcastTxOut (Babbage.TxOut addr val Alonzo.NoDatum SNothing)
        = Right $ Alonzo.TxOut addr val SNothing
    downcastTxOut (Babbage.TxOut addr val (Alonzo.DatumHash dh) SNothing)
        = Right $ Alonzo.TxOut addr val (SJust dh)

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

utxoFromTxOuts
    :: RecentEra era
    -> [(TxIn, Core.TxOut (ShelleyLedgerEra era))]
    -> (Shelley.UTxO (ShelleyLedgerEra era))
utxoFromTxOuts era = withStandardCryptoConstraint era $
    Shelley.UTxO . Map.fromList

utxoFromTxOutsInRecentEra
    :: forall era. RecentEra era
    -> [(TxIn, TxOutInRecentEra)]
    -> Either ErrInvalidTxOutInEra (Shelley.UTxO (ShelleyLedgerEra era))
utxoFromTxOutsInRecentEra era = withStandardCryptoConstraint era $
    fmap (Shelley.UTxO . Map.fromList) . mapM (downcast)
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
