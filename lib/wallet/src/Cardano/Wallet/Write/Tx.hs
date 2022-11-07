{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module containing primitive types and functionality appropriate for
-- constructing transactions.
--
-- Indented as a replacement to 'cardano-api' closer
-- to the ledger types, and only caring about the two latest eras (Cf.
-- 'RecentEra'). Intended to be used by things like balanceTx, constructTx and
-- wallet migration.
module Cardano.Wallet.Write.Tx
    (
    -- * Eras

    -- ** RecentEra
      RecentEra (..)
    , IsRecentEra (..)
    , toRecentEra
    , LatestLedgerEra
    , LatestEra

    -- ** Helpers for cardano-api compatibility
    , ShelleyLedgerEra
    , CardanoApiEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra

    -- ** Misc
    , StandardCrypto
    , StandardBabbage
    , StandardAlonzo


    -- * PParams
    , Core.PParams

    -- * Tx
    , Core.Tx
    , modifyTxOutputs
    , outputs
    , body
    , modifyLedgerBody

    -- * TxOut
    , Core.TxOut
    , TxOutInBabbage
    , TxOutInRecentEra (..)
    , unwrapTxOutInRecentEra
    , ErrInvalidTxOutInEra (..)
    , modifyTxOutValue
    , modifyTxOutCoin
    , value

    , computeMinimumCoinForTxOut
    , setMinimumCoinForTxOut
    , isBelowMinimumCoinForTxOut

    -- ** Address
    , Address
    , unsafeAddressFromBytes

    -- ** Value
    , Value
    , modifyCoin
    , coin
    , Coin

    -- ** Datum
    , Datum (..)
    , datumFromCardanoScriptData
    , datumToCardanoScriptData

    -- *** Binary Data
    , BinaryData
    , binaryDataFromBytes
    , binaryDataToBytes

    -- *** Datum Hash
    , DatumHash
    , datumHashFromBytes
    , datumHashToBytes

    -- ** Script
    , Script (..)
    , scriptFromCardanoScriptInAnyLang
    , scriptToCardanoScriptInAnyLang
    , scriptToCardanoEnvelopeJSON
    , scriptFromCardanoEnvelopeJSON
    , Alonzo.isPlutusScript

    -- * TxIn
    , TxIn
    , unsafeMkTxIn

    -- * UTxO
    , Shelley.UTxO (..)
    , utxoFromTxOutsInRecentEra
    , utxoFromTxOutsInLatestEra
    , utxoFromTxOuts
    , toCardanoUTxO
    , fromCardanoUTxO

    , withEraConversionConstraints
    )
    where

import Prelude

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
import Cardano.Ledger.Serialization
    ( Sized (..), mkSized )
import Cardano.Ledger.Val
    ( coin, modifyCoin, (<->) )
import Control.Arrow
    ( second )
import Data.ByteString
    ( ByteString )
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
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.API
    ( CLI (evaluateMinLovelaceOutput), Coin )
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable
    ( toList )
import qualified Data.Map as Map
import Test.Cardano.Ledger.Alonzo.Examples.Consensus
    ( StandardAlonzo )

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------
--
type BabbageEra = StandardBabbage
type AlonzoEra = StandardAlonzo

type LatestEra = BabbageEra

type LatestLedgerEra = StandardBabbage

--------------------------------------------------------------------------------
-- RecentEra
--------------------------------------------------------------------------------


-- | 'RecentEra' respresents the eras we care about constructing transactions
-- for.
--
-- To have the same software constructing transactions just before and just
-- after a hard-fork, we need to, at that time, support the two latest eras. We
-- could get away with just supporting one era at other times, but for
-- simplicity we stick with always supporting the two latest eras for now.
--
-- NOTE: We /could/ let 'era' refer to eras from the ledger rather than from
-- cardano-api.
data RecentEra era where
    RecentEraBabbage :: RecentEra BabbageEra
    RecentEraAlonzo :: RecentEra AlonzoEra

-- |
--
-- @
--    CardanoApiEra (ShelleyLedgerEra cardanoEra) ~ cardanoEra
--    ShelleyLedgerEra (CardanoApiEra era) ~ era
--  @
type family CardanoApiEra era where
  CardanoApiEra BabbageEra = Cardano.BabbageEra
  CardanoApiEra AlonzoEra = Cardano.AlonzoEra


class Cardano.IsShelleyBasedEra (CardanoApiEra era) => IsRecentEra era where
    recentEra :: RecentEra era

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
toRecentEra
    :: Cardano.CardanoEra cardanoEra
    -> Maybe (RecentEra (Cardano.ShelleyLedgerEra cardanoEra))
toRecentEra = \case
    Cardano.BabbageEra -> Just RecentEraBabbage
    Cardano.AlonzoEra  -> Just RecentEraAlonzo
    Cardano.MaryEra    -> Nothing
    Cardano.AllegraEra -> Nothing
    Cardano.ShelleyEra -> Nothing
    Cardano.ByronEra   -> Nothing

instance IsRecentEra BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra AlonzoEra where
    recentEra = RecentEraAlonzo

cardanoEraFromRecentEra
    :: RecentEra era
    -> Cardano.CardanoEra (CardanoApiEra era)
cardanoEraFromRecentEra =
    Cardano.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra
    :: RecentEra era
    -> Cardano.ShelleyBasedEra (CardanoApiEra era)
shelleyBasedEraFromRecentEra = \case
    RecentEraBabbage -> Cardano.ShelleyBasedEraBabbage
    RecentEraAlonzo -> Cardano.ShelleyBasedEraAlonzo

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

modifyTxOutValue
    :: RecentEra era
    -> (Value era -> Value era)
    -> TxOut era
    -> TxOut era
modifyTxOutValue RecentEraBabbage f (Babbage.TxOut addr val dat script)=
        withStandardCryptoConstraint RecentEraBabbage $ Babbage.TxOut addr (f val) dat script
modifyTxOutValue RecentEraAlonzo f (Alonzo.TxOut addr val dat) =
        withStandardCryptoConstraint RecentEraAlonzo $ Alonzo.TxOut addr (f val) dat

modifyTxOutCoin
    :: RecentEra era
    -> (Coin -> Coin)
    -> TxOut era
    -> TxOut era
modifyTxOutCoin era f = withStandardCryptoConstraint era $
    modifyTxOutValue era (modifyCoin f)

value
    :: RecentEra era
    -> TxOut era
    -> Value era
value RecentEraBabbage (Babbage.TxOut _ val _ _) = val
value RecentEraAlonzo (Alonzo.TxOut _ val _) = val

type TxOutInBabbage = Babbage.TxOut (Babbage.BabbageEra StandardCrypto)

type Address = Ledger.Addr StandardCrypto

unsafeAddressFromBytes :: ByteString -> Address
unsafeAddressFromBytes bytes = case Ledger.deserialiseAddr bytes of
    Just addr -> addr
    Nothing -> error "unsafeAddressFromBytes: failed to deserialise"

scriptFromCardanoScriptInAnyLang
    :: Cardano.ScriptInAnyLang
    -> Script LatestLedgerEra
scriptFromCardanoScriptInAnyLang
    = Cardano.toShelleyScript
    . fromMaybe (error "all valid scripts should be valid in latest era")
    . Cardano.toScriptInEra latestEra
  where
    latestEra = Cardano.BabbageEra

-- | NOTE: The roundtrip
-- @
--     scriptToCardanoScriptInAnyLang . scriptFromCardanoScriptInAnyLang
-- @
-- will convert 'SimpleScriptV1' to 'SimpleScriptV2'. Because 'SimpleScriptV1'
-- is 'ShelleyEra'-specific, and 'ShelleyEra' is not a 'RecentEra', this should
-- not be a problem.
scriptToCardanoScriptInAnyLang
    :: Script LatestLedgerEra
    -> Cardano.ScriptInAnyLang
scriptToCardanoScriptInAnyLang =
    rewrap
    . Cardano.fromShelleyBasedScript latestEra
  where
    rewrap (Cardano.ScriptInEra _ s) = Cardano.toScriptInAnyLang s
    latestEra = Cardano.ShelleyBasedEraBabbage

-- | NOTE: The roundtrip
-- @
--     scriptToCardanoEnvelopeJSON . scriptFromCardanoEnvelopeJSON
-- @
-- will convert 'SimpleScriptV1' to 'SimpleScriptV2'. Because 'SimpleScriptV1'
-- is 'ShelleyEra'-specific, and 'ShelleyEra' is not a 'RecentEra', this should
-- not be a problem.
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
            Cardano.SimpleScriptLanguage Cardano.SimpleScriptV1 -> f
            Cardano.SimpleScriptLanguage Cardano.SimpleScriptV2 -> f
            Cardano.PlutusScriptLanguage Cardano.PlutusScriptV1 -> f
            Cardano.PlutusScriptLanguage Cardano.PlutusScriptV2 -> f

scriptFromCardanoEnvelopeJSON
    :: Aeson.Value
    -> Aeson.Parser (Script LatestLedgerEra)
scriptFromCardanoEnvelopeJSON v = fmap scriptFromCardanoScriptInAnyLang $ do
    envelope <- Aeson.parseJSON v
    case textEnvelopeToScript envelope of
        Left textEnvErr
            -> fail $ Cardano.displayError textEnvErr
        Right (Cardano.ScriptInAnyLang l s)
            -> pure $ Cardano.ScriptInAnyLang l s
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
binaryDataFromBytes
    :: ByteString
    -> Either String (BinaryData LatestLedgerEra)
binaryDataFromBytes =
    Alonzo.makeBinaryData . toShort

binaryDataToBytes :: BinaryData LatestLedgerEra -> ByteString
binaryDataToBytes =
    CBOR.serialize'
    . Alonzo.getPlutusData
    . Alonzo.binaryDataToData

datumFromCardanoScriptData
    :: IsRecentEra era
    => Cardano.ScriptData
    -> BinaryData era
datumFromCardanoScriptData =
    Alonzo.dataToBinaryData
    . Cardano.toAlonzoData

datumToCardanoScriptData
    :: IsRecentEra era
    => BinaryData era
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
    -> Either ErrInvalidTxOutInEra (TxOut era)
unwrapTxOutInRecentEra era recentEraTxOut = case era of
    RecentEraBabbage -> pure $ castTxOut recentEraTxOut
    RecentEraAlonzo -> downcastTxOut recentEraTxOut

castTxOut
    :: TxOutInRecentEra
    -> TxOut BabbageEra
castTxOut (TxOutInRecentEra addr val datum mscript) =
    (Babbage.TxOut addr val datum (toStrict mscript))
  where
    toStrict (Just a) = SJust a
    toStrict Nothing = SNothing

downcastTxOut
    :: TxOutInRecentEra
    -> Either
        ErrInvalidTxOutInEra
        (Core.TxOut AlonzoEra)
downcastTxOut (TxOutInRecentEra _addr _val _datum (Just _script))
    = Left ErrInlineScriptNotSupportedInAlonzo
downcastTxOut (TxOutInRecentEra _addr _val (Alonzo.Datum _) _script)
    = Left ErrInlineDatumNotSupportedInAlonzo
downcastTxOut (TxOutInRecentEra addr val Alonzo.NoDatum Nothing)
    = Right $ Alonzo.TxOut addr val SNothing
downcastTxOut (TxOutInRecentEra addr val (Alonzo.DatumHash dh) Nothing)
    = Right $ Alonzo.TxOut addr val (SJust dh)

--
-- MinimumUTxO
--

-- |
--
-- prop> forall out.
-- prop> let
-- prop>     c = computeMinimumCoinForUTxO out
-- prop> in
-- prop>     forall c' > c. satisfiesMinimumUTxO (out { coin = c' })
--
-- NOTE:
computeMinimumCoinForTxOut
    :: forall era. RecentEra era -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams era
    -> TxOut era
    -> Coin
computeMinimumCoinForTxOut era pp out = withCLIConstraint era $
    evaluateMinLovelaceOutput pp (withMaxLengthSerializedCoin out)
  where
    withMaxLengthSerializedCoin
        :: TxOut era
        -> TxOut era
    withMaxLengthSerializedCoin =
        withStandardCryptoConstraint era $
            modifyTxOutValue era $ modifyCoin (const $ toLedger txOutMaxCoin)

isBelowMinimumCoinForTxOut
    :: forall era. RecentEra era -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams era
    -> TxOut era
    -> Either Coin ()
isBelowMinimumCoinForTxOut era pp out = withCLIConstraint era $
    let
        requiredMin = evaluateMinLovelaceOutput pp out
        actualCoin = getCoin era out
    in
        if actualCoin >= requiredMin
        then Right ()
        else Left $ requiredMin <-> actualCoin

getCoin :: RecentEra era -> TxOut era -> Coin
getCoin RecentEraBabbage (Babbage.TxOut _ val _ _) = coin val
getCoin RecentEraAlonzo (Alonzo.TxOut _ val _) = coin val


setMinimumCoinForTxOut
    :: forall era. RecentEra era -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams era
    -> TxOut era
    -> TxOut era
setMinimumCoinForTxOut era pp out =
    let
        minCoin = computeMinimumCoinForTxOut era pp out
    in
        withStandardCryptoConstraint era $
            modifyTxOutValue era (modifyCoin (const minCoin)) out

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Construct a 'UTxO era' using 'TxIn's and 'TxOut's in said era.
utxoFromTxOuts
    :: RecentEra era
    -> [(TxIn, Core.TxOut era)]
    -> (Shelley.UTxO era)
utxoFromTxOuts era = withStandardCryptoConstraint era $
    Shelley.UTxO . Map.fromList

-- | Construct a 'UTxO era' using 'TxOutInRecentEra'. Fails if any output is
-- invalid in the targeted 'era'.
utxoFromTxOutsInRecentEra
    :: forall era. RecentEra era
    -> [(TxIn, TxOutInRecentEra)]
    -> Either ErrInvalidTxOutInEra (Shelley.UTxO era)
utxoFromTxOutsInRecentEra era = withStandardCryptoConstraint era $
    fmap (Shelley.UTxO . Map.fromList) . mapM downcast
  where
    downcast
        :: (TxIn, TxOutInRecentEra)
        -> Either
            ErrInvalidTxOutInEra
            (TxIn, TxOut era)
    downcast (i, o) = do
        o' <- unwrapTxOutInRecentEra era o
        pure (i, o')

-- | Useful for testing.
utxoFromTxOutsInLatestEra
    :: [(TxIn, TxOutInRecentEra)]
    -> Shelley.UTxO LatestLedgerEra
utxoFromTxOutsInLatestEra = withStandardCryptoConstraint RecentEraBabbage $
    Shelley.UTxO . Map.fromList . map (second castTxOut)

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

modifyTxOutputs
    :: RecentEra era
    -> (TxOut era -> TxOut era)
    -> Core.TxBody era
    -> Core.TxBody era
modifyTxOutputs era f body = case era of
    RecentEraBabbage -> body
        { Babbage.outputs = mapSized f <$> Babbage.outputs body
        }
    RecentEraAlonzo -> body
        { Alonzo.outputs = f <$> Alonzo.outputs body
        }
  where
    mapSized f = mkSized . f . sizedValue

body
    :: RecentEra era
    -> Core.Tx era
    -> Core.TxBody era
body RecentEraBabbage = Alonzo.body -- same type for babbage
body RecentEraAlonzo = Alonzo.body

-- Until we have convenient lenses to use
outputs
    :: RecentEra era
    -> Core.TxBody era
    -> [TxOut era]
outputs RecentEraBabbage = map sizedValue . toList . Babbage.outputs
outputs RecentEraAlonzo = toList . Alonzo.outputs

--------------------------------------------------------------------------------
-- Compatibility
--------------------------------------------------------------------------------
-- | NOTE: The roundtrip
-- @
--     toCardanoUTxO . fromCardanoUTxO
-- @
-- will mark any 'SimpleScriptV1' reference scripts as 'SimpleScriptV2'. Because
-- 'SimpleScriptV1' is 'ShelleyEra'-specific, and 'ShelleyEra' is not a
-- 'RecentEra', this should not be a problem.
toCardanoUTxO
    :: forall era. IsRecentEra era
    => Shelley.UTxO era
    -> Cardano.UTxO (CardanoApiEra era)
toCardanoUTxO = withStandardCryptoConstraint era $
    withEraConversionConstraints era $
        Cardano.UTxO
        . Map.mapKeys Cardano.fromShelleyTxIn
        . Map.map (Cardano.fromShelleyTxOut (shelleyBasedEraFromRecentEra era))
        . unUTxO
  where
    era = recentEra @era
    unUTxO (Shelley.UTxO m) = m


fromCardanoUTxO
    :: forall era. IsRecentEra era
    => Cardano.UTxO (CardanoApiEra era)
    -> Shelley.UTxO era
fromCardanoUTxO = withStandardCryptoConstraint era $
      withEraConversionConstraints era $
        Shelley.UTxO
        . Map.mapKeys Cardano.toShelleyTxIn
        . Map.map (Cardano.toShelleyTxOut (shelleyBasedEraFromRecentEra era))
        . unCardanoUTxO
  where
    era = recentEra @era
    unCardanoUTxO (Cardano.UTxO m) = m

-- NOTE: To reduce the need for the caller to deal with @CardanoApiEra
-- (ShelleyLedgerEra era) ~ era@, we quantify this function over @cardanoEra@
-- instead of @era@.
modifyLedgerBody
    :: RecentEra (ShelleyLedgerEra cardanoEra)
    -> (Core.TxBody (ShelleyLedgerEra cardanoEra)
        -> Core.TxBody (ShelleyLedgerEra cardanoEra))
    -> Cardano.Tx cardanoEra
    -> Cardano.Tx cardanoEra
modifyLedgerBody era f (Cardano.Tx body keyWits) =
    withEraConversionConstraints era $
        let
            Cardano.ShelleyTxBody
                shelleyEra
                ledgerBody
                scripts
                scriptData
                auxData
                validity
                = body
            body' = Cardano.ShelleyTxBody
                shelleyEra
                (f ledgerBody)
                scripts
                scriptData
                auxData
                validity
        in
            Cardano.Tx body' keyWits

--------------------------------------------------------------------------------
-- Module-internal helpers
--------------------------------------------------------------------------------

withStandardCryptoConstraint
    :: RecentEra era
    -> ((Crypto era ~ StandardCrypto) => a)
    -> a
withStandardCryptoConstraint era a = case era of
    RecentEraBabbage -> a
    RecentEraAlonzo -> a

withEraConversionConstraints
    :: RecentEra era
    -> ((ShelleyLedgerEra (CardanoApiEra era) ~ era) => a)
    -> a
withEraConversionConstraints era a = case era of
    RecentEraBabbage -> a
    RecentEraAlonzo -> a

withCLIConstraint
    :: RecentEra era
    -> (CLI era => a)
    -> a
withCLIConstraint era a = case era of
    RecentEraBabbage -> a
    RecentEraAlonzo -> a
