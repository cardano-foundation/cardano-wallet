{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
    , fromRecentEra
    , LatestLedgerEra
    , LatestEra

    -- ** Key witness counts
    , KeyWitnessCount (..)

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra
    , ShelleyLedgerEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra

    -- ** Existential wrapper
    , AnyRecentEra (..)
    , InAnyRecentEra (..)
    , asAnyRecentEra
    , fromAnyRecentEra
    , withInAnyRecentEra
    , withRecentEra

    -- ** Misc
    , StandardCrypto
    , StandardBabbage
    , StandardAlonzo

    -- * PParams
    , Core.PParams

    -- * Tx
    , Core.Tx
    , Core.TxBody
    , txBody
    , outputs
    , modifyTxOutputs
    , modifyLedgerBody

    -- * TxOut
    , Core.TxOut
    , TxOutInBabbage
    , TxOutInRecentEra (..)
    , unwrapTxOutInRecentEra
    , ErrInvalidTxOutInEra (..)
    , modifyTxOutValue
    , modifyTxOutCoin
    , txOutValue

    , computeMinimumCoinForTxOut
    , isBelowMinimumCoinForTxOut

    -- ** Address
    , Address
    , unsafeAddressFromBytes

    -- ** Value
    , Value
    , modifyCoin
    , coin
    , Coin (..)

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
    , Script
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
    , fromCardanoTx
    , toCardanoUTxO
    , fromCardanoUTxO
    , toCardanoValue

    -- * Balancing
    , evaluateMinimumFee
    , evaluateTransactionBalance
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
    ( AlonzoScript (..) )
import Cardano.Ledger.Coin
    ( Coin (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Crypto )
import Cardano.Ledger.Mary
    ( MaryValue )
import Cardano.Ledger.SafeHash
    ( SafeHash, extractHash, unsafeMakeSafeHash )
import Cardano.Ledger.Serialization
    ( Sized (..), mkSized )
import Cardano.Ledger.Shelley.API
    ( CLI (evaluateMinLovelaceOutput) )
import Cardano.Ledger.Val
    ( coin, modifyCoin )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toLedger )
import Control.Arrow
    ( second )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( toShort )
import Data.Foldable
    ( toList )
import Data.Maybe
    ( fromMaybe )
import Data.Maybe.Strict
    ( StrictMaybe (..) )
import Data.Typeable
    ( Typeable )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )
import Test.Cardano.Ledger.Alonzo.Examples.Consensus
    ( StandardAlonzo )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
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
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

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

deriving instance Eq (RecentEra era)
deriving instance Show (RecentEra era)

class (Cardano.IsShelleyBasedEra era, Typeable era) => IsRecentEra era where
    recentEra :: RecentEra era

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
toRecentEra :: Cardano.CardanoEra era -> Maybe (RecentEra era)
toRecentEra = \case
    Cardano.BabbageEra -> Just RecentEraBabbage
    Cardano.AlonzoEra  -> Just RecentEraAlonzo
    Cardano.MaryEra    -> Nothing
    Cardano.AllegraEra -> Nothing
    Cardano.ShelleyEra -> Nothing
    Cardano.ByronEra   -> Nothing

fromRecentEra :: RecentEra era -> Cardano.CardanoEra era
fromRecentEra = \case
  RecentEraBabbage -> Cardano.BabbageEra
  RecentEraAlonzo -> Cardano.AlonzoEra

instance IsRecentEra BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra AlonzoEra where
    recentEra = RecentEraAlonzo

cardanoEraFromRecentEra :: RecentEra era -> Cardano.CardanoEra era
cardanoEraFromRecentEra =
    Cardano.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra :: RecentEra era -> Cardano.ShelleyBasedEra era
shelleyBasedEraFromRecentEra = \case
    RecentEraBabbage -> Cardano.ShelleyBasedEraBabbage
    RecentEraAlonzo -> Cardano.ShelleyBasedEraAlonzo

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
     InAnyRecentEra
         :: IsRecentEra era -- Provide class constraint
         => RecentEra era   -- and explicit value.
         -> thing era
         -> InAnyRecentEra thing

withInAnyRecentEra
    :: InAnyRecentEra thing
    -> (forall era. IsRecentEra era => thing era -> a)
    -> a
withInAnyRecentEra (InAnyRecentEra _era tx) f = f tx

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

-- | An existential type like 'AnyCardanoEra', but for 'RecentEra'.
data AnyRecentEra where
     AnyRecentEra :: IsRecentEra era -- Provide class constraint
                  => RecentEra era   -- and explicit value.
                  -> AnyRecentEra    -- and that's it.

instance Show AnyRecentEra where
    show (AnyRecentEra era) = "AnyRecentEra " <> show era

fromAnyRecentEra :: AnyRecentEra -> Cardano.AnyCardanoEra
fromAnyRecentEra (AnyRecentEra era) = Cardano.AnyCardanoEra (fromRecentEra era)

withRecentEra ::
    AnyRecentEra -> (forall era. IsRecentEra era => RecentEra era -> a) -> a
withRecentEra (AnyRecentEra era) f = f era

--------------------------------------------------------------------------------
-- Key witness counts
--------------------------------------------------------------------------------

data KeyWitnessCount = KeyWitnessCount
    { nKeyWits :: !Word
    -- ^ "Normal" verification key witnesses introduced with the Shelley era.

    , nBootstrapWits :: !Word
    -- ^ Bootstrap key witnesses, a.k.a Byron witnesses.
    } deriving (Eq, Show)

instance Semigroup KeyWitnessCount where
    (KeyWitnessCount s1 b1) <> (KeyWitnessCount s2 b2)
        = KeyWitnessCount (s1 + s2) (b1 + b2)

instance Monoid KeyWitnessCount where
    mempty = KeyWitnessCount 0 0

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
    -> (MaryValue StandardCrypto -> MaryValue StandardCrypto)
    -> TxOut (ShelleyLedgerEra era)
    -> TxOut (ShelleyLedgerEra era)
modifyTxOutValue RecentEraBabbage f (Babbage.BabbageTxOut addr val dat script) =
    withStandardCryptoConstraint RecentEraBabbage $
        Babbage.BabbageTxOut addr (f val) dat script
modifyTxOutValue RecentEraAlonzo f (Alonzo.AlonzoTxOut addr val dat) =
    withStandardCryptoConstraint RecentEraAlonzo $
        Alonzo.AlonzoTxOut addr (f val) dat

modifyTxOutCoin
    :: RecentEra era
    -> (Coin -> Coin)
    -> TxOut (ShelleyLedgerEra era)
    -> TxOut (ShelleyLedgerEra era)
modifyTxOutCoin era f = withStandardCryptoConstraint era $
    modifyTxOutValue era (modifyCoin f)

txOutValue
    :: RecentEra era
    -> TxOut (ShelleyLedgerEra era)
    -> MaryValue StandardCrypto
txOutValue RecentEraBabbage (Babbage.BabbageTxOut _ val _ _) = val
txOutValue RecentEraAlonzo (Alonzo.AlonzoTxOut _ val _) = val

type TxOutInBabbage = Babbage.BabbageTxOut (Babbage.BabbageEra StandardCrypto)

type Address = Ledger.Addr StandardCrypto

type Script = AlonzoScript
type Value = MaryValue

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
scriptToCardanoEnvelopeJSON :: AlonzoScript LatestLedgerEra -> Aeson.Value
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
    -> Aeson.Parser (AlonzoScript LatestLedgerEra)
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
        (MaryValue StandardCrypto)
        (Datum LatestLedgerEra)
        (Maybe (AlonzoScript LatestLedgerEra))
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

castTxOut
    :: TxOutInRecentEra
    -> TxOut (ShelleyLedgerEra BabbageEra)
castTxOut (TxOutInRecentEra addr val datum mscript) =
    (Babbage.BabbageTxOut addr val datum (toStrict mscript))
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
    = Right $ Alonzo.AlonzoTxOut addr val SNothing
downcastTxOut (TxOutInRecentEra addr val (Alonzo.DatumHash dh) Nothing)
    = Right $ Alonzo.AlonzoTxOut addr val (SJust dh)

--
-- MinimumUTxO
--

-- | Compute the minimum ada quantity required for a given 'TxOut'.
--
-- Unlike @Ledger.evaluateMinLovelaceOutput@, this function may return an
-- overestimation for the sake of satisfying the property:
--
-- @
--     forall out.
--     let
--         c = computeMinimumCoinForUTxO out
--     in
--         forall c' >= c.
--         not $ isBelowMinimumCoinForTxOut modifyTxOutCoin (const c') out
-- @
--
-- This makes it easy for callers to create outputs with near-minimum ada
-- quantities regardless of the fact that modifying the ada 'Coin' value may
-- itself change the size and min-ada requirement.
computeMinimumCoinForTxOut
    :: forall era. RecentEra era
    -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams (ShelleyLedgerEra era)
    -> TxOut (ShelleyLedgerEra era)
    -> Coin
computeMinimumCoinForTxOut era pp out = withCLIConstraint era $
    evaluateMinLovelaceOutput pp (withMaxLengthSerializedCoin out)
  where
    withMaxLengthSerializedCoin
        :: TxOut (ShelleyLedgerEra era)
        -> TxOut (ShelleyLedgerEra era)
    withMaxLengthSerializedCoin =
        withStandardCryptoConstraint era $
            modifyTxOutCoin era (const $ toLedger txOutMaxCoin)

isBelowMinimumCoinForTxOut
    :: forall era. RecentEra era
    -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams (ShelleyLedgerEra era)
    -> TxOut (ShelleyLedgerEra era)
    -> Bool
isBelowMinimumCoinForTxOut era pp out =
    actualCoin < requiredMin
  where
    -- IMPORTANT to use the exact minimum from the ledger function, and not our
    -- overestimating 'computeMinimumCoinForTxOut'.
    requiredMin = withCLIConstraint era $ evaluateMinLovelaceOutput pp out
    actualCoin = getCoin era out

    getCoin :: RecentEra era -> TxOut (ShelleyLedgerEra era) -> Coin
    getCoin RecentEraBabbage (Babbage.BabbageTxOut _ val _ _) = coin val
    getCoin RecentEraAlonzo (Alonzo.AlonzoTxOut _ val _) = coin val

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
    -> (TxOut (ShelleyLedgerEra era) -> TxOut (ShelleyLedgerEra era))
    -> Core.TxBody (ShelleyLedgerEra era)
    -> Core.TxBody (ShelleyLedgerEra era)
modifyTxOutputs era f body = case era of
    RecentEraBabbage -> body
        { Babbage.outputs = mapSized f <$> Babbage.outputs body
        }
    RecentEraAlonzo -> body
        { Alonzo.outputs = f <$> Alonzo.outputs body
        }
  where
    mapSized f' = mkSized . f' . sizedValue

txBody
    :: RecentEra era
    -> Core.Tx (ShelleyLedgerEra era)
    -> Core.TxBody (ShelleyLedgerEra era)
txBody RecentEraBabbage = Alonzo.body -- same type for babbage
txBody RecentEraAlonzo = Alonzo.body

-- Until we have convenient lenses to use
outputs
    :: RecentEra era
    -> Core.TxBody (ShelleyLedgerEra era)
    -> [TxOut (ShelleyLedgerEra era)]
outputs RecentEraBabbage = map sizedValue . toList . Babbage.outputs
outputs RecentEraAlonzo = toList . Alonzo.outputs

-- NOTE: To reduce the need for the caller to deal with @CardanoApiEra
-- (ShelleyLedgerEra era) ~ era@, we quantify this function over @cardanoEra@
-- instead of @era@.
--
-- TODO [ADP-2353] Move to @cardano-api@ related module
modifyLedgerBody
    :: (Core.TxBody (ShelleyLedgerEra cardanoEra)
        -> Core.TxBody (ShelleyLedgerEra cardanoEra))
    -> Cardano.Tx cardanoEra
    -> Cardano.Tx cardanoEra
modifyLedgerBody f (Cardano.Tx body keyWits) = Cardano.Tx body' keyWits
  where
    body' =
        case body of
            Cardano.ByronTxBody {} ->
                error "Impossible: ByronTxBody in ShelleyLedgerEra"
            Cardano.ShelleyTxBody
                shelleyEra
                ledgerBody
                scripts
                scriptData
                auxData
                validity ->
                    Cardano.ShelleyTxBody
                        shelleyEra
                        (f ledgerBody)
                        scripts
                        scriptData
                        auxData
                        validity

--------------------------------------------------------------------------------
-- Compatibility
--------------------------------------------------------------------------------

fromCardanoTx
    :: forall era. IsRecentEra era
    => Cardano.Tx era
    -> Core.Tx (Cardano.ShelleyLedgerEra era)
fromCardanoTx = \case
    Cardano.ShelleyTx _era tx ->
        tx
    Cardano.ByronTx {} ->
        case (recentEra @era) of
            {}

-- | NOTE: The roundtrip
-- @
--     toCardanoUTxO . fromCardanoUTxO
-- @
-- will mark any 'SimpleScriptV1' reference scripts as 'SimpleScriptV2'. Because
-- 'SimpleScriptV1' is 'ShelleyEra'-specific, and 'ShelleyEra' is not a
-- 'RecentEra', this should not be a problem.
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

toCardanoValue
    :: forall era. IsRecentEra era
    => Core.Value (ShelleyLedgerEra era)
    -> Cardano.Value
toCardanoValue = case recentEra @era of
    RecentEraBabbage -> Cardano.fromMaryValue
    RecentEraAlonzo -> Cardano.fromMaryValue

--------------------------------------------------------------------------------
-- Balancing
--------------------------------------------------------------------------------

-- | Computes the minimal fee amount necessary to pay for a given transaction.
--
evaluateMinimumFee
    :: RecentEra era
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
    -> Core.Tx (Cardano.ShelleyLedgerEra era)
    -> KeyWitnessCount
    -> Coin
evaluateMinimumFee era pp tx kwc =
    mainFee <> bootWitnessFee
  where
    KeyWitnessCount {nKeyWits, nBootstrapWits} = kwc

    mainFee :: Coin
    mainFee = withCLIConstraint era $
        Shelley.evaluateTransactionFee pp tx nKeyWits

    bootWitnessFee :: Coin
    bootWitnessFee =
        if nBootstrapWits > 0
        then error "evaluateMinimumFee: bootstrap witnesses not yet supported"
        else mempty

-- | Evaluate the /balance/ of a transaction using the ledger.
--
-- The balance is defined as:
-- @
-- (value consumed by transaction) - (value produced by transaction)
-- @
--
-- For a transaction to be valid, it must have a balance of __zero__.
--
-- Note that the fee field of the transaction affects the balance, and
-- is not automatically the minimum fee.
--
evaluateTransactionBalance
    :: RecentEra era
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
    -> Shelley.UTxO (Cardano.ShelleyLedgerEra era)
    -> Core.TxBody (Cardano.ShelleyLedgerEra era)
    -> Core.Value (Cardano.ShelleyLedgerEra era)
evaluateTransactionBalance era pp utxo txBody' =
    withCLIConstraint era $
        Shelley.evaluateTransactionBalance pp utxo isNewPool txBody'
  where
    isNewPool =
        -- TODO: ADP-2651
        -- Pass this parameter in as a function instead of hard-coding the
        -- value here:
        const True

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

withCLIConstraint
    :: RecentEra era
    -> (CLI (ShelleyLedgerEra era) => a)
    -> a
withCLIConstraint era a = case era of
    RecentEraBabbage -> a
    RecentEraAlonzo -> a
