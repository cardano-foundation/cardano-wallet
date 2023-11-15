{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Internal.Cardano.Write.Tx
    (
    -- * Eras

    -- ** RecentEra
      RecentEra (..)
    , IsRecentEra (..)
    , toRecentEra
    , fromRecentEra
    , MaybeInRecentEra (..)
    , toRecentEraGADT
    , LatestLedgerEra
    , LatestEra
    , withConstraints
    , RecentEraConstraints
    , RecentEraLedgerConstraints

    -- ** Key witness counts
    , KeyWitnessCount (..)

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra
    , CardanoApi.ShelleyLedgerEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    , asCardanoApiTx
    , fromCardanoApiTx
    , toCardanoApiUTxO
    , fromCardanoApiUTxO
    , toCardanoApiValue
    , toCardanoApiLovelace
    , toCardanoApiTx

    -- ** Existential wrapper
    , AnyRecentEra (..)
    , InAnyRecentEra (..)
    , asAnyRecentEra
    , toAnyCardanoEra
    , fromAnyCardanoEra
    , withInAnyRecentEra
    , withRecentEra

    -- ** Misc
    , StandardCrypto
    , StandardBabbage
    , StandardConway

    -- * PParams
    , Core.PParams
    , FeePerByte (..)
    , getFeePerByte
    , feeOfBytes
    , maxScriptExecutionCost
    , stakeKeyDeposit
    , ProtVer (..)
    , Version

    -- * Tx
    , Core.Tx
    , Core.TxBody
    , txBody
    , outputs
    , modifyLedgerBody
    , emptyTx
    , serializeTx

    -- * TxId
    , Ledger.TxId

    -- * TxOut
    , Core.TxOut
    , BabbageTxOut (..)
    , TxOutInBabbage
    , TxOutInRecentEra (..)
    , unwrapTxOutInRecentEra
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

    -- *** Binary Data
    , BinaryData

    -- *** Datum Hash
    , DatumHash
    , datumHashFromBytes
    , datumHashToBytes

    -- ** Script
    , Script
    , Alonzo.isPlutusScript

    -- * TxIn
    , TxIn
    , unsafeMkTxIn

    -- * UTxO
    , Shelley.UTxO (..)
    , utxoFromTxOutsInRecentEra
    , utxoFromTxOuts

    -- * Policy and asset identifiers
    , PolicyId
    , AssetName

    -- * Balancing
    , evaluateMinimumFee
    , evaluateTransactionBalance
    )
    where

import Prelude

import Cardano.Crypto.Hash
    ( Hash (UnsafeHash)
    )
import Cardano.Ledger.Allegra.Scripts
    ( translateTimelock
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript (..)
    )
import Cardano.Ledger.Alonzo.Scripts.Data
    ( BinaryData
    , Datum (..)
    )
import Cardano.Ledger.Alonzo.TxInfo
    ( ExtendedUTxO
    )
import Cardano.Ledger.Alonzo.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Alonzo.UTxO
    ( AlonzoScriptsNeeded
    )
import Cardano.Ledger.Api.UTxO
    ( EraUTxO (ScriptsNeeded)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (..)
    , Version
    , maybeToStrictMaybe
    )
import Cardano.Ledger.Binary
    ( Sized (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Mary
    ( MaryValue
    )
import Cardano.Ledger.Mary.Value
    ( AssetName
    , PolicyID
    )
import Cardano.Ledger.SafeHash
    ( SafeHash
    , extractHash
    , unsafeMakeSafeHash
    )
import Cardano.Ledger.Val
    ( coin
    , modifyCoin
    )
import Control.Arrow
    ( second
    , (>>>)
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Short
    ( toShort
    )
import Data.Coerce
    ( coerce
    )
import Data.Foldable
    ( toList
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Kind
    ( Type
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Type.Equality
    ( (:~:) (Refl)
    , TestEquality (testEquality)
    )
import Data.Typeable
    ( Typeable
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage
    , StandardConway
    )

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Byron as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Core as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway.TxBody as Conway
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Core
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( txOutMaxCoin
    )
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

type LatestEra = CardanoApi.ConwayEra

type LatestLedgerEra = StandardConway

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
    RecentEraBabbage :: RecentEra CardanoApi.BabbageEra
    RecentEraConway :: RecentEra CardanoApi.ConwayEra

deriving instance Eq (RecentEra era)
deriving instance Show (RecentEra era)

instance TestEquality RecentEra where
    testEquality RecentEraBabbage RecentEraBabbage = Just Refl
    testEquality RecentEraConway RecentEraConway = Just Refl
    testEquality RecentEraBabbage RecentEraConway = Nothing
    testEquality RecentEraConway RecentEraBabbage = Nothing

class
    ( CardanoApi.IsShelleyBasedEra era
    , Typeable era
    ) => IsRecentEra era where
    recentEra :: RecentEra era

-- | Convenient constraints. Constraints may be dropped as we move to new eras.
--
-- Adding too many constraints shouldn't be a concern as the point of
-- 'RecentEra' is to work with a small closed set of eras, anyway.
type RecentEraLedgerConstraints era =
    ( Core.Era era
    , Core.EraTx era
    , Core.EraCrypto era ~ StandardCrypto
    , Core.Script era ~ AlonzoScript era
    , Core.Tx era ~ Babbage.AlonzoTx era
    , Core.Value era ~ Value
    , Core.TxWits era ~ AlonzoTxWits era
    , ExtendedUTxO era
    , Alonzo.AlonzoEraPParams era
    , Ledger.AlonzoEraTx era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    , Eq (TxOut era)
    , Ledger.Crypto (Core.EraCrypto era)
    , Show (TxOut era)
    , Show (Core.Tx era)
    , Eq (Core.Tx era)
    , Babbage.BabbageEraTxBody era
    , Shelley.EraUTxO era
    )

type RecentEraConstraints era =
    ( IsRecentEra era
    , RecentEraLedgerConstraints (CardanoApi.ShelleyLedgerEra era)
    )

-- | Bring useful constraints into scope from a value-level
-- 'RecentEra'.
--
-- TODO [ADP-2354] Make 'RecentEraLedgerConstraints' superclass of
-- 'IsRecentEra'. Currently this would cause weird type-inference errors in the
-- wallet code specifically with GHC 8.10.
-- https://cardanofoundation.atlassian.net/browse/ADP-2353
withConstraints
    :: RecentEra era -> (RecentEraConstraints era => a) -> a
withConstraints era a = case era of
    RecentEraBabbage -> a
    RecentEraConway -> a

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
toRecentEra :: CardanoApi.CardanoEra era -> Maybe (RecentEra era)
toRecentEra = \case
    CardanoApi.ConwayEra  -> Just RecentEraConway
    CardanoApi.BabbageEra -> Just RecentEraBabbage
    CardanoApi.AlonzoEra  -> Nothing
    CardanoApi.MaryEra    -> Nothing
    CardanoApi.AllegraEra -> Nothing
    CardanoApi.ShelleyEra -> Nothing
    CardanoApi.ByronEra   -> Nothing

fromRecentEra :: RecentEra era -> CardanoApi.CardanoEra era
fromRecentEra = \case
    RecentEraConway -> CardanoApi.ConwayEra
    RecentEraBabbage -> CardanoApi.BabbageEra

instance IsRecentEra CardanoApi.BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra CardanoApi.ConwayEra where
    recentEra = RecentEraConway

cardanoEraFromRecentEra :: RecentEra era -> CardanoApi.CardanoEra era
cardanoEraFromRecentEra =
    CardanoApi.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra :: RecentEra era -> CardanoApi.ShelleyBasedEra era
shelleyBasedEraFromRecentEra = \case
    RecentEraConway -> CardanoApi.ShelleyBasedEraConway
    RecentEraBabbage -> CardanoApi.ShelleyBasedEraBabbage

-- | For convenience working with 'IsRecentEra'.
--
-- Similar to 'CardanoApi.cardanoEra', but with an 'IsRecentEra era' constraint
-- instead of 'CardanoApi.IsCardanoEra'.
cardanoEra :: forall era. IsRecentEra era => CardanoApi.CardanoEra era
cardanoEra = cardanoEraFromRecentEra $ recentEra @era

-- | For convenience working with 'IsRecentEra'.
--
-- Similar to 'CardanoApi.shelleyBasedEra, but with a 'IsRecentEra era'
-- constraint instead of 'CardanoApi.IsShelleyBasedEra'.
shelleyBasedEra :: forall era. IsRecentEra era => CardanoApi.ShelleyBasedEra era
shelleyBasedEra = shelleyBasedEraFromRecentEra $ recentEra @era

data MaybeInRecentEra (thing :: Type -> Type)
    = InNonRecentEraByron
    | InNonRecentEraShelley
    | InNonRecentEraAllegra
    | InNonRecentEraMary
    | InNonRecentEraAlonzo
    | InRecentEraBabbage (thing CardanoApi.BabbageEra)
    | InRecentEraConway (thing CardanoApi.ConwayEra)

deriving instance
    ( Eq (a CardanoApi.BabbageEra)
    , Eq (a CardanoApi.ConwayEra)
    ) =>
    Eq (MaybeInRecentEra a)
deriving instance
    ( Show (a CardanoApi.BabbageEra)
    , Show (a CardanoApi.ConwayEra)
    ) =>
    Show (MaybeInRecentEra a)

toRecentEraGADT
    :: MaybeInRecentEra a
    -> Either CardanoApi.AnyCardanoEra (InAnyRecentEra a)
toRecentEraGADT = \case
    InNonRecentEraByron ->
        Left $ CardanoApi.AnyCardanoEra CardanoApi.ByronEra
    InNonRecentEraShelley ->
        Left $ CardanoApi.AnyCardanoEra CardanoApi.ShelleyEra
    InNonRecentEraAllegra ->
        Left $ CardanoApi.AnyCardanoEra CardanoApi.AllegraEra
    InNonRecentEraMary ->
        Left $ CardanoApi.AnyCardanoEra CardanoApi.MaryEra
    InNonRecentEraAlonzo ->
        Left $ CardanoApi.AnyCardanoEra CardanoApi.AlonzoEra
    InRecentEraBabbage a ->
        Right $ InAnyRecentEra recentEra a
    InRecentEraConway a ->
        Right $ InAnyRecentEra recentEra a

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

-- | "Downcast" something existentially wrapped in 'CardanoApi.InAnyCardanoEra'.
asAnyRecentEra
    :: CardanoApi.InAnyCardanoEra a
    -> Maybe (InAnyRecentEra a)
asAnyRecentEra = \case
    CardanoApi.InAnyCardanoEra CardanoApi.ByronEra _ ->
        Nothing
    CardanoApi.InAnyCardanoEra CardanoApi.ShelleyEra _ ->
        Nothing
    CardanoApi.InAnyCardanoEra CardanoApi.AllegraEra _ ->
        Nothing
    CardanoApi.InAnyCardanoEra CardanoApi.MaryEra _ ->
        Nothing
    CardanoApi.InAnyCardanoEra CardanoApi.AlonzoEra _ ->
        Nothing
    CardanoApi.InAnyCardanoEra CardanoApi.BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    CardanoApi.InAnyCardanoEra CardanoApi.ConwayEra a ->
        Just $ InAnyRecentEra RecentEraConway a

-- | An existential type like 'AnyCardanoEra', but for 'RecentEra'.
data AnyRecentEra where
    AnyRecentEra
        :: IsRecentEra era -- Provide class constraint
        => RecentEra era   -- and explicit value.
        -> AnyRecentEra    -- and that's it.

instance Enum AnyRecentEra where
    -- NOTE: We're not starting at 0! 0 would be Byron, which is not a recent
    -- era.
    fromEnum = fromEnum . toAnyCardanoEra
    toEnum n = fromMaybe err . fromAnyCardanoEra $ toEnum n
      where
        err = error $ unwords
            [ "AnyRecentEra.toEnum:", show n
            , "doesn't correspond to a recent era."
            ]

instance Bounded AnyRecentEra where
    minBound = AnyRecentEra RecentEraBabbage
    maxBound = AnyRecentEra RecentEraConway

instance Show AnyRecentEra where
    show (AnyRecentEra era) = "AnyRecentEra " <> show era

instance Eq AnyRecentEra where
    AnyRecentEra e1 == AnyRecentEra e2 =
        isJust $ testEquality e1 e2

toAnyCardanoEra :: AnyRecentEra -> CardanoApi.AnyCardanoEra
toAnyCardanoEra (AnyRecentEra era) =
    CardanoApi.AnyCardanoEra (fromRecentEra era)

fromAnyCardanoEra
    :: CardanoApi.AnyCardanoEra
    -> Maybe AnyRecentEra
fromAnyCardanoEra = \case
    CardanoApi.AnyCardanoEra CardanoApi.ByronEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.ShelleyEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.AllegraEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.MaryEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.AlonzoEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.BabbageEra ->
        Just $ AnyRecentEra RecentEraBabbage
    CardanoApi.AnyCardanoEra CardanoApi.ConwayEra ->
        Just $ AnyRecentEra RecentEraConway

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
    -> (Value -> Value)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
modifyTxOutValue RecentEraConway f (BabbageTxOut addr val dat script) =
    BabbageTxOut addr (f val) dat script
modifyTxOutValue RecentEraBabbage f (BabbageTxOut addr val dat script) =
    BabbageTxOut addr (f val) dat script

modifyTxOutCoin
    :: RecentEra era
    -> (Coin -> Coin)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
modifyTxOutCoin era = modifyTxOutValue era . modifyCoin

txOutValue :: RecentEra era -> TxOut (CardanoApi.ShelleyLedgerEra era) -> Value
txOutValue RecentEraConway (Babbage.BabbageTxOut _ val _ _) = val
txOutValue RecentEraBabbage (Babbage.BabbageTxOut _ val _ _) = val

type TxOutInBabbage = Babbage.BabbageTxOut (Babbage.BabbageEra StandardCrypto)

type Address = Ledger.Addr StandardCrypto

type Script = AlonzoScript
type Value = MaryValue StandardCrypto

unsafeAddressFromBytes :: ByteString -> Address
unsafeAddressFromBytes bytes = case Ledger.deserialiseAddr bytes of
    Just addr -> addr
    Nothing -> error "unsafeAddressFromBytes: failed to deserialise"

type DatumHash = Alonzo.DataHash StandardCrypto

datumHashFromBytes :: ByteString -> Maybe DatumHash
datumHashFromBytes = fmap unsafeMakeSafeHash <$> Crypto.hashFromBytes

datumHashToBytes :: SafeHash crypto a -> ByteString
datumHashToBytes = Crypto.hashToBytes . extractHash

-- | Type representing a TxOut in the latest or previous era.
--
-- The underlying representation is isomorphic to 'TxOut LatestLedgerEra'.
--
-- Can be unwrapped using 'unwrapTxOutInRecentEra' or
-- 'utxoFromTxOutsInRecentEra'.
--
-- Implementation assumes @TxOut latestEra ⊇ TxOut prevEra@ in the sense that
-- the latest era has not removed information from the @TxOut@. This allows
-- e.g. @ToJSON@ / @FromJSON@ instances to be written for two eras using only
-- one implementation.
data TxOutInRecentEra =
    TxOutInRecentEra
        Address
        Value
        (Datum LatestLedgerEra)
        (Maybe (AlonzoScript LatestLedgerEra))
        -- Same contents as 'TxOut LatestLedgerEra'.

unwrapTxOutInRecentEra
    :: RecentEra era
    -> TxOutInRecentEra
    -> (TxOut (CardanoApi.ShelleyLedgerEra era))
unwrapTxOutInRecentEra era recentEraTxOut = case era of
    RecentEraConway -> recentEraToConwayTxOut recentEraTxOut
    RecentEraBabbage -> recentEraToBabbageTxOut recentEraTxOut

recentEraToConwayTxOut
    :: TxOutInRecentEra
    -> Babbage.BabbageTxOut LatestLedgerEra
recentEraToConwayTxOut (TxOutInRecentEra addr val datum mscript) =
    Babbage.BabbageTxOut addr val datum (maybeToStrictMaybe mscript)

recentEraToBabbageTxOut
    :: TxOutInRecentEra
    -> Babbage.BabbageTxOut (Babbage.BabbageEra StandardCrypto)
recentEraToBabbageTxOut (TxOutInRecentEra addr val datum mscript) =
    Babbage.BabbageTxOut addr val
        (castDatum datum)
        (maybeToStrictMaybe (castScript <$> mscript))
  where
    castDatum = \case
        Alonzo.NoDatum ->
            Alonzo.NoDatum
        Alonzo.DatumHash h ->
            Alonzo.DatumHash h
        Alonzo.Datum binaryData ->
            Alonzo.Datum (coerce binaryData)
    castScript :: AlonzoScript StandardConway -> AlonzoScript StandardBabbage
    castScript = \case
        Alonzo.TimelockScript timelockEra ->
            Alonzo.TimelockScript (translateTimelock timelockEra)
        Alonzo.PlutusScript l bs ->
            Alonzo.PlutusScript l bs

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
    -> Core.PParams (CardanoApi.ShelleyLedgerEra era)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
    -> Coin
computeMinimumCoinForTxOut era pp out = withConstraints era $
    Core.getMinCoinTxOut pp (withMaxLengthSerializedCoin out)
  where
    withMaxLengthSerializedCoin
        :: TxOut (CardanoApi.ShelleyLedgerEra era)
        -> TxOut (CardanoApi.ShelleyLedgerEra era)
    withMaxLengthSerializedCoin =
        modifyTxOutCoin era (const $ Convert.toLedger W.txOutMaxCoin)

isBelowMinimumCoinForTxOut
    :: forall era. RecentEra era
    -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams (CardanoApi.ShelleyLedgerEra era)
    -> TxOut (CardanoApi.ShelleyLedgerEra era)
    -> Bool
isBelowMinimumCoinForTxOut era pp out =
    actualCoin < requiredMin
  where
    -- IMPORTANT to use the exact minimum from the ledger function, and not our
    -- overestimating 'computeMinimumCoinForTxOut'.
    requiredMin = withConstraints era $ Core.getMinCoinTxOut pp out
    actualCoin = getCoin era out

    getCoin :: RecentEra era -> TxOut (CardanoApi.ShelleyLedgerEra era) -> Coin
    getCoin RecentEraConway (Babbage.BabbageTxOut _ val _ _) = coin val
    getCoin RecentEraBabbage (Babbage.BabbageTxOut _ val _ _) = coin val

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Construct a 'UTxO era' using 'TxIn's and 'TxOut's in said era.
utxoFromTxOuts
    :: RecentEra era
    -> [(TxIn, Core.TxOut (CardanoApi.ShelleyLedgerEra era))]
    -> (Shelley.UTxO (CardanoApi.ShelleyLedgerEra era))
utxoFromTxOuts era = withConstraints era $
    Shelley.UTxO . Map.fromList

-- | Construct a 'UTxO era' using 'TxOutInRecentEra'.
--
-- Used to have a possibility for failure when we supported Alonzo and Babbage,
-- and could possibly become failable again with future eras.
utxoFromTxOutsInRecentEra
    :: forall era. RecentEra era
    -> [(TxIn, TxOutInRecentEra)]
    -> Shelley.UTxO (CardanoApi.ShelleyLedgerEra era)
utxoFromTxOutsInRecentEra era = withConstraints era $
    Shelley.UTxO . Map.fromList . map (second (unwrapTxOutInRecentEra era))

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

serializeTx
    :: forall era. IsRecentEra era
    => Core.Tx (CardanoApi.ShelleyLedgerEra era)
    -> ByteString
serializeTx tx = CardanoApi.serialiseToCBOR $ toCardanoApiTx @era tx

txBody
    :: RecentEra era
    -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
    -> Core.TxBody (CardanoApi.ShelleyLedgerEra era)
txBody era = case era of
    RecentEraBabbage -> Babbage.body -- same type for babbage
    RecentEraConway -> Babbage.body -- same type for conway

-- Until we have convenient lenses to use
outputs
    :: RecentEra era
    -> Core.TxBody (CardanoApi.ShelleyLedgerEra era)
    -> [TxOut (CardanoApi.ShelleyLedgerEra era)]
outputs RecentEraConway = map sizedValue . toList . Conway.ctbOutputs
outputs RecentEraBabbage = map sizedValue . toList . Babbage.btbOutputs

-- NOTE: To reduce the need for the caller to deal with
-- @CardanoApiEra (CardanoApi.ShelleyLedgerEra era) ~ era@, we quantify this
-- function over @cardanoEra@ instead of @era@.
--
-- TODO [ADP-2353] Move to @cardano-api@ related module
modifyLedgerBody
    :: forall cardanoEra. IsRecentEra cardanoEra
    => (Core.TxBody (CardanoApi.ShelleyLedgerEra cardanoEra) ->
        Core.TxBody (CardanoApi.ShelleyLedgerEra cardanoEra))
    -> Core.Tx (CardanoApi.ShelleyLedgerEra cardanoEra)
    -> Core.Tx (CardanoApi.ShelleyLedgerEra cardanoEra)
modifyLedgerBody f = asCardanoApiTx @cardanoEra modify
  where
    modify (CardanoApi.Tx body keyWits) = CardanoApi.Tx body' keyWits
      where
        body' = case body of
            CardanoApi.ByronTxBody {} ->
                error "Impossible: ByronTxBody in CardanoApi.ShelleyLedgerEra"
            CardanoApi.ShelleyTxBody
                shelleyEra
                ledgerBody
                scripts
                scriptData
                auxData
                validity ->
                    CardanoApi.ShelleyTxBody
                        shelleyEra
                        (f ledgerBody)
                        scripts
                        scriptData
                        auxData
                        validity

emptyTx :: RecentEra era -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
emptyTx era = withConstraints era $ Core.mkBasicTx Core.mkBasicTxBody

--------------------------------------------------------------------------------
-- Compatibility
--------------------------------------------------------------------------------

asCardanoApiTx
    :: forall era. IsRecentEra era
    => (CardanoApi.Tx era -> CardanoApi.Tx era)
    -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
    -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
asCardanoApiTx f
    = fromCardanoApiTx
    . f
    . toCardanoApiTx

fromCardanoApiTx
    :: forall era. IsRecentEra era
    => CardanoApi.Tx era
    -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
fromCardanoApiTx = \case
    CardanoApi.ShelleyTx _era tx ->
        tx
    CardanoApi.ByronTx {} ->
        case (recentEra @era) of
            {}

toCardanoApiTx
    :: forall era. IsRecentEra era
    => Core.Tx (CardanoApi.ShelleyLedgerEra era)
    -> CardanoApi.Tx era
toCardanoApiTx = CardanoApi.ShelleyTx
    (shelleyBasedEraFromRecentEra $ recentEra @era)

toCardanoApiUTxO
    :: forall era. IsRecentEra era
    => Shelley.UTxO (CardanoApi.ShelleyLedgerEra era)
    -> CardanoApi.UTxO era
toCardanoApiUTxO = withConstraints (recentEra @era) $
    CardanoApi.UTxO
    . Map.mapKeys CardanoApi.fromShelleyTxIn
    . Map.map (CardanoApi.fromShelleyTxOut (shelleyBasedEra @era))
    . unUTxO
  where
    unUTxO (Shelley.UTxO m) = m

fromCardanoApiUTxO
    :: forall era. IsRecentEra era
    => CardanoApi.UTxO era
    -> Shelley.UTxO (CardanoApi.ShelleyLedgerEra era)
fromCardanoApiUTxO = withConstraints (recentEra @era) $
    Shelley.UTxO
    . Map.mapKeys CardanoApi.toShelleyTxIn
    . Map.map (CardanoApi.toShelleyTxOut (shelleyBasedEra @era))
    . CardanoApi.unUTxO

toCardanoApiValue
    :: forall era. IsRecentEra era
    => Core.Value (CardanoApi.ShelleyLedgerEra era)
    -> CardanoApi.Value
toCardanoApiValue = withConstraints (recentEra @era) CardanoApi.fromMaryValue

toCardanoApiLovelace :: Coin -> CardanoApi.Lovelace
toCardanoApiLovelace = CardanoApi.fromShelleyLovelace

--------------------------------------------------------------------------------
-- PParams
--------------------------------------------------------------------------------

-- | The 'minfeeA' protocol parameter in unit @lovelace/byte@.
newtype FeePerByte = FeePerByte Natural
    deriving (Show, Eq)

getFeePerByte
    :: HasCallStack
    => RecentEra era
    -> Core.PParams (CardanoApi.ShelleyLedgerEra era)
    -> FeePerByte
getFeePerByte era pp =
    unsafeCoinToFee $
        case era of
            RecentEraConway -> pp ^. Core.ppMinFeeAL
            RecentEraBabbage -> pp ^. Core.ppMinFeeAL
  where
    unsafeCoinToFee :: Coin -> FeePerByte
    unsafeCoinToFee = unCoin >>> intCastMaybe >>> \case
        Just fee -> FeePerByte fee
        Nothing -> error "Impossible: min fee protocol parameter is negative"

feeOfBytes :: FeePerByte -> Natural -> Coin
feeOfBytes (FeePerByte perByte) bytes = Coin $ intCast $ perByte * bytes

type ExUnitPrices = Alonzo.Prices

type ExUnits = Alonzo.ExUnits

txscriptfee :: ExUnitPrices -> ExUnits -> Coin
txscriptfee = Alonzo.txscriptfee

maxScriptExecutionCost
    :: RecentEra era -> Core.PParams (CardanoApi.ShelleyLedgerEra era) -> Coin
maxScriptExecutionCost era pp = withConstraints era $
    txscriptfee (pp ^. Alonzo.ppPricesL) (pp ^. Alonzo.ppMaxTxExUnitsL)

stakeKeyDeposit
    :: RecentEra era -> Core.PParams (CardanoApi.ShelleyLedgerEra era) -> Coin
stakeKeyDeposit era pp = withConstraints era $ pp ^. Core.ppKeyDepositL

--------------------------------------------------------------------------------
-- Balancing
--------------------------------------------------------------------------------

-- | Computes the minimal fee amount necessary to pay for a given transaction.
--
evaluateMinimumFee
    :: RecentEra era
    -> Core.PParams (CardanoApi.ShelleyLedgerEra era)
    -> Core.Tx (CardanoApi.ShelleyLedgerEra era)
    -> KeyWitnessCount
    -> Coin
evaluateMinimumFee era pp tx kwc =
    mainFee <> bootWitnessFee
  where
    KeyWitnessCount {nKeyWits, nBootstrapWits} = kwc

    mainFee :: Coin
    mainFee = withConstraints era $
        Shelley.evaluateTransactionFee pp tx nKeyWits

    FeePerByte feePerByte = getFeePerByte era pp

    bootWitnessFee :: Coin
    bootWitnessFee = Coin $ intCast $ feePerByte * byteCount
      where
        byteCount :: Natural
        byteCount = sizeOf_BootstrapWitnesses $ intCast nBootstrapWits

        -- Matching implementation in "Cardano.Wallet.Shelley.Transaction".
        -- Equivalence is tested in property.
        sizeOf_BootstrapWitnesses :: Natural -> Natural
        sizeOf_BootstrapWitnesses 0 = 0
        sizeOf_BootstrapWitnesses n = 4 + 180 * n

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
    :: forall era
     . RecentEra era
    -> Core.PParams (CardanoApi.ShelleyLedgerEra era)
    -> Shelley.UTxO (CardanoApi.ShelleyLedgerEra era)
    -> Core.TxBody (CardanoApi.ShelleyLedgerEra era)
    -> Core.Value (CardanoApi.ShelleyLedgerEra era)
evaluateTransactionBalance era pp utxo = withConstraints era $
    let -- Looks up the current deposit amount for a registered stake credential
        -- delegation.
        --
        -- This function must produce a valid answer for all stake credentials
        -- present in any of the 'DeRegKey' delegation certificates in the
        -- supplied 'TxBody'. In other words, there is no requirement to know
        -- about all of the delegation certificates in the ledger state,
        -- just those this transaction cares about.
        lookupRefund :: Core.StakeCredential StandardCrypto -> Maybe Coin
        lookupRefund _stakeCred = Just $ pp ^. Core.ppKeyDepositL

        -- Checks whether a pool with a supplied 'PoolStakeId' is already
        -- registered.
        --
        -- There is no requirement to answer this question for all stake pool
        -- credentials, just those that have their registration certificates
        -- included in the supplied 'TxBody'.
        isRegPoolId :: Ledger.KeyHash 'Ledger.StakePool StandardCrypto -> Bool
        isRegPoolId _keyHash = True

    in Ledger.evalBalanceTxBody pp lookupRefund isRegPoolId utxo

--------------------------------------------------------------------------------
-- Policy and asset identifiers
--------------------------------------------------------------------------------

type PolicyId = PolicyID StandardCrypto
