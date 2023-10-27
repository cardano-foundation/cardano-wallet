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
    , RecentEraLedgerConstraints

    -- ** Key witness counts
    , KeyWitnessCount (..)

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra
    , Cardano.ShelleyLedgerEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    , fromCardanoTx
    , toCardanoUTxO
    , fromCardanoUTxO
    , toCardanoValue
    , toCardanoLovelace
    , toCardanoTx

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
import Cardano.Ledger.SafeHash
    ( SafeHash
    , extractHash
    , unsafeMakeSafeHash
    )
import Cardano.Ledger.Val
    ( coin
    , modifyCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin
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

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Cardano
import qualified Cardano.Api.Shelley as Cardano
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
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

type LatestEra = Cardano.ConwayEra

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
    RecentEraBabbage :: RecentEra Cardano.BabbageEra
    RecentEraConway :: RecentEra Cardano.ConwayEra

deriving instance Eq (RecentEra era)
deriving instance Show (RecentEra era)

instance TestEquality RecentEra where
    testEquality RecentEraBabbage RecentEraBabbage = Just Refl
    testEquality RecentEraConway RecentEraConway = Just Refl
    testEquality RecentEraBabbage RecentEraConway = Nothing
    testEquality RecentEraConway RecentEraBabbage = Nothing

class
    ( Cardano.IsShelleyBasedEra era
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
    , Core.Value era ~ MaryValue StandardCrypto
    , Core.TxWits era ~ AlonzoTxWits era
    , ExtendedUTxO era
    , Alonzo.AlonzoEraPParams era
    , Ledger.AlonzoEraTx era
    , Alonzo.AlonzoEraTxBody era
    , Babbage.ShelleyEraTxBody era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    , Shelley.EraUTxO era
    )

-- | Bring useful constraints into scope from a value-level
-- 'RecentEra'.
--
-- TODO [ADP-2354] Make 'RecentEraLedgerConstraints' superclass of
-- 'IsRecentEra'. Currently this would cause weird type-inference errors in the
-- wallet code specifically with GHC 8.10.
-- https://cardanofoundation.atlassian.net/browse/ADP-2353
withConstraints
    :: RecentEra era
    -> ((RecentEraLedgerConstraints (Cardano.ShelleyLedgerEra era)) => a)
    -> a
withConstraints era a = case era of
    RecentEraBabbage -> a
    RecentEraConway -> a

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
toRecentEra :: Cardano.CardanoEra era -> Maybe (RecentEra era)
toRecentEra = \case
    Cardano.ConwayEra  -> Just RecentEraConway
    Cardano.BabbageEra -> Just RecentEraBabbage
    Cardano.AlonzoEra  -> Nothing
    Cardano.MaryEra    -> Nothing
    Cardano.AllegraEra -> Nothing
    Cardano.ShelleyEra -> Nothing
    Cardano.ByronEra   -> Nothing

fromRecentEra :: RecentEra era -> Cardano.CardanoEra era
fromRecentEra = \case
  RecentEraConway -> Cardano.ConwayEra
  RecentEraBabbage -> Cardano.BabbageEra

instance IsRecentEra Cardano.BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra Cardano.ConwayEra where
    recentEra = RecentEraConway

cardanoEraFromRecentEra :: RecentEra era -> Cardano.CardanoEra era
cardanoEraFromRecentEra =
    Cardano.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra :: RecentEra era -> Cardano.ShelleyBasedEra era
shelleyBasedEraFromRecentEra = \case
    RecentEraConway -> Cardano.ShelleyBasedEraConway
    RecentEraBabbage -> Cardano.ShelleyBasedEraBabbage

-- | For convenience working with 'IsRecentEra'. Similar to 'Cardano.cardanoEra,
-- but with a 'IsRecentEra era' constraint instead of 'Cardano.IsCardanoEra.
cardanoEra :: forall era. IsRecentEra era => Cardano.CardanoEra era
cardanoEra = cardanoEraFromRecentEra $ recentEra @era

-- | For convenience working with 'IsRecentEra'. Similar to
-- 'Cardano.shelleyBasedEra, but with a 'IsRecentEra era' constraint instead of
-- 'Cardano.IsShelleyBasedEra'.
shelleyBasedEra :: forall era. IsRecentEra era => Cardano.ShelleyBasedEra era
shelleyBasedEra = shelleyBasedEraFromRecentEra $ recentEra @era

data MaybeInRecentEra (thing :: Type -> Type)
    = InNonRecentEraByron
    | InNonRecentEraShelley
    | InNonRecentEraAllegra
    | InNonRecentEraMary
    | InNonRecentEraAlonzo
    | InRecentEraBabbage (thing Cardano.BabbageEra)
    | InRecentEraConway (thing Cardano.ConwayEra)

deriving instance (Eq (a Cardano.BabbageEra), (Eq (a Cardano.ConwayEra)))
    => Eq (MaybeInRecentEra a)
deriving instance (Show (a Cardano.BabbageEra), (Show (a Cardano.ConwayEra)))
    => Show (MaybeInRecentEra a)

toRecentEraGADT
    :: MaybeInRecentEra a
    -> Either Cardano.AnyCardanoEra (InAnyRecentEra a)
toRecentEraGADT = \case
    InNonRecentEraByron   -> Left $ Cardano.AnyCardanoEra Cardano.ByronEra
    InNonRecentEraShelley -> Left $ Cardano.AnyCardanoEra Cardano.ShelleyEra
    InNonRecentEraAllegra -> Left $ Cardano.AnyCardanoEra Cardano.AllegraEra
    InNonRecentEraMary    -> Left $ Cardano.AnyCardanoEra Cardano.MaryEra
    InNonRecentEraAlonzo  -> Left $ Cardano.AnyCardanoEra Cardano.AlonzoEra
    InRecentEraBabbage a  -> Right $ InAnyRecentEra recentEra a
    InRecentEraConway a   -> Right $ InAnyRecentEra recentEra a

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
    Cardano.InAnyCardanoEra Cardano.ByronEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.ShelleyEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.AllegraEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.MaryEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.AlonzoEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    Cardano.InAnyCardanoEra Cardano.ConwayEra a ->
        Just $ InAnyRecentEra RecentEraConway a

-- | An existential type like 'AnyCardanoEra', but for 'RecentEra'.
data AnyRecentEra where
     AnyRecentEra :: IsRecentEra era -- Provide class constraint
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

toAnyCardanoEra :: AnyRecentEra -> Cardano.AnyCardanoEra
toAnyCardanoEra (AnyRecentEra era) = Cardano.AnyCardanoEra (fromRecentEra era)

fromAnyCardanoEra
    :: Cardano.AnyCardanoEra
    -> Maybe AnyRecentEra
fromAnyCardanoEra = \case
    Cardano.AnyCardanoEra Cardano.ByronEra -> Nothing
    Cardano.AnyCardanoEra Cardano.ShelleyEra -> Nothing
    Cardano.AnyCardanoEra Cardano.AllegraEra -> Nothing
    Cardano.AnyCardanoEra Cardano.MaryEra -> Nothing
    Cardano.AnyCardanoEra Cardano.AlonzoEra -> Nothing
    Cardano.AnyCardanoEra Cardano.BabbageEra
        -> Just $ AnyRecentEra RecentEraBabbage
    Cardano.AnyCardanoEra Cardano.ConwayEra
        -> Just $ AnyRecentEra RecentEraConway

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
    -> TxOut (Cardano.ShelleyLedgerEra era)
    -> TxOut (Cardano.ShelleyLedgerEra era)
modifyTxOutValue RecentEraConway f (BabbageTxOut addr val dat script) =
        BabbageTxOut addr (f val) dat script
modifyTxOutValue RecentEraBabbage f (BabbageTxOut addr val dat script) =
        BabbageTxOut addr (f val) dat script

modifyTxOutCoin
    :: RecentEra era
    -> (Coin -> Coin)
    -> TxOut (Cardano.ShelleyLedgerEra era)
    -> TxOut (Cardano.ShelleyLedgerEra era)
modifyTxOutCoin era = modifyTxOutValue era . modifyCoin

txOutValue
    :: RecentEra era
    -> TxOut (Cardano.ShelleyLedgerEra era)
    -> MaryValue StandardCrypto
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

unwrapTxOutInRecentEra
    :: RecentEra era
    -> TxOutInRecentEra
    -> (TxOut (Cardano.ShelleyLedgerEra era))
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
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
    -> TxOut (Cardano.ShelleyLedgerEra era)
    -> Coin
computeMinimumCoinForTxOut era pp out = withConstraints era $
    Core.getMinCoinTxOut pp (withMaxLengthSerializedCoin out)
  where
    withMaxLengthSerializedCoin
        :: TxOut (Cardano.ShelleyLedgerEra era)
        -> TxOut (Cardano.ShelleyLedgerEra era)
    withMaxLengthSerializedCoin =
        modifyTxOutCoin era (const $ Convert.toLedger txOutMaxCoin)

isBelowMinimumCoinForTxOut
    :: forall era. RecentEra era
    -- FIXME [ADP-2353] Replace 'RecentEra' with 'IsRecentEra'
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
    -> TxOut (Cardano.ShelleyLedgerEra era)
    -> Bool
isBelowMinimumCoinForTxOut era pp out =
    actualCoin < requiredMin
  where
    -- IMPORTANT to use the exact minimum from the ledger function, and not our
    -- overestimating 'computeMinimumCoinForTxOut'.
    requiredMin = withConstraints era $ Core.getMinCoinTxOut pp out
    actualCoin = getCoin era out

    getCoin :: RecentEra era -> TxOut (Cardano.ShelleyLedgerEra era) -> Coin
    getCoin RecentEraConway (Babbage.BabbageTxOut _ val _ _) = coin val
    getCoin RecentEraBabbage (Babbage.BabbageTxOut _ val _ _) = coin val

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Construct a 'UTxO era' using 'TxIn's and 'TxOut's in said era.
utxoFromTxOuts
    :: RecentEra era
    -> [(TxIn, Core.TxOut (Cardano.ShelleyLedgerEra era))]
    -> (Shelley.UTxO (Cardano.ShelleyLedgerEra era))
utxoFromTxOuts era = withConstraints era $
    Shelley.UTxO . Map.fromList

-- | Construct a 'UTxO era' using 'TxOutInRecentEra'.
--
-- Used to have a possibility for failure when we supported Alonzo and Babbage,
-- and could possibly become failable again with future eras.
utxoFromTxOutsInRecentEra
    :: forall era. RecentEra era
    -> [(TxIn, TxOutInRecentEra)]
    -> Shelley.UTxO (Cardano.ShelleyLedgerEra era)
utxoFromTxOutsInRecentEra era = withConstraints era $
    Shelley.UTxO . Map.fromList . map (second (unwrapTxOutInRecentEra era))

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

txBody
    :: RecentEra era
    -> Core.Tx (Cardano.ShelleyLedgerEra era)
    -> Core.TxBody (Cardano.ShelleyLedgerEra era)
txBody era = case era of
    RecentEraBabbage -> Babbage.body -- same type for babbage
    RecentEraConway -> Babbage.body -- same type for conway

-- Until we have convenient lenses to use
outputs
    :: RecentEra era
    -> Core.TxBody (Cardano.ShelleyLedgerEra era)
    -> [TxOut (Cardano.ShelleyLedgerEra era)]
outputs RecentEraConway = map sizedValue . toList . Conway.ctbOutputs
outputs RecentEraBabbage = map sizedValue . toList . Babbage.btbOutputs

-- NOTE: To reduce the need for the caller to deal with @CardanoApiEra
-- (Cardano.ShelleyLedgerEra era) ~ era@, we quantify this function over
-- @cardanoEra@ instead of @era@.
--
-- TODO [ADP-2353] Move to @cardano-api@ related module
modifyLedgerBody
    :: (Core.TxBody (Cardano.ShelleyLedgerEra cardanoEra)
        -> Core.TxBody (Cardano.ShelleyLedgerEra cardanoEra))
    -> Cardano.Tx cardanoEra
    -> Cardano.Tx cardanoEra
modifyLedgerBody f (Cardano.Tx body keyWits) = Cardano.Tx body' keyWits
  where
    body' =
        case body of
            Cardano.ByronTxBody {} ->
                error "Impossible: ByronTxBody in Cardano.ShelleyLedgerEra"
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

emptyTx :: RecentEra era -> Core.Tx (Cardano.ShelleyLedgerEra era)
emptyTx era = withConstraints era $ Core.mkBasicTx Core.mkBasicTxBody

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

toCardanoTx
    :: forall era. IsRecentEra era
    => Core.Tx (Cardano.ShelleyLedgerEra era)
    -> Cardano.Tx era
toCardanoTx = Cardano.ShelleyTx (shelleyBasedEraFromRecentEra $ recentEra @era)

toCardanoUTxO
    :: forall era. IsRecentEra era
    => Shelley.UTxO (Cardano.ShelleyLedgerEra era)
    -> Cardano.UTxO era
toCardanoUTxO = withConstraints (recentEra @era) $
    Cardano.UTxO
    . Map.mapKeys Cardano.fromShelleyTxIn
    . Map.map (Cardano.fromShelleyTxOut (shelleyBasedEra @era))
    . unUTxO
  where
    unUTxO (Shelley.UTxO m) = m

fromCardanoUTxO
    :: forall era. IsRecentEra era
    => Cardano.UTxO era
    -> Shelley.UTxO (Cardano.ShelleyLedgerEra era)
fromCardanoUTxO = withConstraints (recentEra @era) $
    Shelley.UTxO
    . Map.mapKeys Cardano.toShelleyTxIn
    . Map.map (Cardano.toShelleyTxOut (shelleyBasedEra @era))
    . Cardano.unUTxO

toCardanoValue
    :: forall era. IsRecentEra era
    => Core.Value (Cardano.ShelleyLedgerEra era)
    -> Cardano.Value
toCardanoValue = withConstraints (recentEra @era) Cardano.fromMaryValue

toCardanoLovelace :: Coin -> Cardano.Lovelace
toCardanoLovelace = Cardano.fromShelleyLovelace

--------------------------------------------------------------------------------
-- PParams
--------------------------------------------------------------------------------

-- | The 'minfeeA' protocol parameter in unit @lovelace/byte@.
newtype FeePerByte = FeePerByte Natural
    deriving (Show, Eq)

getFeePerByte
    :: HasCallStack
    => RecentEra era
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
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
    :: RecentEra era -> Core.PParams (Cardano.ShelleyLedgerEra era) -> Coin
maxScriptExecutionCost era pp = withConstraints era $
    txscriptfee (pp ^. Alonzo.ppPricesL) (pp ^. Alonzo.ppMaxTxExUnitsL)

stakeKeyDeposit
    :: RecentEra era -> Core.PParams (Cardano.ShelleyLedgerEra era) -> Coin
stakeKeyDeposit era pp = withConstraints era $ pp ^. Core.ppKeyDepositL

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
    -> Core.PParams (Cardano.ShelleyLedgerEra era)
    -> Shelley.UTxO (Cardano.ShelleyLedgerEra era)
    -> Core.TxBody (Cardano.ShelleyLedgerEra era)
    -> Core.Value (Cardano.ShelleyLedgerEra era)
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
