{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
    -- ** Key witness counts
      KeyWitnessCounts (..)

    -- ** Helpers for cardano-api compatibility
    , fromCardanoApiTx
    , toCardanoApiTx

    -- ** Misc
    , StandardCrypto
    , BabbageEra
    , ConwayEra

    -- * PParams
    , PParams
    , PParamsInAnyRecentEra (..)
    , toRecentEraGADT
    , FeePerByte (..)
    , getFeePerByte
    , feeOfBytes
    , maxScriptExecutionCost
    , stakeKeyDeposit
    , ProtVer (..)
    , Version

    -- * Tx
    , Tx
    , TxBody
    , serializeTx
    , deserializeTx

    -- * TxId
    , Ledger.TxId

    -- * TxOut
    , TxOut
    , BabbageTxOut (..)
    , TxOutInBabbage
    , TxOutInRecentEra (..)
    , ErrInvalidTxOutInEra (..)
    , unwrapTxOutInRecentEra
    , wrapTxOutInRecentEra

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

    -- ** Rewards
    , RewardAccount
    , StakeCredential

    -- ** Script
    , Script
    , Alonzo.isPlutusScript
    , ScriptHash

    -- * TxIn
    , TxIn
    , unsafeMkTxIn

    -- * UTxO
    , Shelley.UTxO (..)
    , utxoFromTxOutsInRecentEra
    , unsafeUtxoFromTxOutsInRecentEra
    , forceUTxOToEra

    -- * Policy and asset identifiers
    , type PolicyId
    , pattern PolicyId
    , AssetName

    -- * Balancing
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
import Cardano.Ledger.Api
    ( Tx
    , TxBody
    , TxOut
    , coinTxOutL
    , upgradeTxOut
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (..)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.BaseTypes
    ( ProtVer (..)
    , StrictMaybe (..)
    , Version
    , maybeToStrictMaybe
    , strictMaybeToMaybe
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Conway.PParams
    ( ppDRepDepositL
    )
import Cardano.Ledger.Conway.Scripts
    ( PlutusScript (..)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Mary
    ( MaryValue
    )
import Cardano.Ledger.Mary.Value
    ( AssetName
    )
import Cardano.Ledger.Plutus.Data
    ( BinaryData
    , Datum (..)
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
import Data.Generics.Internal.VL.Lens
    ( over
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import GHC.Stack
    ( HasCallStack
    )
import Internal.Cardano.Write.Eras
    ( Babbage
    , CardanoApiEra
    , Conway
    , IsRecentEra (..)
    , LatestLedgerEra
    , MaybeInRecentEra (..)
    , RecentEra (..)
    , shelleyBasedEraFromRecentEra
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Eras
    ( BabbageEra
    , ConwayEra
    )

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Core as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Core
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Value
import qualified Cardano.Ledger.Plutus.Data as Alonzo
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( txOutMaxCoin
    )
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Key witness counts
--------------------------------------------------------------------------------

data KeyWitnessCounts = KeyWitnessCounts
    { nKeyWits :: !Word
    -- ^ "Normal" verification key witnesses introduced with the Shelley era.

    , nBootstrapWits :: !Word
    -- ^ Bootstrap key witnesses, a.k.a Byron witnesses.
    } deriving (Eq, Show)

instance Semigroup KeyWitnessCounts where
    KeyWitnessCounts s1 b1 <> KeyWitnessCounts s2 b2
        = KeyWitnessCounts (s1 + s2) (b1 + b2)

instance Monoid KeyWitnessCounts where
    mempty = KeyWitnessCounts 0 0

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

type TxOutInBabbage = Babbage.BabbageTxOut Babbage

type Address = Ledger.Addr StandardCrypto

type RewardAccount = Ledger.RewardAccount StandardCrypto
type Script = AlonzoScript
type ScriptHash = Core.ScriptHash StandardCrypto
type Value = MaryValue StandardCrypto

unsafeAddressFromBytes :: ByteString -> Address
unsafeAddressFromBytes bytes = case Ledger.decodeAddr bytes of
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

wrapTxOutInRecentEra
    :: forall era. IsRecentEra era
    => TxOut era
    -> TxOutInRecentEra
wrapTxOutInRecentEra out = case recentEra @era of
    RecentEraConway ->
        let
            BabbageTxOut addr v d s = out
        in
            TxOutInRecentEra addr v d (strictMaybeToMaybe s)
    RecentEraBabbage -> wrapTxOutInRecentEra @Conway $ upgradeTxOut out

data ErrInvalidTxOutInEra
    = InlinePlutusV3ScriptNotSupportedInBabbage
    deriving (Show, Eq)

unwrapTxOutInRecentEra
    :: forall era. IsRecentEra era
    => TxOutInRecentEra
    -> Either ErrInvalidTxOutInEra (TxOut era)
unwrapTxOutInRecentEra recentEraTxOut = case recentEra @era of
    RecentEraConway -> pure $ recentEraToConwayTxOut recentEraTxOut
    RecentEraBabbage -> recentEraToBabbageTxOut recentEraTxOut

recentEraToConwayTxOut
    :: TxOutInRecentEra
    -> Babbage.BabbageTxOut LatestLedgerEra
recentEraToConwayTxOut (TxOutInRecentEra addr val datum mscript) =
    Babbage.BabbageTxOut addr val datum (maybeToStrictMaybe mscript)

recentEraToBabbageTxOut
    :: TxOutInRecentEra
    -> Either ErrInvalidTxOutInEra (BabbageTxOut Babbage)
recentEraToBabbageTxOut (TxOutInRecentEra addr val datum mscript) =
    Babbage.BabbageTxOut addr val
        (downgradeDatum datum)
        <$> (maybe (Right SNothing) (fmap SJust . downgradeScript) mscript)
  where
    downgradeDatum = \case
        Alonzo.NoDatum ->
            Alonzo.NoDatum
        Alonzo.DatumHash h ->
            Alonzo.DatumHash h
        Alonzo.Datum binaryData ->
            Alonzo.Datum (coerce binaryData)

    downgradeScript
        :: AlonzoScript Conway
        -> Either ErrInvalidTxOutInEra (AlonzoScript Babbage)
    downgradeScript = \case
        TimelockScript timelockEra
            -> pure $ Alonzo.TimelockScript (translateTimelock timelockEra)
        PlutusScript s
            -> PlutusScript <$> downgradePlutusScript s

    downgradePlutusScript
        :: PlutusScript Conway
        -> Either ErrInvalidTxOutInEra (PlutusScript Babbage)
    downgradePlutusScript = \case
        ConwayPlutusV1 s -> pure $ BabbagePlutusV1 s
        ConwayPlutusV2 s -> pure $ BabbagePlutusV2 s
        ConwayPlutusV3 _s -> Left InlinePlutusV3ScriptNotSupportedInBabbage

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
    :: forall era. IsRecentEra era
    => PParams era
    -> TxOut era
    -> Coin
computeMinimumCoinForTxOut pp out =
    Core.getMinCoinTxOut pp (withMaxLengthSerializedCoin out)
  where
    withMaxLengthSerializedCoin
        :: TxOut era
        -> TxOut era
    withMaxLengthSerializedCoin =
        over coinTxOutL (const $ Convert.toLedger W.txOutMaxCoin)

isBelowMinimumCoinForTxOut
    :: forall era. IsRecentEra era
    => PParams era
    -> TxOut era
    -> Bool
isBelowMinimumCoinForTxOut pp out =
    actualCoin < requiredMin
  where
    -- IMPORTANT to use the exact minimum from the ledger function, and not our
    -- overestimating 'computeMinimumCoinForTxOut'.
    requiredMin = Core.getMinCoinTxOut pp out
    actualCoin = out ^. coinTxOutL

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

-- | Construct a 'UTxO era' using 'TxOutInRecentEra'.
--
-- Used to have a possibility for failure when we supported Alonzo and Babbage,
-- and could possibly become failable again with future eras.
utxoFromTxOutsInRecentEra
    :: IsRecentEra era
    => [(TxIn, TxOutInRecentEra)]
    -> Either ErrInvalidTxOutInEra (Shelley.UTxO era)
utxoFromTxOutsInRecentEra =
    fmap (Shelley.UTxO . Map.fromList) . mapM (secondM unwrapTxOutInRecentEra)

  where
    secondM :: Monad m => (o -> m o') -> (i, o) -> m (i, o')
    secondM f (i, o) = f o >>= \o' -> return (i, o')

unsafeUtxoFromTxOutsInRecentEra
    :: IsRecentEra era
    => [(TxIn, TxOutInRecentEra)]
    -> Shelley.UTxO era
unsafeUtxoFromTxOutsInRecentEra =
    either (error . show) id . utxoFromTxOutsInRecentEra

forceUTxOToEra
    :: forall era1 era2. (IsRecentEra era1, IsRecentEra era2)
    => UTxO era1
    -> Either ErrInvalidTxOutInEra (UTxO era2)
forceUTxOToEra (UTxO utxo) =
    utxoFromTxOutsInRecentEra
    $ map (second wrapTxOutInRecentEra)
    $ Map.toList utxo

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

serializeTx
    :: forall era. IsRecentEra era
    => Tx era
    -> ByteString
serializeTx tx =
    CardanoApi.serialiseToCBOR $ toCardanoApiTx tx

deserializeTx :: forall era. IsRecentEra era => ByteString -> Tx era
deserializeTx = case recentEra @era of
    RecentEraBabbage -> deserializeBabbageTx
    RecentEraConway -> deserializeConwayTx
  where
    deserializeBabbageTx :: ByteString -> Tx Babbage
    deserializeBabbageTx
        = fromCardanoApiTx
        . either (error . show) id
        . CardanoApi.deserialiseFromCBOR (CardanoApi.AsTx CardanoApi.AsBabbageEra)

    deserializeConwayTx :: ByteString -> Tx Conway
    deserializeConwayTx
        = fromCardanoApiTx
        . either (error . show) id
        . CardanoApi.deserialiseFromCBOR (CardanoApi.AsTx CardanoApi.AsConwayEra)

--------------------------------------------------------------------------------
-- Compatibility
--------------------------------------------------------------------------------

fromCardanoApiTx
    :: IsRecentEra era
    => CardanoApi.Tx (CardanoApiEra era)
    -> Tx era
fromCardanoApiTx = \case
    CardanoApi.ShelleyTx _era tx ->
        tx

toCardanoApiTx
    :: forall era. IsRecentEra era
    => Tx era
    -> CardanoApi.Tx (CardanoApiEra era)
toCardanoApiTx =
    CardanoApi.ShelleyTx
    $ shelleyBasedEraFromRecentEra (recentEra :: RecentEra era)

--------------------------------------------------------------------------------
-- PParams
--------------------------------------------------------------------------------

type PParams = Core.PParams

data PParamsInAnyRecentEra where
    PParamsInAnyRecentEra
        :: IsRecentEra era
        => RecentEra era
        -> PParams era
        -> PParamsInAnyRecentEra

toRecentEraGADT
    :: MaybeInRecentEra PParams
    -> Either CardanoApi.AnyCardanoEra PParamsInAnyRecentEra
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
        Right $ PParamsInAnyRecentEra recentEra a
    InRecentEraConway a ->
        Right $ PParamsInAnyRecentEra recentEra a

-- | The 'minfeeA' protocol parameter in unit @lovelace/byte@.
newtype FeePerByte = FeePerByte Natural
    deriving (Show, Eq)

getFeePerByte
    :: forall era. (HasCallStack, IsRecentEra era)
    => PParams era
    -> FeePerByte
getFeePerByte pp =
    unsafeCoinToFee $
        case recentEra @era of
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

maxScriptExecutionCost :: IsRecentEra era => PParams era -> Coin
maxScriptExecutionCost pp =
    txscriptfee (pp ^. Alonzo.ppPricesL) (pp ^. Alonzo.ppMaxTxExUnitsL)

stakeKeyDeposit :: IsRecentEra era => PParams era -> Coin
stakeKeyDeposit pp = pp ^. Core.ppKeyDepositL

--------------------------------------------------------------------------------
-- Balancing
--------------------------------------------------------------------------------

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
    :: forall era. IsRecentEra era
    => PParams era
    -> (StakeCredential -> Maybe Coin)
    -> Shelley.UTxO era
    -> TxBody era
    -> Core.Value era
evaluateTransactionBalance pp depositLookup =
    Ledger.evalBalanceTxBody
        pp
        depositLookup
        dRepDepositAssumeCurrent
        assumePoolIsReg
  where
    -- Deposit lookup for `UnRegDRep` certificates in the TxBody
    --
    -- TODO [ADP-3404] Query actual value of deposit
    --
    -- https://cardanofoundation.atlassian.net/browse/ADP-3404
    dRepDepositAssumeCurrent
        :: Core.Credential 'Ledger.DRepRole StandardCrypto
        -> Maybe Coin
    dRepDepositAssumeCurrent _drepCred = case recentEra @era of
        RecentEraConway -> Just $ pp ^. ppDRepDepositL
        RecentEraBabbage -> error "impossible: lookupDRep called in Babbage"

    -- Checks whether a pool with a supplied 'PoolStakeId' is already
    -- registered.
    --
    -- There is no requirement to answer this question for all stake pool
    -- credentials, just those that have their registration certificates
    -- included in the supplied 'TxBody'.
    -- TODO [ADP-3274] Query actual registration status
    --
    -- https://cardanofoundation.atlassian.net/browse/ADP-3274
    assumePoolIsReg
        :: Ledger.KeyHash 'Ledger.StakePool StandardCrypto
        -> Bool
    assumePoolIsReg _keyHash = True

--------------------------------------------------------------------------------
-- Policy and asset identifiers
--------------------------------------------------------------------------------

type PolicyId = Value.PolicyID StandardCrypto

{-# COMPLETE PolicyId #-}
pattern PolicyId
    :: Core.ScriptHash StandardCrypto
    -> Value.PolicyID StandardCrypto
pattern PolicyId h = Value.PolicyID h

--------------------------------------------------------------------------------
-- Stake Credential
--------------------------------------------------------------------------------

type StakeCredential = Core.StakeCredential StandardCrypto
