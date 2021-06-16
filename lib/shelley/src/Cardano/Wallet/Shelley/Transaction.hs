{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer

    -- * Internals
    , TxPayload (..)
    , TxSkeleton (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor (..)
    , _decodeSignedTx
    , _estimateMaxNumberOfInputs
    , emptyTxPayload
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyWitness
    , mkTx
    , mkTxSkeleton
    , mkUnsignedTx
    , txConstraints
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, toXPub )
import Cardano.Api
    ( AnyCardanoEra (..)
    , ByronEra
    , CardanoEra (..)
    , IsShelleyBasedEra (..)
    , NetworkId
    , SerialiseAsCBOR (..)
    , ShelleyBasedEra (..)
    )
import Cardano.Binary
    ( ToCBOR, serialize' )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Ledger.Crypto
    ( DSIGN )
import Cardano.Ledger.Era
    ( Crypto, Era )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase (..), RewardAccount (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toRewardAccountRaw )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionCriteria (..)
    , SelectionLimit (..)
    , SelectionResult (changeGenerated, inputsSelected, outputsCovered)
    , SelectionSkeleton (..)
    , prepareOutputsWith
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..), ProtocolParameters (..), TxParameters (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin, subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , SerialisedTx (..)
    , TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , Tx (..)
    , TxConstraints (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxSize (..)
    , txOutCoin
    , txOutMaxTokenQuantity
    , txSizeDistance
    )
import Cardano.Wallet.Shelley.Compatibility
    ( fromAllegraTx
    , fromMaryTx
    , fromShelleyTx
    , maxTokenBundleSerializedLengthBytes
    , sealShelleyTx
    , toAllegraTxOut
    , toCardanoLovelace
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toHDPayloadAddress
    , toMaryTxOut
    , toShelleyTxOut
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( computeMinimumAdaQuantity )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)
    , ErrSelectionCriteria (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , withdrawalToCoin
    )
import Control.Arrow
    ( first, left, second )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Kind
    ( Type )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Type.Equality
    ( type (==) )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( Buildable, pretty )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Network.Block
    ( SlotNo )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL

-- | Type encapsulating what we need to know to add things -- payloads,
-- certificates -- to a transaction.
--
-- Designed to allow us to have /one/ @mkTx@ which doesn't care whether we
-- include certificates or not.
data TxPayload era = TxPayload
    { _metadata ::  Maybe Cardano.TxMetadata
      -- ^ User or application-defined metadata to be included in the
      -- transaction.

    , _certificates :: [Cardano.Certificate]
      -- ^ Certificates to be included in the transactions.

    , _extraWitnesses :: Cardano.TxBody era -> [Cardano.KeyWitness era]
      -- ^ Create payload-specific witesses given the unsigned transaction body.
      --
      -- Caller has the freedom and responsibility to provide the correct
      -- witnesses for what they're trying to do.
    }

emptyTxPayload :: TxPayload era
emptyTxPayload = TxPayload Nothing mempty mempty

data TxWitnessTag
    = TxWitnessByronUTxO WalletStyle
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

data WalletStyle
    = Icarus
    | Byron
    deriving (Show, Eq)

type EraConstraints era =
    ( IsShelleyBasedEra era
    , ToCBOR (SL.TxBody (Cardano.ShelleyLedgerEra era))
    , Era (Cardano.ShelleyLedgerEra era)
    , DSIGN (Crypto (Cardano.ShelleyLedgerEra era)) ~ DSIGN.Ed25519DSIGN
    , (era == ByronEra) ~ 'False
    )

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
class TxWitnessTagFor (k :: Depth -> Type -> Type) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor ShelleyKey where
    txWitnessTagFor = TxWitnessShelleyUTxO

instance TxWitnessTagFor IcarusKey where
    txWitnessTagFor = TxWitnessByronUTxO Icarus

instance TxWitnessTagFor ByronKey where
    txWitnessTagFor = TxWitnessByronUTxO Byron

constructUnsignedTx
    :: forall era.
        ( EraConstraints era
        )
    => Cardano.NetworkId
    -> (Maybe Cardano.TxMetadata, [Cardano.Certificate])
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> RewardAccount
    -- ^ Reward account
    -> Coin
    -- ^ An optional withdrawal amount, can be zero
    -> SelectionResult TxOut
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> ShelleyBasedEra era
    -> Either ErrMkTx SerialisedTx
constructUnsignedTx networkId (md, certs) ttl rewardAcnt wdrl cs fee era =
    SerialisedTx . serialiseToCBOR <$> tx
  where
    tx = mkUnsignedTx era ttl cs md wdrls certs (toCardanoLovelace fee)
    wdrls = mkWithdrawals networkId rewardAcnt wdrl

mkTx
    :: forall k era.
        ( TxWitnessTagFor k
        , WalletKey k
        , EraConstraints era
        )
    => Cardano.NetworkId
    -> TxPayload era
    -> SlotNo
    -- ^ Slot at which the transaction will expire.
    -> (XPrv, Passphrase "encryption")
    -- ^ Reward account
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -- ^ Key store
    -> Coin
    -- ^ An optional withdrawal amount, can be zero
    -> SelectionResult TxOut
    -- ^ Finalized asset selection
    -> Coin
    -- ^ Explicit fee amount
    -> ShelleyBasedEra era
    -> Either ErrMkTx (Tx, SealedTx)
mkTx networkId payload ttl (rewardAcnt, pwdAcnt) keyFrom wdrl cs fees era = do
    let TxPayload md certs mkExtraWits = payload
    let wdrls = mkWithdrawals
            networkId
            (toRewardAccountRaw . toXPub $ rewardAcnt)
            wdrl

    unsigned <- mkUnsignedTx era ttl cs md wdrls certs (toCardanoLovelace fees)

    wits <- case (txWitnessTagFor @k) of
        TxWitnessShelleyUTxO -> do
            addrWits <- forM (inputsSelected cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkShelleyWitness unsigned (getRawKey k, pwd)

            let wdrlsWits
                    | null wdrls = []
                    | otherwise =
                      [mkShelleyWitness unsigned (rewardAcnt, pwdAcnt)]

            pure $ mkExtraWits unsigned <> F.toList addrWits <> wdrlsWits

        TxWitnessByronUTxO{} -> do
            bootstrapWits <- forM (inputsSelected cs) $ \(_, TxOut addr _) -> do
                (k, pwd) <- lookupPrivateKey keyFrom addr
                pure $ mkByronWitness unsigned networkId addr (getRawKey k, pwd)
            pure $ F.toList bootstrapWits <> mkExtraWits unsigned

    let signed = Cardano.makeSignedTransaction wits unsigned
    let withResolvedInputs tx = tx
            { resolvedInputs = second txOutCoin <$> F.toList (inputsSelected cs)
            }
    Right $ first withResolvedInputs $ case era of
        ShelleyBasedEraShelley -> sealShelleyTx fromShelleyTx signed
        ShelleyBasedEraAllegra -> sealShelleyTx fromAllegraTx signed
        ShelleyBasedEraMary    -> sealShelleyTx fromMaryTx signed

newTransactionLayer
    :: forall k.
        ( TxWitnessTagFor k
        , WalletKey k
        )
    => NetworkId
    -> TransactionLayer k
newTransactionLayer networkId = TransactionLayer
    { mkTransaction = \era stakeCreds keystore pp ctx selection -> do
        let ttl   = txTimeToLive ctx
        let wdrl  = withdrawalToCoin $ view #txWithdrawal ctx
        let delta = selectionDelta txOutCoin selection
        case view #txDelegationAction ctx of
            Nothing -> do
                withShelleyBasedEra era $ do
                    let payload = TxPayload (view #txMetadata ctx) mempty mempty
                    let fees = delta
                    mkTx networkId payload ttl stakeCreds keystore wdrl selection fees

            Just action -> do
                withShelleyBasedEra era $ do
                    let stakeXPub = toXPub $ fst stakeCreds
                    let certs = mkDelegationCertificates action stakeXPub
                    let mkWits unsigned =
                            [ mkShelleyWitness unsigned stakeCreds
                            ]
                    let payload = TxPayload (view #txMetadata ctx) certs mkWits
                    let fees = case action of
                            RegisterKeyAndJoin{} ->
                                unsafeSubtractCoin selection delta (stakeKeyDeposit pp)
                            _ ->
                                delta
                    mkTx networkId payload ttl stakeCreds keystore wdrl selection fees

    , mkUnsignedTransaction = \era stakeXPub pp ctx selection -> do
        let ttl   = txTimeToLive ctx
        let wdrl  = withdrawalToCoin $ view #txWithdrawal ctx
        let delta = selectionDelta txOutCoin selection
        let rewardAcct = toRewardAccountRaw stakeXPub
        case view #txDelegationAction ctx of
            Nothing -> do
                withShelleyBasedEra era $ do
                    let md = view #txMetadata ctx
                    let fees = delta
                    constructUnsignedTx networkId (md, []) ttl rewardAcct wdrl selection fees

            Just action -> do
                withShelleyBasedEra era $ do
                    let certs = mkDelegationCertificates action stakeXPub
                    let payload = (view #txMetadata ctx, certs)
                    let fees = case action of
                            RegisterKeyAndJoin{} ->
                                unsafeSubtractCoin selection delta (stakeKeyDeposit pp)
                            _ ->
                                delta
                    constructUnsignedTx networkId payload ttl rewardAcct wdrl selection fees

    , initSelectionCriteria = _initSelectionCriteria @k

    , calcMinimumCost = \pp ctx skeleton ->
        estimateTxCost pp $
        mkTxSkeleton (txWitnessTagFor @k) ctx skeleton

    , tokenBundleSizeAssessor =
        Compatibility.tokenBundleSizeAssessor

    , constraints = \pp -> txConstraints pp (txWitnessTagFor @k)

    , decodeSignedTx =
        _decodeSignedTx
    }
  where
    unsafeSubtractCoin
        :: (HasCallStack, Buildable ctx) => ctx -> Coin -> Coin -> Coin
    unsafeSubtractCoin ctx a b = case a `subtractCoin` b of
        Nothing -> error $ unlines
            [ "unsafeSubtractCoin: got a negative value. Tried to subtract "
            <> show b <> " from " <> show a <> "."
            , "In the context of: "
            , pretty ctx
            ]
        Just c ->
            c

mkDelegationCertificates
    :: DelegationAction
        -- Pool Id to which we're planning to delegate
    -> XPub
        -- Reward account public key
    -> [Cardano.Certificate]
mkDelegationCertificates da accXPub =
    case da of
       Join poolId ->
               [ toStakePoolDlgCert accXPub poolId ]
       RegisterKeyAndJoin poolId ->
               [ toStakeKeyRegCert  accXPub
               , toStakePoolDlgCert accXPub poolId
               ]
       Quit -> [toStakeKeyDeregCert accXPub]

_calcMinimumCoinValue
    :: ProtocolParameters
    -> TokenMap
    -> Coin
_calcMinimumCoinValue pp =
    computeMinimumAdaQuantity (minimumUTxOvalue pp)

-- NOTE / FIXME: This is an 'estimation' because it is actually quite hard to
-- estimate what would be the cost of a selecting a particular input. Indeed, an
-- input may contain any arbitrary assets, which has a direct impact on the
-- shape of change outputs. In practice, this should work out pretty well
-- because of other approximations done along the way which should compensate
-- for possible extra assets in inputs not counted as part of this estimation.
--
-- Worse that may happen here is the wallet generating a transaction that is
-- slightly too big, For a better user experience, we could detect that earlier
-- before submitting the transaction and return a more user-friendly error.
--
-- Or... to be even better, the 'SelectionLimit' from the RoundRobin module
-- could be a function of the 'SelectionState' already selected. With this
-- information and the shape of the requested output, we can get down to a
-- pretty accurate result.
_estimateMaxNumberOfInputs
    :: forall k. TxWitnessTagFor k
    => Quantity "byte" Word16
     -- ^ Transaction max size in bytes
    -> TransactionCtx
     -- ^ An additional transaction context
    -> [TxOut]
     -- ^ A list of outputs being considered.
    -> Int
_estimateMaxNumberOfInputs txMaxSize ctx outs =
    fromIntegral $ findLargestUntil ((> maxSize) . txSizeGivenInputs) 0
  where
    -- | Find the largest amount of inputs that doesn't make the tx too big.
    -- Tries in sequence from 0 and upward (up to 255, but smaller than 50 in
    -- practice because of the max transaction size).
    findLargestUntil :: (Integer -> Bool) -> Integer -> Integer
    findLargestUntil isTxTooLarge inf
        | inf == maxNInps        = maxNInps
        | isTxTooLarge (inf + 1) = inf
        | otherwise              = findLargestUntil isTxTooLarge (inf + 1)

    maxSize  = toInteger (getQuantity txMaxSize)
    maxNInps = 255 -- Arbitrary, but large enough.

    txSizeGivenInputs nInps = fromIntegral size
      where
        TxSize size = estimateTxSize $ mkTxSkeleton
            (txWitnessTagFor @k) ctx sel
        sel  = dummySkeleton (fromIntegral nInps) outs

_initSelectionCriteria
    :: forall k. TxWitnessTagFor k
    => ProtocolParameters
    -> TransactionCtx
    -> UTxOIndex.UTxOIndex
    -> NE.NonEmpty TxOut
    -> Either ErrSelectionCriteria SelectionCriteria
_initSelectionCriteria pp ctx utxoAvailable outputsUnprepared
    | (address, assetCount) : _ <- excessivelyLargeBundles =
        Left $
            -- We encountered one or more excessively large token bundles.
            -- Just report the first such bundle:
            ErrSelectionCriteriaOutputTokenBundleSizeExceedsLimit $
            ErrOutputTokenBundleSizeExceedsLimit {address, assetCount}
    | (address, asset, quantity) : _ <- excessiveTokenQuantities =
        Left $
            -- We encountered one or more excessive token quantities.
            -- Just report the first such quantity:
            ErrSelectionCriteriaOutputTokenQuantityExceedsLimit $
            ErrOutputTokenQuantityExceedsLimit
                { address
                , asset
                , quantity
                , quantityMaxBound = txOutMaxTokenQuantity
                }
    | otherwise =
        pure SelectionCriteria
            { outputsToCover
            , utxoAvailable
            , selectionLimit
            , extraCoinSource
            , assetsToMint
            , assetsToBurn
            }
  where
    -- The complete list of token bundles whose serialized lengths are greater
    -- than the limit of what is allowed in a transaction output:
    excessivelyLargeBundles :: [(Address, Int)]
    excessivelyLargeBundles =
        [ (address, assetCount)
        | output <- F.toList outputsToCover
        , let bundle = view #tokens output
        , bundleIsExcessivelyLarge bundle
        , let address = view #address output
        , let assetCount = Set.size $ TokenBundle.getAssets bundle
        ]

      where
        bundleIsExcessivelyLarge b = case assessSize b of
            TokenBundleSizeWithinLimit -> False
            OutputTokenBundleSizeExceedsLimit -> True
          where
            assessSize =
                assessTokenBundleSize Compatibility.tokenBundleSizeAssessor

    -- The complete list of token quantities that exceed the maximum quantity
    -- allowed in a transaction output:
    excessiveTokenQuantities :: [(Address, AssetId, TokenQuantity)]
    excessiveTokenQuantities =
        [ (address, asset, quantity)
        | output <- F.toList outputsToCover
        , let address = view #address output
        , (asset, quantity) <-
            TokenMap.toFlatList $ view #tokens $ view #tokens output
        , quantity > txOutMaxTokenQuantity
        ]

    txMaxSize = getTxMaxSize $ txParameters pp

    selectionLimit = MaximumInputLimit $
        _estimateMaxNumberOfInputs @k txMaxSize ctx (NE.toList outputsToCover)

    extraCoinSource = Just $ addCoin
        (withdrawalToCoin $ view #txWithdrawal ctx)
        ( case view #txDelegationAction ctx of
            Just Quit -> stakeKeyDeposit pp
            _ -> Coin 0
        )

    outputsToCover =
        prepareOutputsWith (_calcMinimumCoinValue pp) outputsUnprepared

    -- Until we properly support minting and burning, set to empty.
    assetsToMint = TokenMap.empty
    assetsToBurn = TokenMap.empty

dummySkeleton :: Int -> [TxOut] -> SelectionSkeleton
dummySkeleton inputCount outputs = SelectionSkeleton
    { skeletonInputCount =
        inputCount
    , skeletonOutputs =
        outputs
    , skeletonChange =
        TokenBundle.getAssets . view #tokens <$> outputs
    }

_decodeSignedTx
    :: AnyCardanoEra
    -> ByteString
    -> Either ErrDecodeSignedTx (Tx, SealedTx)
_decodeSignedTx era bytes = do
    case era of
        AnyCardanoEra ShelleyEra ->
            case Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsShelleyEra) bytes of
                Right txValid ->
                    pure $ sealShelleyTx fromShelleyTx txValid
                Left decodeErr ->
                    Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

        AnyCardanoEra AllegraEra ->
            case Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsAllegraEra) bytes of
                Right txValid ->
                    pure $ sealShelleyTx fromAllegraTx txValid
                Left decodeErr ->
                    Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

        AnyCardanoEra MaryEra ->
            case Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsMaryEra) bytes of
                Right txValid ->
                    pure $ sealShelleyTx fromMaryTx txValid
                Left decodeErr ->
                    Left $ ErrDecodeSignedTxWrongPayload (T.pack $ show decodeErr)

        _ ->
            Left ErrDecodeSignedTxNotSupported

txConstraints :: ProtocolParameters -> TxWitnessTag -> TxConstraints
txConstraints protocolParams witnessTag = TxConstraints
    { txBaseCost
    , txBaseSize
    , txInputCost
    , txInputSize
    , txOutputCost
    , txOutputSize
    , txOutputMaximumSize
    , txOutputMaximumTokenQuantity
    , txOutputMinimumAdaQuantity
    , txRewardWithdrawalCost
    , txRewardWithdrawalSize
    , txMaximumSize
    }
  where
    txBaseCost =
        estimateTxCost protocolParams empty

    txBaseSize =
        estimateTxSize empty

    txInputCost =
        marginalCostOf empty {txInputCount = 1}

    txInputSize =
        marginalSizeOf empty {txInputCount = 1}

    txOutputCost bundle =
        marginalCostOf empty {txOutputs = [mkTxOut bundle]}

    txOutputSize bundle =
        marginalSizeOf empty {txOutputs = [mkTxOut bundle]}

    txOutputMaximumSize = (<>)
        (TxSize $ fromIntegral maxTokenBundleSerializedLengthBytes)
        (txOutputSize mempty)

    txOutputMaximumTokenQuantity =
        TokenQuantity $ fromIntegral $ maxBound @Word64

    txOutputMinimumAdaQuantity =
        computeMinimumAdaQuantity (minimumUTxOvalue protocolParams)

    txRewardWithdrawalCost c =
        marginalCostOf empty {txRewardWithdrawal = c}

    txRewardWithdrawalSize c =
        marginalSizeOf empty {txRewardWithdrawal = c}

    txMaximumSize = protocolParams
        & view (#txParameters . #getTxMaxSize)
        & getQuantity
        & fromIntegral
        & TxSize

    empty :: TxSkeleton
    empty = emptyTxSkeleton witnessTag

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalCostOf :: TxSkeleton -> Coin
    marginalCostOf =
        Coin.distance txBaseCost . estimateTxCost protocolParams

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalSizeOf :: TxSkeleton -> TxSize
    marginalSizeOf =
        txSizeDistance txBaseSize . estimateTxSize

    -- Constructs a real transaction output from a token bundle.
    mkTxOut :: TokenBundle -> TxOut
    mkTxOut = TxOut dummyAddress
      where
        dummyAddress :: Address
        dummyAddress = Address $ BS.replicate dummyAddressLength nullByte

        dummyAddressLength :: Int
        dummyAddressLength = 65
        -- Note: This is almost certainly too high. However, we are at liberty
        -- to overestimate the length of an address (which is safe). Therefore,
        -- we can choose a length that we know is greater than or equal to all
        -- address lengths.

        nullByte :: Word8
        nullByte = 0

-- | Includes just the parts of a transaction necessary to estimate its size.
--
-- In particular, this record type includes the minimal set of data needed for
-- the 'estimateTxCost' and 'estimateTxSize' functions to perform their
-- calculations, and nothing else.
--
-- The data included in 'TxSkeleton' is a subset of the data included in the
-- union of 'SelectionSkeleton' and 'TransactionCtx'.
--
data TxSkeleton = TxSkeleton
    { txMetadata :: !(Maybe TxMetadata)
    , txDelegationAction :: !(Maybe DelegationAction)
    , txRewardWithdrawal :: !Coin
    , txWitnessTag :: !TxWitnessTag
    , txInputCount :: !Int
    , txOutputs :: ![TxOut]
    , txChange :: ![Set AssetId]
    }
    deriving (Eq, Show)

-- | Constructs an empty transaction skeleton.
--
-- This may be used to estimate the size and cost of an empty transaction.
--
emptyTxSkeleton :: TxWitnessTag -> TxSkeleton
emptyTxSkeleton txWitnessTag = TxSkeleton
    { txMetadata = Nothing
    , txDelegationAction = Nothing
    , txRewardWithdrawal = Coin 0
    , txWitnessTag
    , txInputCount = 0
    , txOutputs = []
    , txChange = []
    }

-- | Constructs a transaction skeleton from wallet primitive types.
--
-- This function extracts a subset of the data included in 'SelectionSkeleton'
-- and 'TransactionCtx'.
--
mkTxSkeleton
    :: TxWitnessTag
    -> TransactionCtx
    -> SelectionSkeleton
    -> TxSkeleton
mkTxSkeleton witness context skeleton = TxSkeleton
    { txMetadata = view #txMetadata context
    , txDelegationAction = view #txDelegationAction context
    , txRewardWithdrawal = withdrawalToCoin $ view #txWithdrawal context
    , txWitnessTag = witness
    , txInputCount = view #skeletonInputCount skeleton
    , txOutputs = view #skeletonOutputs skeleton
    , txChange = view #skeletonChange skeleton
    }

-- | Estimates the final cost of a transaction based on its skeleton.
--
estimateTxCost :: ProtocolParameters -> TxSkeleton -> Coin
estimateTxCost pp skeleton =
    computeFee $ estimateTxSize skeleton
  where
    LinearFee (Quantity a) (Quantity b) = getFeePolicy $ txParameters pp

    computeFee :: TxSize -> Coin
    computeFee (TxSize size) =
        Coin $ ceiling (a + b * fromIntegral size)

-- | Estimates the final size of a transaction based on its skeleton.
--
-- This function uses the upper bounds of CBOR serialized objects as the basis
-- for many of its calculations. The following document is used as a reference:
--
-- https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley/chain-and-ledger/shelley-spec-ledger-test/cddl-files/shelley.cddl
--
estimateTxSize :: TxSkeleton -> TxSize
estimateTxSize skeleton =
    TxSize $ fromIntegral sizeOf_Transaction
  where
    TxSkeleton
        { txMetadata
        , txDelegationAction
        , txRewardWithdrawal
        , txWitnessTag
        , txInputCount
        , txOutputs
        , txChange
        } = skeleton

    numberOf_Inputs
        = fromIntegral txInputCount

    numberOf_CertificateSignatures
        = maybe 0 (const 1) txDelegationAction

    numberOf_Withdrawals
        = if txRewardWithdrawal > Coin 0 then 1 else 0

    numberOf_VkeyWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> 0
            TxWitnessShelleyUTxO ->
                numberOf_Inputs
                + numberOf_Withdrawals
                + numberOf_CertificateSignatures

    numberOf_BootstrapWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> numberOf_Inputs
            TxWitnessShelleyUTxO -> 0

    -- transaction =
    --   [ transaction_body
    --   , transaction_witness_set
    --   , transaction_metadata / null
    --   ]
    sizeOf_Transaction
        = sizeOf_SmallArray
        + sizeOf_TransactionBody
        + sizeOf_WitnessSet
        + sizeOf_Metadata

    -- transaction_body =
    --   { 0 : set<transaction_input>
    --   , 1 : [* transaction_output]
    --   , 2 : coin ; fee
    --   , 3 : uint ; ttl
    --   , ? 4 : [* certificate]
    --   , ? 5 : withdrawals
    --   , ? 6 : update
    --   , ? 7 : metadata_hash
    --   }
    sizeOf_TransactionBody
        = sizeOf_SmallMap
        + sizeOf_Inputs
        + sizeOf_Outputs
        + sizeOf_Fee
        + sizeOf_Ttl
        + sizeOf_Certificates
        + sizeOf_Withdrawals
        + sizeOf_Update
        + sizeOf_MetadataHash
      where
        -- 0 => set<transaction_input>
        sizeOf_Inputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + sizeOf_Input * numberOf_Inputs

        -- 1 => [* transaction_output]
        sizeOf_Outputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + F.sum (sizeOf_Output <$> txOutputs)
            + F.sum (sizeOf_ChangeOutput <$> txChange)

        -- 2 => fee
        sizeOf_Fee
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- 3 => ttl
        sizeOf_Ttl
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- ?4 => [* certificates ]
        sizeOf_Certificates
            = case txDelegationAction of
                Nothing ->
                    0
                Just RegisterKeyAndJoin{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeRegistration
                    + sizeOf_StakeDelegation
                Just Join{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDelegation
                Just Quit{} ->
                    sizeOf_SmallUInt
                    + sizeOf_SmallArray
                    + sizeOf_StakeDeregistration

        -- ?5 => withdrawals
        sizeOf_Withdrawals
            = (if numberOf_Withdrawals > 0
                then sizeOf_SmallUInt + sizeOf_SmallMap
                else 0)
            + sizeOf_Withdrawal * numberOf_Withdrawals

        -- ?6 => update
        sizeOf_Update
            = 0 -- Assuming no updates is running through cardano-wallet

        -- ?7 => metadata_hash
        sizeOf_MetadataHash
            = maybe 0 (const (sizeOf_SmallUInt + sizeOf_Hash32)) txMetadata

    -- For metadata, we can't choose a reasonable upper bound, so it's easier to
    -- measure the serialize data since we have it anyway. When it's "empty",
    -- metadata are represented by a special "null byte" in CBOR `F6`.
    sizeOf_Metadata
        = maybe 1 (toInteger . BS.length . serialiseToCBOR) txMetadata

    -- transaction_input =
    --   [ transaction_id : $hash32
    --   , index : uint
    --   ]
    sizeOf_Input
        = sizeOf_SmallArray
        + sizeOf_Hash32
        + sizeOf_UInt

    -- transaction_output =
    --   [address, amount : value]
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_Output TxOut {address, tokens}
        = sizeOf_SmallArray
        + sizeOf_Address address
        + sizeOf_SmallArray
        + sizeOf_Coin (TokenBundle.getCoin tokens)
        + F.foldl' (\t -> (t +) . sizeOf_NativeAsset) 0
            (TokenBundle.getAssets tokens)

    -- transaction_output =
    --   [address, amount : value]
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_ChangeOutput xs
        = sizeOf_SmallArray
        + sizeOf_ChangeAddress
        + sizeOf_SmallArray
        + sizeOf_LargeUInt
        + F.foldl' (\t -> (t +) . sizeOf_NativeAsset) 0 xs

    -- stake_registration =
    --   (0, stake_credential)
    sizeOf_StakeRegistration
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential

    -- stake_deregistration =
    --   (1, stake_credential)
    sizeOf_StakeDeregistration
        = sizeOf_StakeRegistration

    -- stake_delegation =
    --   (2, stake_credential, pool_keyhash)
    sizeOf_StakeDelegation
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_StakeCredential
        + sizeOf_Hash28

    -- stake_credential =
    --   [  0, addr_keyhash
    --   // 1, scripthash
    --   ]
    sizeOf_StakeCredential
        = sizeOf_SmallArray
        + sizeOf_SmallUInt
        + sizeOf_Hash28

    -- We carry addresses already serialized, so it's a matter of measuring.
    sizeOf_Address addr
        = 2 + toInteger (BS.length (unAddress addr))

    -- For change address, we consider the worst-case scenario based on the
    -- given wallet scheme. Byron addresses are larger.
    --
    -- NOTE: we could do slightly better if we wanted to for Byron addresses and
    -- discriminate based on the network as well since testnet addresses are
    -- larger than mainnet ones. But meh.
    sizeOf_ChangeAddress
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> 85
            TxWitnessShelleyUTxO -> 59

    -- multiasset<a> = { * policy_id => { * asset_name => a } }
    -- policy_id = scripthash
    -- asset_name = bytes .size (0..32)
    sizeOf_NativeAsset AssetId{tokenName}
        = sizeOf_SmallMap -- NOTE: Assuming < 23 policies per output
        + sizeOf_Hash28
        + sizeOf_SmallMap -- NOTE: Assuming < 23 assets per policy
        + sizeOf_AssetName tokenName
        + sizeOf_LargeUInt

    -- asset_name = bytes .size (0..32)
    sizeOf_AssetName name
        = 2 + toInteger (BS.length $ unTokenName name)

    -- Coins can really vary so it's very punishing to always assign them the
    -- upper bound. They will typically be between 3 and 9 bytes (only 6 bytes
    -- difference, but on 20+ outputs, one starts feeling it).
    --
    -- So, for outputs, since we have the values, we can compute it accurately.
    sizeOf_Coin
        = toInteger
        . BS.length
        . CBOR.toStrictByteString
        . CBOR.encodeWord64
        . unCoin

    -- withdrawals =
    --   { * reward_account => coin }
    sizeOf_Withdrawal
        = sizeOf_Hash28
        + sizeOf_LargeUInt

    -- transaction_witness_set =
    --   { ?0 => [* vkeywitness ]
    --   , ?1 => [* multisig_script ]
    --   , ?2 => [* bootstrap_witness ]
    --   }
    sizeOf_WitnessSet
        = sizeOf_SmallMap
        + sizeOf_VKeyWitnesses
        + sizeOf_MultisigScript
        + sizeOf_BootstrapWitnesses
      where
        -- ?0 => [* vkeywitness ]
        sizeOf_VKeyWitnesses
            = (if numberOf_VkeyWitnesses > 0
                then sizeOf_Array + sizeOf_SmallUInt else 0)
            + sizeOf_VKeyWitness * numberOf_VkeyWitnesses

        -- ?1 => [* multisig_script ]
        sizeOf_MultisigScript
            = 0

        -- ?2 => [* bootstrap_witness ]
        sizeOf_BootstrapWitnesses
            = (if numberOf_BootstrapWitnesses > 0
                then sizeOf_Array + sizeOf_SmallUInt
                else 0)
            + sizeOf_BootstrapWitness * numberOf_BootstrapWitnesses

    -- vkeywitness =
    --  [ $vkey
    --  , $signature
    --  ]
    sizeOf_VKeyWitness
        = sizeOf_SmallArray
        + sizeOf_VKey
        + sizeOf_Signature

    -- bootstrap_witness =
    --  [ public_key : $vkey
    --  , signature  : $signature
    --  , chain_code : bytes .size 32
    --  , attributes : bytes
    --  ]
    sizeOf_BootstrapWitness
        = sizeOf_SmallArray
        + sizeOf_VKey
        + sizeOf_Signature
        + sizeOf_ChainCode
        + sizeOf_Attributes
      where
        sizeOf_ChainCode  = 34
        sizeOf_Attributes = 45 -- NOTE: could be smaller by ~34 for Icarus

    -- A Blake2b-224 hash, resulting in a 28-byte digest wrapped in CBOR, so
    -- with 2 bytes overhead (length <255, but length > 23)
    sizeOf_Hash28
        = 30

    -- A Blake2b-256 hash, resulting in a 32-byte digest wrapped in CBOR, so
    -- with 2 bytes overhead (length <255, but length > 23)
    sizeOf_Hash32
        = 34

    -- A 32-byte Ed25519 public key, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_VKey
        = 34

    -- A 64-byte Ed25519 signature, encoded as a CBOR-bytestring so with 2
    -- bytes overhead (length < 255, but length > 23)
    sizeOf_Signature
        = 66

    -- A CBOR UInt which is less than 23 in value fits on a single byte. Beyond,
    -- the first byte is used to encode the number of bytes necessary to encode
    -- the number itself, followed by the number itself.
    --
    -- When considering a 'UInt', we consider the worst case scenario only where
    -- the uint is encoded over 4 bytes, so up to 2^32 which is fine for most
    -- cases but coin values.
    sizeOf_SmallUInt = 1
    sizeOf_UInt = 5
    sizeOf_LargeUInt = 9

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each key-value pair (encoded as two concatenated CBOR elements).
    sizeOf_SmallMap = 1

    -- A CBOR array with less than 23 elements, fits on a single byte, followed
    -- by each elements. Otherwise, the length of the array is encoded first,
    -- very much like for UInt.
    --
    -- When considering an 'Array', we consider large scenarios where arrays can
    -- have up to 65536 elements.
    sizeOf_SmallArray = 1
    sizeOf_Array = 3

lookupPrivateKey
    :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> Address
    -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
lookupPrivateKey keyFrom addr =
    maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

withShelleyBasedEra
    :: forall a. ()
    => AnyCardanoEra
    -> (forall era. EraConstraints era => ShelleyBasedEra era -> Either ErrMkTx a)
    -> Either ErrMkTx a
withShelleyBasedEra era fn = case era of
    AnyCardanoEra ByronEra    -> Left $ ErrInvalidEra era
    AnyCardanoEra ShelleyEra  -> fn ShelleyBasedEraShelley
    AnyCardanoEra AllegraEra  -> fn ShelleyBasedEraAllegra
    AnyCardanoEra MaryEra     -> fn ShelleyBasedEraMary

-- FIXME: Make this a Allegra or Shelley transaction depending on the era we're
-- in. However, quoting Duncan:
--
--    "Yes, you can submit Shelley format transactions in the Allegra era.The
--    proper way to do that is marking it as a Shelley tx using GenTxShelley.
--    The improper way is to submit it as a GenTxAllegra. The latter should
--    still work since the binary formats are upwards compatible."
--
-- Which suggests that we may get away with Shelley-only transactions for now?
mkUnsignedTx
    :: forall era.  Cardano.IsCardanoEra era
    => ShelleyBasedEra era
    -> Cardano.SlotNo
    -> SelectionResult TxOut
    -> Maybe Cardano.TxMetadata
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> [Cardano.Certificate]
    -> Cardano.Lovelace
    -> Either ErrMkTx (Cardano.TxBody era)
mkUnsignedTx era ttl cs md wdrls certs fees =
    left toErrMkTx $ Cardano.makeTransactionBody $ Cardano.TxBodyContent
    { Cardano.txIns =
        (,Cardano.BuildTxWith (Cardano.KeyWitness Cardano.KeyWitnessForSpending))
        . toCardanoTxIn
        . fst <$> F.toList (inputsSelected cs)

    , Cardano.txOuts =
        toShelleyBasedTxOut <$> (outputsCovered cs ++ F.toList (changeGenerated cs))

    , Cardano.txWithdrawals =
        let
            wit = Cardano.BuildTxWith
                $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
        in
            Cardano.TxWithdrawals wdrlsSupported
                (map (\(key, coin) -> (key, coin, wit)) wdrls)

    , Cardano.txCertificates =
        let
            -- It seems that passing Map.empty here works just fine.
            witMap = Map.empty
            ctx = Cardano.BuildTxWith witMap
        in
            Cardano.TxCertificates certSupported certs ctx

    , Cardano.txFee = explicitFees fees

    , Cardano.txValidityRange =
        ( Cardano.TxValidityNoLowerBound
        , Cardano.TxValidityUpperBound txValidityUpperBoundSupported ttl
        )

    , Cardano.txMetadata =
        maybe
            Cardano.TxMetadataNone
            (Cardano.TxMetadataInEra metadataSupported)
            md

    , Cardano.txAuxScripts =
        Cardano.TxAuxScriptsNone

    , Cardano.txUpdateProposal =
        Cardano.TxUpdateProposalNone

    , Cardano.txMintValue =
        Cardano.TxMintNone
    }
  where
    toErrMkTx :: Cardano.TxBodyError era -> ErrMkTx
    toErrMkTx = ErrConstructedInvalidTx . T.pack . Cardano.displayError

    toShelleyBasedTxOut :: TxOut -> Cardano.TxOut era
    toShelleyBasedTxOut = case era of
        ShelleyBasedEraShelley -> toShelleyTxOut
        ShelleyBasedEraAllegra -> toAllegraTxOut
        ShelleyBasedEraMary -> toMaryTxOut

    metadataSupported :: Cardano.TxMetadataSupportedInEra era
    metadataSupported = case era of
        ShelleyBasedEraShelley -> Cardano.TxMetadataInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxMetadataInAllegraEra
        ShelleyBasedEraMary -> Cardano.TxMetadataInMaryEra

    certSupported :: Cardano.CertificatesSupportedInEra era
    certSupported = case era of
        ShelleyBasedEraShelley -> Cardano.CertificatesInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.CertificatesInAllegraEra
        ShelleyBasedEraMary    -> Cardano.CertificatesInMaryEra

    explicitFees :: Cardano.Lovelace -> Cardano.TxFee era
    explicitFees = case era of
        ShelleyBasedEraShelley -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAllegraEra
        ShelleyBasedEraMary    -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInMaryEra

    wdrlsSupported :: Cardano.WithdrawalsSupportedInEra era
    wdrlsSupported = case era of
        ShelleyBasedEraShelley -> Cardano.WithdrawalsInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.WithdrawalsInAllegraEra
        ShelleyBasedEraMary    -> Cardano.WithdrawalsInMaryEra

    txValidityUpperBoundSupported :: Cardano.ValidityUpperBoundSupportedInEra era
    txValidityUpperBoundSupported = case era of
        ShelleyBasedEraShelley -> Cardano.ValidityUpperBoundInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.ValidityUpperBoundInAllegraEra
        ShelleyBasedEraMary -> Cardano.ValidityUpperBoundInMaryEra

mkWithdrawals
    :: NetworkId
    -> RewardAccount
    -> Coin
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId acc amount
    | amount == Coin 0 = mempty
    | otherwise = [ (stakeAddress, toCardanoLovelace amount) ]
  where
    cred = toCardanoStakeCredential acc
    stakeAddress = Cardano.makeStakeAddress networkId cred

mkShelleyWitness
    :: IsShelleyBasedEra era
    => Cardano.TxBody era
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness era
mkShelleyWitness body key =
    Cardano.makeShelleyKeyWitness body (unencrypt key)
  where
    unencrypt (xprv, pwd) = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

mkByronWitness
    :: forall era. (EraConstraints era)
    => Cardano.TxBody era
    -> Cardano.NetworkId
    -> Address
    -> (XPrv, Passphrase "encryption")
    -> Cardano.KeyWitness era
mkByronWitness (Cardano.ShelleyTxBody era body _scripts _auxData) nw addr encryptedKey =
    Cardano.ShelleyBootstrapWitness era $
        SL.makeBootstrapWitness txHash (unencrypt encryptedKey) addrAttr
  where
    txHash = Crypto.castHash $ Crypto.hashWith serialize' body

    unencrypt (xprv, pwd) = CC.SigningKey
        $ Crypto.HD.xPrvChangePass pwd BS.empty xprv

    addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
        (toHDPayloadAddress addr)
        (Byron.toByronNetworkMagic nw)
