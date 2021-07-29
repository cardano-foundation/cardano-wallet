{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
    , _decodeSealedTx
    , _estimateMaxNumberOfInputs
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyKeyWitness
    , mkTxSkeleton
    , toCardanoTxBody
    , txConstraints
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Address.Script
    ( KeyHash, Script (..) )
import Cardano.Api
    ( AnyCardanoEra (..)
    , InAnyCardanoEra (..)
    , IsShelleyBasedEra (..)
    , NetworkId
    , SerialiseAsCBOR (..)
    , ShelleyBasedEra (..)
    , cardanoEraStyle
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), RewardAccount (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, toRewardAccountRaw )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionLimit (..)
    , SelectionLimitOf (..)
    , SelectionResult (changeGenerated, inputsSelected, outputsCovered)
    , SelectionSkeleton (..)
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..), ProtocolParameters (..), TxParameters (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin, subtractCoin, sumCoins )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , TxConstraints (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxSize (..)
    , sealedTxFromCardano'
    , txOutCoin
    , txSizeDistance
    )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoTx
    , fromShelleyTxId
    , rewardAccountFromStakeAddress
    , rewardAccountFromStakeCredential
    , toAllegraTxOut
    , toAlonzoTxOut
    , toCardanoLovelace
    , toCardanoStakeCredential
    , toCardanoTxIn
    , toMaryTxOut
    , toShelleyTxOut
    , toStakeKeyDeregCert
    , toStakeKeyRegCert
    , toStakePoolDlgCert
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( computeMinimumAdaQuantity )
import Cardano.Wallet.Transaction
    ( DecryptedSigningKey (..)
    , DelegationAction (..)
    , ErrMkTransaction (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)
    , ErrSelectionCriteria (..)
    , SignTransactionKeyStore (..)
    , SignTransactionResult (..)
    , SignTransactionWitness (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , keyStoreLookup
    , keyStoreLookupWithdrawal
    , withdrawalToCoin
    )
import Data.Bifunctor
    ( Bifunctor (..), bimap )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Kind
    ( Type )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( Buildable (..), pretty )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Text as T


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
    }

data TxWitnessTag
    = TxWitnessByronUTxO WalletStyle
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

data WalletStyle
    = Icarus
    | Byron
    deriving (Show, Eq)

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

_mkSignedTransaction
    :: forall k.
        ( TxWitnessTagFor k
        , WalletKey k
        )
    => Cardano.NetworkId
    -> SignTransactionKeyStore (k 'AddressK XPrv)
    -- ^ Key store
    -> SealedTx
    -- ^ The unsigned transaction
    -> SignTransactionResult (k 'AddressK XPrv) (InAnyCardanoEra Cardano.KeyWitness) SealedTx
_mkSignedTransaction networkId keyStore sealed = case view #cardanoTx sealed of
    InAnyCardanoEra era tx -> case cardanoEraStyle era of
        Cardano.LegacyByronEra ->
            SignTransactionResult sealed [] [] []
        Cardano.ShelleyBasedEra _ ->
            bimap (InAnyCardanoEra era) sealedTxFromCardano' $
            mkSignedShelleyTransaction networkId keyStore tx

mkSignedShelleyTransaction
    :: forall k era.
        ( TxWitnessTagFor k
        , WalletKey k
        , IsShelleyBasedEra era
        )
    => Cardano.NetworkId
    -> SignTransactionKeyStore (k 'AddressK XPrv)
    -- ^ Credentials for signing.
    -> Cardano.Tx era
    -- ^ The transaction to be signed. May already have some witnesses.
    -> SignTransactionResult (k 'AddressK XPrv) (Cardano.KeyWitness era) (Cardano.Tx era)
mkSignedShelleyTransaction networkId keyStore tx = SignTransactionResult
    { tx = Cardano.makeSignedTransaction
        (mapMaybe witness addrWits ++ map snd (wdrlWits ++ certWits))
        body
    , addressWitnesses = addrWits
    , withdrawalWitnesses = wdrlWits
    , certificateWitnesses = certWits
    }
  where
    body@(Cardano.TxBody txBodyContent) = Cardano.getTxBody tx

    ----------------------------------------------------------------------------
    -- Looking up keys from the store and creating key witnesses.
    --

    mkWit (fmap getRawKey -> sk) addr = case txWitnessTagFor @k of
        TxWitnessShelleyUTxO -> mkShelleyKeyWitness body sk
        TxWitnessByronUTxO Icarus -> mkByronWitness body networkId Nothing sk
        TxWitnessByronUTxO Byron -> mkByronWitness body networkId (Just addr) sk

    mkStakeWit = keyStoreLookupWithdrawal keyStore (mkShelleyKeyWitness body)

    ----------------------------------------------------------------------------
    -- Payment address witnesses

    selectedInputs =
        [ TxIn (fromShelleyTxId $ Cardano.toShelleyTxId txid) (fromIntegral ix)
        | Cardano.TxIn txid (Cardano.TxIx ix) <- fst <$> Cardano.txIns txBodyContent ]

    addrWits = map (keyStoreLookup keyStore mkWit) selectedInputs

    ----------------------------------------------------------------------------
    -- Withdrawal witnesses

    wdrls = case Cardano.txWithdrawals txBodyContent of
        Cardano.TxWithdrawalsNone -> []
        Cardano.TxWithdrawals _era ws -> case txWitnessTagFor @k of
            TxWitnessShelleyUTxO ->
                [ rewardAccountFromStakeAddress addr
                | (addr, _lovelace, _buildTx) <- ws ]
            TxWitnessByronUTxO{} -> []


    wdrlWits = mapMaybe mkStakeWit wdrls

    ----------------------------------------------------------------------------
    -- Certificate witnesses (register stake key/join/quit stake pool)

    certs = mapMaybe
          (fmap rewardAccountFromStakeCredential . certToStakeCredential)
          cardanoCerts

    cardanoCerts = case Cardano.txCertificates txBodyContent of
        Cardano.TxCertificatesNone -> []
        Cardano.TxCertificates _era cs _buildTx -> cs

    certToStakeCredential = \case
        Cardano.StakeAddressRegistrationCertificate stakeCredential ->
            Just stakeCredential
        Cardano.StakeAddressDeregistrationCertificate stakeCredential ->
            Just stakeCredential
        Cardano.StakeAddressDelegationCertificate stakeCredential _poolId ->
            Just stakeCredential
        _ ->
            Nothing

    certWits = case txWitnessTagFor @k of
        TxWitnessShelleyUTxO -> mapMaybe mkStakeWit certs
        TxWitnessByronUTxO{} -> []

newTransactionLayer
    :: forall k.
        ( TxWitnessTagFor k
        , WalletKey k
        )
    => NetworkId
    -> TransactionLayer k SealedTx
newTransactionLayer networkId = TransactionLayer
    { mkTransactionBody = _mkTransactionBody networkId
    , mkSignedTransaction = \keyStore -> first (const ()) . _mkSignedTransaction networkId keyStore

    , calcMinimumCost = \pp ctx skeleton ->
        estimateTxCost pp $
        mkTxSkeleton (txWitnessTagFor @k) ctx skeleton

    , computeSelectionLimit = \pp ctx outputsToCover ->
        let txMaxSize = getTxMaxSize $ txParameters pp in
        MaximumInputLimit $
            _estimateMaxNumberOfInputs @k txMaxSize ctx outputsToCover

    , tokenBundleSizeAssessor =
        Compatibility.tokenBundleSizeAssessor

    , constraints = \pp -> txConstraints pp (txWitnessTagFor @k)

    , decodeTx = _decodeSealedTx
    }

_decodeSealedTx :: SealedTx -> Tx
_decodeSealedTx (cardanoTx -> InAnyCardanoEra _era tx) = fromCardanoTx tx

_mkTransactionBody
    :: Cardano.NetworkId
    -> (AnyCardanoEra, ProtocolParameters)
    -> Maybe RewardAccount
    -> TransactionCtx
    -> SelectionResult TxOut
    -> Either ErrMkTransaction SealedTx
_mkTransactionBody networkId (e@(AnyCardanoEra era), pp) rewardAcct ctx cs =
     case cardanoEraStyle era of
        Cardano.LegacyByronEra ->
            Left $ ErrMkTransactionInvalidEra e
        Cardano.ShelleyBasedEra era' ->
            sealedTxFromCardano' . Cardano.makeSignedTransaction mempty <$>
                mkShelleyTransactionBody networkId rewardAcct pp ctx cs era'

mkShelleyTransactionBody
    :: forall era. Cardano.IsCardanoEra era
    => Cardano.NetworkId
    -> Maybe RewardAccount
    -> ProtocolParameters
    -> TransactionCtx
    -> SelectionResult TxOut
    -> ShelleyBasedEra era
    -> Either ErrMkTransaction (Cardano.TxBody era)
mkShelleyTransactionBody networkId rewardAcct pp ctx cs era =
    toCardanoTxBody era payload ttl wdrls cs fee
  where
    payload = TxPayload (view #txMetadata ctx) certs
    ttl = txTimeToLive ctx
    wdrl = withdrawalToCoin $ view #txWithdrawal ctx
    wdrls = maybe [] (mkWithdrawals networkId wdrl) rewardAcct

    (deposits, certs) = case (view #txDelegationActions ctx, rewardAcct) of
        (ds, Just a) -> (map getDeposit ds, map (mkDelegationCertificate a) ds)
        _ -> ([], [])

    feeGap = selectionDelta txOutCoin cs
    fee = unsafeSubtractCoin cs feeGap (sumCoins deposits)

    getDeposit = \case
        RegisterKey -> Coin 0
        Join _ -> stakeKeyDeposit pp
        Quit -> Coin 0

    unsafeSubtractCoin
        :: (HasCallStack, Buildable x) => x -> Coin -> Coin -> Coin
    unsafeSubtractCoin x a b = fromMaybe (error err) (a `subtractCoin` b)
      where
        err = unlines
            [ "unsafeSubtractCoin: got a negative value. Tried to subtract "
            <> show b <> " from " <> show a <> "."
            , "In the context of: "
            , pretty x
            ]

mkDelegationCertificate
    :: RewardAccount
        -- Reward account public key hash
    -> DelegationAction
        -- Pool Id to which we're planning to delegate
    -> Cardano.Certificate
mkDelegationCertificate rewardAcct = \case
   Join poolId -> toStakePoolDlgCert rewardAcct poolId
   RegisterKey -> toStakeKeyRegCert rewardAcct
   Quit -> toStakeKeyDeregCert rewardAcct

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

dummySkeleton :: Int -> [TxOut] -> SelectionSkeleton
dummySkeleton inputCount outputs = SelectionSkeleton
    { skeletonInputCount =
        inputCount
    , skeletonOutputs =
        outputs
    , skeletonChange =
        TokenBundle.getAssets . view #tokens <$> outputs
    }

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
        (txOutputSize mempty)
        (view
            (#txParameters . #getTokenBundleMaxSize . #unTokenBundleMaxSize)
            protocolParams)

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
    , txDelegationActions :: ![DelegationAction]
    , txRewardWithdrawal :: !Coin
    , txWitnessTag :: !TxWitnessTag
    , txInputCount :: !Int
    , txOutputs :: ![TxOut]
    , txChange :: ![Set AssetId]
    , txScripts :: [Script KeyHash]
    , txMintBurnAssets :: [AssetId]
    -- ^ Assets that have been both minted and burned, or minted or burned
    -- multiple times, will appear multiple times in this list, once for each
    -- mint or burn. For example if the caller "mints 3" of asset id "A", and
    -- "burns 3" of asset id "A", "A" will appear twice in the list.
    }
    deriving (Eq, Show)

-- | Constructs an empty transaction skeleton.
--
-- This may be used to estimate the size and cost of an empty transaction.
--
emptyTxSkeleton :: TxWitnessTag -> TxSkeleton
emptyTxSkeleton txWitnessTag = TxSkeleton
    { txMetadata = Nothing
    , txDelegationActions = []
    , txRewardWithdrawal = Coin 0
    , txWitnessTag
    , txInputCount = 0
    , txOutputs = []
    , txChange = []
    , txScripts = []
    , txMintBurnAssets = []
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
    , txDelegationActions = view #txDelegationActions context
    , txRewardWithdrawal = withdrawalToCoin $ view #txWithdrawal context
    , txWitnessTag = witness
    , txInputCount = view #skeletonInputCount skeleton
    , txOutputs = view #skeletonOutputs skeleton
    , txChange = view #skeletonChange skeleton
    -- Until we actually support minting and burning, leave these as empty.
    , txScripts = []
    , txMintBurnAssets = []
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
-- https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley-ma/shelley-ma-test/cddl-files/shelley-ma.cddl
--
estimateTxSize :: TxSkeleton -> TxSize
estimateTxSize skeleton =
    TxSize $ fromIntegral sizeOf_Transaction
  where
    TxSkeleton
        { txMetadata
        , txDelegationActions
        , txRewardWithdrawal
        , txWitnessTag
        , txInputCount
        , txOutputs
        , txChange
        , txScripts
        , txMintBurnAssets
        } = skeleton

    numberOf_Inputs
        = fromIntegral txInputCount

    numberOf_CertificateSignatures
        = if null txDelegationActions then 0 else 1

    numberOf_Withdrawals
        = if txRewardWithdrawal > Coin 0 then 1 else 0

    -- Total number of signatures the scripts require
    numberOf_ScriptVkeyWitnesses
        = sumVia scriptRequiredKeySigs txScripts

    scriptRequiredKeySigs :: Num num => Script KeyHash -> num
    scriptRequiredKeySigs = \case
        RequireSignatureOf _ ->
            1
        RequireAllOf ss ->
            sumVia scriptRequiredKeySigs ss
        RequireAnyOf ss ->
            sumVia scriptRequiredKeySigs ss
        ActiveFromSlot _ ->
            0
        ActiveUntilSlot _ ->
            0
        RequireSomeOf _ ss ->
            -- We don't know how many the user will sign with, so we just assume
            -- the worst case of "signs with all".
            sumVia scriptRequiredKeySigs ss

    numberOf_VkeyWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO{} -> 0
            TxWitnessShelleyUTxO ->
                numberOf_Inputs
                + numberOf_Withdrawals
                + numberOf_CertificateSignatures
                + numberOf_ScriptVkeyWitnesses

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
    --   , ? 8 : uint ; validity interval start
    --   , ? 9 : mint
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
        + sizeOf_ValidityIntervalStart
        + sumVia sizeOf_Mint txMintBurnAssets
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
            = sum (map sizeOf_Certificate txDelegationActions)
        sizeOf_Certificate = \case
            RegisterKey ->
                sizeOf_StakeRegistration
            Join{} ->
                sizeOf_SmallUInt
                + sizeOf_SmallArray
                + sizeOf_StakeDelegation
            Quit{} ->
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

        -- ?8 => uint ; validity interval start
        sizeOf_ValidityIntervalStart
            = sizeOf_UInt

        -- ?9 => mint = multiasset<int64>
        -- mint = multiasset<int64>
        sizeOf_Mint AssetId{tokenName}
          = sizeOf_MultiAsset sizeOf_Int64 tokenName

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
        + sumVia sizeOf_NativeAsset (TokenBundle.getAssets tokens)

    -- transaction_output =
    --   [address, amount : value]
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_ChangeOutput xs
        = sizeOf_SmallArray
        + sizeOf_ChangeAddress
        + sizeOf_SmallArray
        + sizeOf_LargeUInt
        + sumVia sizeOf_NativeAsset xs

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

    -- value = coin / [coin,multiasset<uint>]
    -- We consider "native asset" to just be the "multiasset<uint>" part of the
    -- above, hence why we don't also include the size of the coin. Where this
    -- is used, the size of the coin and array are are added too.
    sizeOf_NativeAsset AssetId{tokenName}
        = sizeOf_MultiAsset sizeOf_LargeUInt tokenName

    -- multiasset<a> = { * policy_id => { * asset_name => a } }
    -- policy_id = scripthash
    -- asset_name = bytes .size (0..32)
    sizeOf_MultiAsset sizeOf_a name
      = sizeOf_SmallMap -- NOTE: Assuming < 23 policies per output
      + sizeOf_Hash28
      + sizeOf_SmallMap -- NOTE: Assuming < 23 assets per policy
      + sizeOf_AssetName name
      + sizeOf_a

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
        + sizeOf_NativeScripts txScripts
        + sizeOf_BootstrapWitnesses
      where
        -- ?0 => [* vkeywitness ]
        sizeOf_VKeyWitnesses
            = (if numberOf_VkeyWitnesses > 0
                then sizeOf_Array + sizeOf_SmallUInt else 0)
            + sizeOf_VKeyWitness * numberOf_VkeyWitnesses

        -- ?1 => [* native_script ]
        sizeOf_NativeScripts []
            = 0
        sizeOf_NativeScripts ss
            = sizeOf_Array
            + sizeOf_SmallUInt
            + sumVia sizeOf_NativeScript ss

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

    -- native_script =
    --   [ script_pubkey      = (0, addr_keyhash)
    --   // script_all        = (1, [ * native_script ])
    --   // script_any        = (2, [ * native_script ])
    --   // script_n_of_k     = (3, n: uint, [ * native_script ])
    --   // invalid_before    = (4, uint)
    --      ; Timelock validity intervals are half-open intervals [a, b).
    --      ; This field specifies the left (included) endpoint a.
    --   // invalid_hereafter = (5, uint)
    --      ; Timelock validity intervals are half-open intervals [a, b).
    --      ; This field specifies the right (excluded) endpoint b.
    --   ]
    sizeOf_NativeScript = \case
        RequireSignatureOf _ ->
            sizeOf_SmallUInt + sizeOf_Hash28
        RequireAllOf ss ->
            sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
        RequireAnyOf ss ->
            sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
        RequireSomeOf _ ss ->
            sizeOf_SmallUInt
                + sizeOf_UInt
                + sizeOf_Array
                + sumVia sizeOf_NativeScript ss
        ActiveFromSlot _ ->
            sizeOf_SmallUInt + sizeOf_UInt
        ActiveUntilSlot _ ->
            sizeOf_SmallUInt + sizeOf_UInt

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

    -- A CBOR Int which is less than 23 in value fits on a single byte. Beyond,
    -- the first byte is used to encode the number of bytes necessary to encode
    -- the number, followed by the number itself. In this case, 8 bytes are used
    -- to encode an int64, plus one byte to encode the number of bytes
    -- necessary.
    sizeOf_Int64 = 9

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

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0

-- FIXME: Make this a Allegra or Shelley transaction depending on the era we're
-- in. However, quoting Duncan:
--
--    "Yes, you can submit Shelley format transactions in the Allegra era.The
--    proper way to do that is marking it as a Shelley tx using GenTxShelley.
--    The improper way is to submit it as a GenTxAllegra. The latter should
--    still work since the binary formats are upwards compatible."
--
-- Which suggests that we may get away with Shelley-only transactions for now?
toCardanoTxBody
    :: forall era. Cardano.IsCardanoEra era
    => ShelleyBasedEra era
    -> TxPayload era
    -> Cardano.SlotNo
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
    -> SelectionResult TxOut
    -> Coin
    -> Either ErrMkTransaction (Cardano.TxBody era)
toCardanoTxBody era (TxPayload md certs) ttl wdrl cs fee =
    -- TODO: It would be nice to have a non-validating variant of makeTransactionBody.
    first toErr $ Cardano.makeTransactionBody $ Cardano.TxBodyContent
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
                (map (\(key, coin) -> (key, coin, wit)) wdrl)

    , txInsCollateral =
        -- TODO: [ADP-957] Support collateral.
        Cardano.TxInsCollateralNone

    , txProtocolParams =
        -- TODO: [ADP-1058] We presumably need to provide the protocol params if
        -- our tx uses scripts?
        Cardano.BuildTxWith Nothing

    , txScriptValidity =
        Cardano.TxScriptValidityNone

    , txExtraScriptData = Cardano.BuildTxWith Cardano.TxExtraScriptDataNone

    , txExtraKeyWits = Cardano.TxExtraKeyWitnessesNone

    , Cardano.txCertificates =
        let
            -- It seems that passing Map.empty here works just fine.
            witMap = Map.empty
            ctx = Cardano.BuildTxWith witMap
        in
            Cardano.TxCertificates certSupported certs ctx

    , Cardano.txFee = explicitFees (toCardanoLovelace fee)

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
    toErr :: Cardano.TxBodyError -> ErrMkTransaction
    toErr = ErrMkTransactionTxBodyError . T.pack . Cardano.displayError

    toShelleyBasedTxOut :: TxOut -> Cardano.TxOut era
    toShelleyBasedTxOut = case era of
        ShelleyBasedEraShelley -> toShelleyTxOut
        ShelleyBasedEraAllegra -> toAllegraTxOut
        ShelleyBasedEraMary -> toMaryTxOut
        ShelleyBasedEraAlonzo -> toAlonzoTxOut

    metadataSupported :: Cardano.TxMetadataSupportedInEra era
    metadataSupported = case era of
        ShelleyBasedEraShelley -> Cardano.TxMetadataInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxMetadataInAllegraEra
        ShelleyBasedEraMary -> Cardano.TxMetadataInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.TxMetadataInAlonzoEra

    certSupported :: Cardano.CertificatesSupportedInEra era
    certSupported = case era of
        ShelleyBasedEraShelley -> Cardano.CertificatesInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.CertificatesInAllegraEra
        ShelleyBasedEraMary    -> Cardano.CertificatesInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.CertificatesInAlonzoEra

    explicitFees :: Cardano.Lovelace -> Cardano.TxFee era
    explicitFees = case era of
        ShelleyBasedEraShelley -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAllegraEra
        ShelleyBasedEraMary    -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.TxFeeExplicit Cardano.TxFeesExplicitInAlonzoEra

    wdrlsSupported :: Cardano.WithdrawalsSupportedInEra era
    wdrlsSupported = case era of
        ShelleyBasedEraShelley -> Cardano.WithdrawalsInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.WithdrawalsInAllegraEra
        ShelleyBasedEraMary    -> Cardano.WithdrawalsInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.WithdrawalsInAlonzoEra

    txValidityUpperBoundSupported :: Cardano.ValidityUpperBoundSupportedInEra era
    txValidityUpperBoundSupported = case era of
        ShelleyBasedEraShelley -> Cardano.ValidityUpperBoundInShelleyEra
        ShelleyBasedEraAllegra -> Cardano.ValidityUpperBoundInAllegraEra
        ShelleyBasedEraMary -> Cardano.ValidityUpperBoundInMaryEra
        ShelleyBasedEraAlonzo -> Cardano.ValidityUpperBoundInAlonzoEra

mkWithdrawals
    :: NetworkId
    -> Coin
    -> RewardAccount
    -> [(Cardano.StakeAddress, Cardano.Lovelace)]
mkWithdrawals networkId amount acc
    | amount == Coin 0 = mempty
    | otherwise = [ (stakeAddress, toCardanoLovelace amount) ]
  where
    cred = toCardanoStakeCredential acc
    stakeAddress = Cardano.makeStakeAddress networkId cred

decryptSigningKey :: DecryptedSigningKey XPrv -> XPrv
decryptSigningKey (DecryptedSigningKey sk pwd) =
    Crypto.HD.xPrvChangePass pwd BS.empty sk

mkShelleyKeyWitness
    :: IsShelleyBasedEra era
    => Cardano.TxBody era
    -> DecryptedSigningKey XPrv
    -> Cardano.KeyWitness era
mkShelleyKeyWitness body key =
    Cardano.makeShelleyKeyWitness body key'
  where
    key' = Cardano.WitnessPaymentExtendedKey
        $ Cardano.PaymentExtendedSigningKey
        $ decryptSigningKey key

mkByronWitness
    :: forall era. IsShelleyBasedEra era
    => Cardano.TxBody era
    -> Cardano.NetworkId
    -> Maybe Address
    -> DecryptedSigningKey XPrv
    -> Cardano.KeyWitness era
mkByronWitness body net addr key =
    Cardano.makeShelleyBootstrapWitness addrNet body key'
  where
    addrNet = maybe (Byron.WitnessNetworkId net) Byron.WitnessByronAddress (addr >>= toCardanoByronAddr)
    key' = Cardano.ByronSigningKey $ CC.SigningKey $ decryptSigningKey key

toCardanoByronAddr :: Address -> Maybe (Cardano.Address Cardano.ByronAddr)
toCardanoByronAddr = Cardano.deserialiseFromRawBytes Cardano.AsByronAddress . unAddress
