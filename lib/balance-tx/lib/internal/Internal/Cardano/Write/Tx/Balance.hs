{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use ||" -}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Internal.Cardano.Write.Tx.Balance
    (
    -- * Balancing transactions
      balanceTransaction
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrUpdateSealedTx (..)
    , ErrAssignRedeemers (..)

    -- * Change addresses
    , ChangeAddressGen (..)
    , assignChangeAddresses

    -- * Partial transactions
    , PartialTx (..)
    , Redeemer (..)

    -- * UTxO assumptions
    , UTxOAssumptions (..)

    -- * UTxO indices
    , UTxOIndex
    , constructUTxOIndex

    -- * Utilities
    , posAndNegFromCardanoApiValue
    , fromWalletUTxO
    , toWalletUTxO

    -- ** updateTx
    , TxUpdate (..)
    , noTxUpdate
    , updateTx
    , TxFeeUpdate (..)

    -- ** distributeSurplus
    , distributeSurplus
    , distributeSurplusDelta
    , sizeOfCoin
    , maximumCostOfIncreasingCoin
    , costOfIncreasingCoin
    , TxFeeAndChange (..)
    , ErrMoreSurplusNeeded (..)
    )
    where

import Prelude

import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..)
    )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection
    )
import Cardano.Ledger.Alonzo.Core
    ( ppCollateralPercentageL
    , ppMaxCollateralInputsL
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript (..)
    )
import Cardano.Ledger.Api
    ( BabbageEraTxBody (totalCollateralTxBodyL)
    , Script
    , ScriptHash
    , StandardCrypto
    , addrTxWitsL
    , bodyTxL
    , bootAddrTxWitsL
    , collateralInputsTxBodyL
    , collateralReturnTxBodyL
    , feeTxBodyL
    , hashScript
    , inputsTxBodyL
    , outputsTxBodyL
    , ppMaxTxSizeL
    , scriptTxWitsL
    , witsTxL
    )
import Cardano.Ledger.BaseTypes
    ( StrictMaybe (..)
    )
import Cardano.Ledger.UTxO
    ( txinLookup
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import Control.Arrow
    ( left
    )
import Control.Monad
    ( forM
    , unless
    , when
    )
import Control.Monad.Random
    ( MonadRandom
    , evalRand
    )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT)
    , catchE
    , except
    , runExceptT
    , throwE
    , withExceptT
    )
import Control.Monad.Trans.State
    ( runState
    , state
    )
import Data.Bifunctor
    ( bimap
    , second
    )
import Data.Bits
    ( Bits
    )
import Data.Either
    ( lefts
    , partitionEithers
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCastMaybe
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Semigroup.Cancellative
    ( Reductive ((</>))
    )
import Data.Type.Equality
    ( (:~:) (..)
    , testEquality
    )
import Fmt
    ( Buildable
    , Builder
    , blockListF'
    , build
    , nameF
    , pretty
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Internal.Cardano.Write.ProtocolParameters
    ( ProtocolParameters (..)
    )
import Internal.Cardano.Write.Tx
    ( Address
    , AssetName
    , Coin (..)
    , FeePerByte (..)
    , IsRecentEra (..)
    , KeyWitnessCount (..)
    , PParams
    , PolicyId
    , RecentEra (..)
    , RecentEraConstraints
    , RecentEraLedgerConstraints
    , ShelleyLedgerEra
    , Tx
    , TxBody
    , TxIn
    , TxOut
    , UTxO (..)
    , Value
    , computeMinimumCoinForTxOut
    , evaluateMinimumFee
    , evaluateTransactionBalance
    , feeOfBytes
    , fromCardanoApiTx
    , getFeePerByte
    , isBelowMinimumCoinForTxOut
    , maxScriptExecutionCost
    , modifyLedgerBody
    , modifyTxOutCoin
    , outputs
    , toCardanoApiTx
    , txBody
    , withConstraints
    )
import Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionCollateralError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (change)
    , SelectionParams (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , WalletSelectionContext
    , WalletUTxO (..)
    , performSelection
    , toExternalUTxOMap
    , toInternalUTxOMap
    )
import Internal.Cardano.Write.Tx.Balance.TokenBundleSize
    ( mkTokenBundleSizeAssessor
    )
import Internal.Cardano.Write.Tx.Redeemers
    ( ErrAssignRedeemers (..)
    , Redeemer (..)
    , assignScriptRedeemers
    )
import Internal.Cardano.Write.Tx.Sign
    ( estimateKeyWitnessCount
    , estimateSignedTxSize
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( TxSkeleton (..)
    , assumedTxWitnessTag
    , estimateTxCost
    )
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    )
import Internal.Cardano.Write.UTxOAssumptions
    ( UTxOAssumptions (..)
    , assumedInputScriptTemplate
    )
import Numeric.Natural
    ( Natural
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenFromSeed
    , stdGenSeed
    )
import Text.Pretty.Simple
    ( pShow
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Byron as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Wallet.Primitive.Types.Address as W
    ( Address
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W.TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
    ( AssetId (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
    ( TokenQuantity (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W.TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (..)
    )
import qualified Cardano.Wallet.Primitive.Types.UTxO as W.UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
    ( UTxO (..)
    )
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as Convert
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

-- | Helper wrapper type for the sake of logging.
data BuildableInAnyEra tx = forall era.
    ( Eq (tx era)
    , Show (tx era)
    , Buildable (tx era)
    ) => BuildableInAnyEra (CardanoApi.CardanoEra era) (tx era)

instance Show (BuildableInAnyEra a) where
    show (BuildableInAnyEra _ a) = show a

instance Eq (BuildableInAnyEra a) where
    BuildableInAnyEra era1 thing1 == BuildableInAnyEra era2 thing2 =
        case testEquality era1 era2 of
            Just Refl -> thing1 == thing2
            Nothing -> False

instance Buildable (BuildableInAnyEra a) where
    build (BuildableInAnyEra _ x) = build x

-- | Indicates a failure to select a sufficient amount of collateral.
--
data ErrBalanceTxInsufficientCollateralError era =
    ErrBalanceTxInsufficientCollateralError
    { largestCombinationAvailable :: UTxO (ShelleyLedgerEra era)
        -- ^ The largest available combination of pure ada UTxOs.
    , minimumCollateralAmount :: Coin
        -- ^ The minimum quantity of ada necessary for collateral.
    }
    deriving Generic

deriving instance
    RecentEraLedgerConstraints (ShelleyLedgerEra era) =>
    Eq (ErrBalanceTxInsufficientCollateralError era)

deriving instance
    RecentEraLedgerConstraints (ShelleyLedgerEra era) =>
    Show (ErrBalanceTxInsufficientCollateralError era)

-- | Indicates that there was not enough ada available to create change outputs.
--
-- When creating a change output, ada is required in order to pay for:
--
--  - the minimum ada quantity required for that change output; and
--  - the marginal fee for including that output in the transaction.
--
data ErrBalanceTxUnableToCreateChangeError =
    ErrBalanceTxUnableToCreateChangeError
    { requiredCost :: !Coin
        -- ^ An estimate of the minimal fee required for this transaction to
        -- be considered valid.
        --
        -- TODO: ADP-2547
        -- Investigate whether this field is really appropriate and necessary,
        -- and if not, remove it.
    , shortfall :: !Coin
        -- ^ The total additional quantity of ada required to pay for the
        -- minimum ada quantities of all change outputs as well as the
        -- marginal fee for including these outputs in the transaction.
    }
    deriving (Eq, Generic, Show)

-- | Indicates the insufficient availability of one or more assets.
--
-- This error is returned when the available quantity of one or more assets
-- is insufficient to balance the transaction.
--
-- The 'shortfall' field indicates the minimum extra quantity of each asset
-- that would be necessary to balance the transaction.
--
data ErrBalanceTxAssetsInsufficientError = ErrBalanceTxAssetsInsufficientError
    { available :: Value
        -- ^ The total sum of all assets available.
    , required :: Value
        -- ^ The total sum of all assets required.
    , shortfall :: Value
        -- ^ The total shortfall between available and required assets.
    }
    deriving (Eq, Generic, Show)

data ErrBalanceTxInternalError era
    = RecentEraConstraints era =>
        ErrUnderestimatedFee Coin (Tx (ShelleyLedgerEra era)) KeyWitnessCount
    | ErrFailedBalancing Value

deriving instance Eq (ErrBalanceTxInternalError era)
deriving instance Show (ErrBalanceTxInternalError era)

-- | Errors that can occur when balancing transactions.
data ErrBalanceTx era
    = ErrBalanceTxUpdateError ErrUpdateSealedTx
    | ErrBalanceTxAssetsInsufficient ErrBalanceTxAssetsInsufficientError
    | ErrBalanceTxMaxSizeLimitExceeded
    | ErrBalanceTxExistingCollateral
    | ErrBalanceTxExistingTotalCollateral
    | ErrBalanceTxExistingReturnCollateral
    | RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => ErrBalanceTxInsufficientCollateral
        (ErrBalanceTxInsufficientCollateralError era)
    | ErrBalanceTxConflictingNetworks
    | ErrBalanceTxAssignRedeemers ErrAssignRedeemers
    | ErrBalanceTxInternalError (ErrBalanceTxInternalError era)
    | RecentEraLedgerConstraints (ShelleyLedgerEra era)
        => ErrBalanceTxInputResolutionConflicts
        (NonEmpty (TxOut (ShelleyLedgerEra era), TxOut (ShelleyLedgerEra era)))
    | ErrBalanceTxUnresolvedInputs (NonEmpty TxIn)
    | ErrBalanceTxOutputError ErrBalanceTxOutputError
    | ErrBalanceTxUnableToCreateChange ErrBalanceTxUnableToCreateChangeError
    | ErrBalanceTxUnableToCreateInput
    -- ^ Returned when __both__ of the following conditions are true:
    --   - the given partial transaction has no existing inputs; and
    --   - the given UTxO index is empty.
    -- A transaction must have at least one input in order to be valid.

deriving instance Eq (ErrBalanceTx era)
deriving instance Show (ErrBalanceTx era)

-- | A 'PartialTx' is an an unbalanced 'SealedTx' along with the necessary
-- information to balance it.
--
-- The 'TxIn's of the 'inputs' must exactly match the inputs contained in the
-- 'tx'. If not, the behaviour is undefined. This will be fixed by ADP-1662.
--
-- The provided 'redeemers' will overwrite any redeemers inside the 'tx'. This
-- is done as the internal redeemers in the 'tx' use an index referring to a
-- 'TxIn', rather than an actual 'TxIn'. When we are adding extra inputs as part
-- of balancing, these indexes become incorrect.
--
-- TODO: With some extra care, we could probably remove the 'redeemers' field
-- and instead adjust the existing redeemer indexes ourselves when balancing,
-- even though they are in an "unordered" set.
data PartialTx era = PartialTx
    { tx :: Tx (ShelleyLedgerEra era)
    , inputs :: UTxO (ShelleyLedgerEra era)
      -- ^ NOTE: Can we rename this to something better? Perhaps 'extraUTxO'?
    , redeemers :: [Redeemer]
    }
    deriving Generic

deriving instance
    RecentEraLedgerConstraints (ShelleyLedgerEra era) =>
    Eq (PartialTx era)

deriving instance
    RecentEraLedgerConstraints (ShelleyLedgerEra era) =>
    Show (PartialTx era)

instance
    RecentEraLedgerConstraints (ShelleyLedgerEra era) =>
    Buildable (PartialTx era)
  where
    build (PartialTx tx (UTxO ins) redeemers)
        = nameF "PartialTx" $ mconcat
            [ nameF "inputs" (blockListF' "-" inF (Map.toList ins))
            , nameF "redeemers" (pretty redeemers)
            , nameF "tx" (txF tx)
            ]
      where
        inF = build . show

        txF :: Tx (ShelleyLedgerEra era) -> Builder
        txF tx' = pretty $ pShow tx'

data UTxOIndex era = UTxOIndex
    { walletUTxO :: !W.UTxO
    , walletUTxOIndex :: !(UTxOIndex.UTxOIndex WalletUTxO)
    , ledgerUTxO :: !(UTxO (ShelleyLedgerEra era))
    }

constructUTxOIndex
    :: forall era. IsRecentEra era
    => UTxO (ShelleyLedgerEra era)
    -> UTxOIndex era
constructUTxOIndex ledgerUTxO =
    UTxOIndex {walletUTxO, walletUTxOIndex, ledgerUTxO}
  where
    era = recentEra @era
    walletUTxO = toWalletUTxO era ledgerUTxO
    walletUTxOIndex = UTxOIndex.fromMap $ toInternalUTxOMap walletUTxO

fromWalletUTxO
    :: RecentEra era
    -> W.UTxO
    -> UTxO (ShelleyLedgerEra era)
fromWalletUTxO era (W.UTxO m) = withConstraints era $ UTxO
    $ Map.mapKeys Convert.toLedger
    $ Map.map (toLedgerTxOut era) m

toWalletUTxO
    :: RecentEra era
    -> UTxO (ShelleyLedgerEra era)
    -> W.UTxO
toWalletUTxO era (UTxO m) = withConstraints era $ W.UTxO
    $ Map.mapKeys Convert.toWallet
    $ Map.map (toWalletTxOut era) m

balanceTransaction
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => UTxOAssumptions
    -> ProtocolParameters era
    -- ^ 'CardanoApi.ProtocolParameters' can be retrieved via a Local State
    -- Query to a local node.
    --
    -- If passed an incorrect value, a phase 1 script integrity hash mismatch
    -- will protect against collateral being forfeited.
    --
    -- TODO: Remove the 'W.ProtocolParameters' argument.
    -> TimeTranslation
    -- ^ Needed to convert convert validity intervals from 'UTCTime' to 'SlotNo'
    -- when executing Plutus scripts.
    --
    -- Can be hard-coded for a given network configuration. Just be
    -- cautious of the fact that the value will occasionally
    -- change as new eras are introduced to Cardano.
    --
    -- It is unclear whether an incorrect value could cause collateral to be
    -- forfeited. We should ideally investigate and clarify as part of ADP-1544
    -- or similar ticket. Relevant ledger code:
    -- https://github.com/input-output-hk/cardano-ledger/blob/fdec04e8c071060a003263cdcb37e7319fb4dbf3/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L428-L440
    -> UTxOIndex era
    -- ^ TODO [ADP-1789] Replace with @CardanoApi.UTxO@
    -> ChangeAddressGen changeState
    -> changeState
    -> PartialTx era
    -> ExceptT (ErrBalanceTx era) m (CardanoApi.Tx era, changeState)
balanceTransaction
    utxoAssumptions
    pp
    timeTranslation
    utxo
    genChange
    s
    partialTx
    = do
    let adjustedPartialTx = flip (over #tx) partialTx
            ( fromCardanoApiTx
            . assignMinimalAdaQuantitiesToOutputsWithoutAda
                (recentEra @era)
                (pparamsLedger pp)
            . toCardanoApiTx
            )
        balanceWith strategy =
            balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
                @era @m @changeState
                utxoAssumptions
                pp
                timeTranslation
                utxo
                genChange
                s
                strategy
                adjustedPartialTx
    balanceWith SelectionStrategyOptimal
        `catchE` \e ->
            if minimalStrategyIsWorthTrying e
            then balanceWith SelectionStrategyMinimal
            else throwE e
  where
    -- Determines whether or not the minimal selection strategy is worth trying.
    -- This depends upon the way in which the optimal selection strategy failed.
    minimalStrategyIsWorthTrying :: ErrBalanceTx era -> Bool
    minimalStrategyIsWorthTrying e = or
        [ maxSizeLimitExceeded
        , unableToConstructChange
        , selectionCollateralError
        ]
      where
        -- The size of a transaction can be reduced by selecting fewer inputs,
        -- or by generating less change. Since the minimal selection strategy
        -- selects as few inputs and generates as little change as possible,
        -- using this strategy might allow us to generate a transaction within
        -- the size limit.
        maxSizeLimitExceeded = case e of
            ErrBalanceTxMaxSizeLimitExceeded ->
                True
            _someOtherError ->
                False

        -- In situations where the available supply of ada is constrained, or
        -- where all available ada is bundled up with other tokens, this can
        -- prevent us from generating change. In this case, trying again with
        -- the minimal selection strategy might allow us to select fewer
        -- inputs, generate less change, lower the amount of ada required to
        -- pay for the change, and therefore increase the chance that we can
        -- generate change successfully.
        unableToConstructChange = case e of
            ErrBalanceTxUnableToCreateChange {} ->
                True
            _someOtherError ->
                False

        -- The minimum required amount of collateral depends on the transaction
        -- fee, which in turn depends on the space occupied by ordinary inputs
        -- and generated change. If we select fewer inputs and generate less
        -- change, we can lower the transaction fee, lower the minimum required
        -- amount of collateral, and increase the chance of being able to
        -- satisfy the minimum.
        selectionCollateralError = case e of
            ErrBalanceTxInsufficientCollateral {} ->
                True
            _someOtherError ->
                False

-- | Assigns minimal ada quantities to outputs without ada.
--
-- This function only modifies outputs with ada quantities of zero. Outputs
-- that have non-zero ada quantities will not be modified.
--
-- Minimal ada quantities are computed with the 'computeMinimumCoinForTxOut'
-- function.
--
assignMinimalAdaQuantitiesToOutputsWithoutAda
    :: forall era
     . RecentEra era
    -> PParams (CardanoApi.ShelleyLedgerEra era)
    -> CardanoApi.Tx era
    -> CardanoApi.Tx era
assignMinimalAdaQuantitiesToOutputsWithoutAda era pp = withConstraints era $
    modifyLedgerBody $ over outputsTxBodyL $ fmap modifyTxOut
  where
    modifyTxOut out = flip (modifyTxOutCoin era) out $ \c ->
        if c == mempty then computeMinimumCoinForTxOut era pp out else c

-- | Internal helper to 'balanceTransaction'
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => UTxOAssumptions
    -> ProtocolParameters era
    -> TimeTranslation
    -> UTxOIndex era
    -> ChangeAddressGen changeState
    -> changeState
    -> SelectionStrategy
    -> PartialTx era
    -> ExceptT (ErrBalanceTx era) m (CardanoApi.Tx era, changeState)
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    utxoAssumptions
    protocolParameters@(ProtocolParameters pp)
    timeTranslation
    (UTxOIndex walletUTxO internalUtxoAvailable walletLedgerUTxO)
    genChange
    s
    selectionStrategy
    ptx@(PartialTx partialTx inputUTxO redeemers)
    = do
    guardExistingCollateral partialLedgerTx
    guardExistingTotalCollateral partialLedgerTx
    guardExistingReturnCollateral partialLedgerTx
    guardWalletUTxOConsistencyWith inputUTxO

    (balance0, minfee0, _) <- balanceAfterSettingMinFee partialLedgerTx

    (extraInputs, extraCollateral', extraOutputs, s') <- do

        -- NOTE: It is not possible to know the script execution cost in
        -- advance because it actually depends on the final transaction. Inputs
        -- selected as part of the fee balancing might have an influence on the
        -- execution cost.
        -- However, they are bounded so it is possible to balance the
        -- transaction considering only the maximum cost, and only after, try to
        -- adjust the change and ExUnits of each redeemer to something more
        -- sensible than the max execution cost.

        randomSeed <- stdGenSeed
        let
            transform
                :: Selection
                -> ( [(W.TxIn, W.TxOut)]
                   , [(W.TxIn, W.TxOut)]
                   , [W.TxOut]
                   , changeState
                   )
            transform sel =
                let (sel', s') = assignChangeAddresses genChange sel s
                    inputs = F.toList (sel' ^. #inputs)
                in  ( inputs
                    , sel' ^. #collateral
                    , sel' ^. #change
                    , s'
                    )

        externalSelectedUtxo <- extractExternallySelectedUTxO ptx

        let mSel = selectAssets
                (recentEra @era)
                protocolParameters
                utxoAssumptions
                (extractOutputsFromTx (toCardanoApiTx partialTx))
                redeemers
                (UTxOSelection.fromIndexPair
                    (internalUtxoAvailable, externalSelectedUtxo))
                balance0
                (fromCardanoApiLovelace minfee0)
                randomSeed
                genChange
                selectionStrategy

        except $ transform <$> mSel

    -- NOTE:
    -- Once the coin selection is done, we need to
    --
    -- (a) Add selected inputs, collateral and change outputs to the transaction
    -- (b) Assign correct execution units to every redeemer
    -- (c) Correctly reference redeemed entities with redeemer pointers
    -- (d) Adjust fees and change output(s) to the new fees.
    -- (e) If added inputs are native script originated coresponding scripts
    --     need to be added as witnesses
    --
    -- There's a strong assumption that modifying the fee value AND increasing
    -- the coin values of change outputs does not modify transaction fees; or
    -- more exactly, does not modify the execution units of scripts. This is in
    -- principle a fair assumption because script validators ought to be
    -- unaware of change outputs. If their execution costs increase when change
    -- output values increase, then it becomes impossible to guarantee that fee
    -- balancing will ever converge towards a fixed point. A script validator
    -- doing such a thing is considered bonkers and this is not a behavior we
    -- ought to support.

    let extraInputScripts =
            case utxoAssumptions of
                AllKeyPaymentCredentials -> []
                AllByronKeyPaymentCredentials -> []
                AllScriptPaymentCredentialsFrom _template toInpScripts ->
                    ( toInpScripts
                    . Convert.toLedgerAddress
                    . view #address
                    . snd
                    ) <$> extraInputs <> extraCollateral'
        extraCollateral = fst <$> extraCollateral'
        unsafeFromLovelace (CardanoApi.Lovelace l) = W.Coin.unsafeFromIntegral l
    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , extraInputScripts
        , feeUpdate = UseNewTxFee $ unsafeFromLovelace minfee0
        }

    (balance, candidateMinFee, witCount) <-
        balanceAfterSettingMinFee candidateTx
    surplus <- case CardanoApi.selectLovelace balance of
        (CardanoApi.Lovelace c)
            | c >= 0 ->
                pure $ W.Coin.unsafeFromIntegral c
            | otherwise ->
                throwE
                $ withConstraints era
                $ ErrBalanceTxInternalError
                $ ErrUnderestimatedFee
                    (Coin (-c))
                    candidateTx
                    witCount

    let feeAndChange = TxFeeAndChange
            (unsafeFromLovelace candidateMinFee)
            (extraOutputs)
        feePerByte = getFeePerByte (recentEra @era) pp

    -- @distributeSurplus@ should never fail becase we have provided enough
    -- padding in @selectAssets@.
    TxFeeAndChange updatedFee updatedChange <- withExceptT
        (\(ErrMoreSurplusNeeded c) ->
            ErrBalanceTxInternalError
                $ withConstraints era
                $ ErrUnderestimatedFee
                    (Convert.toLedgerCoin c)
                    candidateTx
                    witCount
        )
        (ExceptT . pure $
            distributeSurplus feePerByte surplus feeAndChange)

    fmap ((, s') . toCardanoApiTx) . guardTxSize witCount
        =<< guardTxBalanced
        =<< assembleTransaction
        TxUpdate
            { extraInputs
            , extraCollateral
            , extraOutputs = updatedChange
            , extraInputScripts
            , feeUpdate = UseNewTxFee updatedFee
            }
  where
    era = recentEra @era

    partialLedgerTx = partialTx

    -- | Extract the inputs from the raw 'tx' of the 'Partialtx', with the
    -- corresponding 'TxOut' according to @combinedUTxO@.
    --
    -- === Examples using pseudo-code
    --
    -- >>> let extraUTxO = {inA -> outA, inB -> outB }
    -- >>> let tx = addInputs [inA] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Right (UTxOIndex.fromMap {inA -> outA})
    --
    -- >>> let extraUTxO = {inB -> outB }
    -- >>> let tx = addInputs [inA, inC] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Left (ErrBalanceTxUnresolvedInputs [inA, inC])
    extractExternallySelectedUTxO
        :: PartialTx era
        -> ExceptT (ErrBalanceTx era) m (UTxOIndex.UTxOIndex WalletUTxO)
    extractExternallySelectedUTxO (PartialTx tx _ _rdms) =
        withConstraints era $ do
            let res = flip map txIns $ \i-> do
                    case txinLookup i combinedUTxO of
                        Nothing ->
                           Left i
                        Just o -> do
                            let i' = Convert.toWallet i
                            let W.TxOut addr bundle = toWalletTxOut era o
                            pure (WalletUTxO i' addr, bundle)

            case partitionEithers res of
                ([], resolved) ->
                    pure $ UTxOIndex.fromSequence resolved
                (unresolvedInsHead:unresolvedInsTail, _) ->
                    throwE
                    . ErrBalanceTxUnresolvedInputs
                    $ (unresolvedInsHead :| unresolvedInsTail)
      where
        txIns :: [TxIn]
        txIns = withConstraints (recentEra @era) $
            Set.toList $ tx ^. (bodyTxL . inputsTxBodyL)

    guardTxSize
        :: KeyWitnessCount
        -> Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m (Tx (ShelleyLedgerEra era))
    guardTxSize witCount tx =
        withConstraints era $ do
            let maxSize = TxSize (pp ^. ppMaxTxSizeL)
            when (estimateSignedTxSize era pp witCount tx > maxSize) $
                throwE ErrBalanceTxMaxSizeLimitExceeded
            pure tx

    guardTxBalanced
        :: Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m (Tx (ShelleyLedgerEra era))
    guardTxBalanced tx = do
        let bal = txBalance tx
        if bal == mempty
            then pure tx
            else throwE
                $ ErrBalanceTxInternalError
                $ ErrFailedBalancing bal

    txBalance :: Tx (ShelleyLedgerEra era) -> Value
    txBalance
        = withConstraints era
        . evaluateTransactionBalance era pp combinedUTxO
        . txBody era

    balanceAfterSettingMinFee
        :: Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m
            (CardanoApi.Value, CardanoApi.Lovelace, KeyWitnessCount)
    balanceAfterSettingMinFee tx = ExceptT . pure $ do
        let witCount = estimateKeyWitnessCount combinedUTxO cardanoApiTxBody
            minfee = Convert.toWalletCoin $ evaluateMinimumFee
                era pp tx witCount
            update = TxUpdate [] [] [] [] (UseNewTxFee minfee)
        tx' <- left ErrBalanceTxUpdateError $ updateTx era tx update
        let balance = CardanoApi.fromMaryValue $ txBalance tx'
            minfee' = CardanoApi.Lovelace $ W.Coin.toInteger minfee
        return (balance, minfee', witCount)
      where
        cardanoApiTxBody :: CardanoApi.TxBody era
        cardanoApiTxBody =
            let CardanoApi.Tx body _ = toCardanoApiTx tx
            in body

    -- | Ensure the wallet UTxO is consistent with a provided @CardanoApi.UTxO@.
    --
    -- They are not consistent iff an input can be looked up in both UTxO sets
    -- with different @Address@, or @TokenBundle@ values.
    --
    -- The @CardanoApi.UTxO era@ is allowed to contain additional information,
    -- like datum hashes, which the wallet UTxO cannot represent.
    --
    -- NOTE: Representing the wallet utxo as a @CardanoApi.UTxO@ will not make
    -- this check easier, even if it may be useful in other regards.
    guardWalletUTxOConsistencyWith
        :: UTxO (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m ()
    guardWalletUTxOConsistencyWith u' = do
        let W.UTxO u = toWalletUTxO (recentEra @era) u'
        let conflicts = lefts $ flip map (Map.toList u) $ \(i, o1) ->
                case i `W.UTxO.lookup` walletUTxO of
                    Just o2 -> unless (o1 == o2) $ Left
                        ( toLedgerTxOut era o1
                        , toLedgerTxOut era o2
                        )
                    Nothing -> pure ()
        case conflicts of
            [] -> return ()
            (c : cs) -> throwE
                $ withConstraints era
                $ ErrBalanceTxInputResolutionConflicts (c :| cs)

    combinedUTxO :: UTxO (ShelleyLedgerEra era)
    combinedUTxO = withConstraints era $ mconcat
         -- The @CardanoApi.UTxO@ can contain strictly more information than
         -- @W.UTxO@. Therefore we make the user-specified @inputUTxO@ to take
         -- precedence. This matters if a user is trying to balance a tx making
         -- use of a datum hash in a UTxO which is also present in the wallet
         -- UTxO set. (Whether or not this is a sane thing for the user to do,
         -- is another question.)
         [ inputUTxO
         , walletLedgerUTxO
         ]

    extractOutputsFromTx :: CardanoApi.Tx era -> [W.TxOut]
    extractOutputsFromTx (CardanoApi.ByronTx _) = case era of {}
    extractOutputsFromTx (CardanoApi.ShelleyTx _ tx) =
        map fromLedgerTxOut
        $ outputs era
        $ txBody era tx
      where
        fromLedgerTxOut :: TxOut (ShelleyLedgerEra era) -> W.TxOut
        fromLedgerTxOut o = case era of
           RecentEraBabbage -> Convert.fromBabbageTxOut o
           RecentEraConway -> Convert.fromConwayTxOut o

    assembleTransaction
        :: TxUpdate
        -> ExceptT (ErrBalanceTx era) m (Tx (ShelleyLedgerEra era))
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left ErrBalanceTxUpdateError $ updateTx era partialLedgerTx update
        left ErrBalanceTxAssignRedeemers $
            assignScriptRedeemers
                era pp timeTranslation combinedUTxO redeemers tx'

    guardExistingCollateral
        :: Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m ()
    guardExistingCollateral tx = withConstraints era $ do
        -- Coin selection does not support pre-defining collateral. In Sep 2021
        -- consensus was that we /could/ allow for it with just a day's work or
        -- so, but that the need for it was unclear enough that it was not in
        -- any way a priority.
        let collIns = tx ^. (bodyTxL . collateralInputsTxBodyL)
        unless (null collIns) $
            throwE ErrBalanceTxExistingCollateral

    guardExistingTotalCollateral
        :: Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m ()
    guardExistingTotalCollateral tx = withConstraints era $ do
        let totColl = tx ^. (bodyTxL . totalCollateralTxBodyL)
        case totColl of
            SNothing -> return ()
            SJust _ -> throwE ErrBalanceTxExistingTotalCollateral

    guardExistingReturnCollateral
        :: Tx (ShelleyLedgerEra era)
        -> ExceptT (ErrBalanceTx era) m ()
    guardExistingReturnCollateral tx = withConstraints era $ do
        let collRet = tx ^. (bodyTxL . collateralReturnTxBodyL)
        case collRet of
            SNothing -> return ()
            SJust _ -> throwE ErrBalanceTxExistingReturnCollateral

    fromCardanoApiLovelace (CardanoApi.Lovelace l) = W.Coin.unsafeFromIntegral l

-- | Select assets to cover the specified balance and fee.
--
-- If the transaction contains redeemers, the function will also ensure the
-- selection covers the fees for the maximum allowed execution units of a
-- transaction. For this, and other reasons, the selection may include too
-- much ada.
selectAssets
    :: forall era changeState
     . RecentEra era
    -> ProtocolParameters era
    -> UTxOAssumptions
    -> [W.TxOut]
    -> [Redeemer]
    -> UTxOSelection WalletUTxO
    -- ^ Specifies which UTxOs are pre-selected, and which UTxOs can be used as
    -- inputs or collateral.
    -> CardanoApi.Value -- Balance to cover
    -> W.Coin -- Current minfee (before selecting assets)
    -> StdGenSeed
    -> ChangeAddressGen changeState
    -> SelectionStrategy
    -- ^ A function to assess the size of a token bundle.
    -> Either (ErrBalanceTx era) Selection
selectAssets era (ProtocolParameters pp) utxoAssumptions outs redeemers
    utxoSelection balance fee0 seed changeGen selectionStrategy = do
        validateTxOutputs'
        performSelection'
  where
    validateTxOutputs'
        :: Either (ErrBalanceTx era) ()
    validateTxOutputs'
        = left ErrBalanceTxOutputError
        $ validateTxOutputs selectionConstraints
            (outs <&> \out -> (view #address out, view #tokens out))

    performSelection'
        :: Either (ErrBalanceTx era) Selection
    performSelection'
        = left (coinSelectionErrorToBalanceTxError era)
        $ (`evalRand` stdGenFromSeed seed) . runExceptT
        $ performSelection selectionConstraints selectionParams

    selectionConstraints = SelectionConstraints
        { tokenBundleSizeAssessor =
            mkTokenBundleSizeAssessor era pp
        , computeMinimumAdaQuantity = \addr tokens -> Convert.toWallet $
            computeMinimumCoinForTxOut
                era
                pp
                (mkLedgerTxOut era addr (W.TokenBundle txOutMaxCoin tokens))
        , isBelowMinimumAdaQuantity = \addr bundle ->
            isBelowMinimumCoinForTxOut
                era
                pp
                (mkLedgerTxOut era addr bundle)
        , computeMinimumCost = \skeleton -> mconcat
            [ feePadding
            , fee0
            , txPlutusScriptExecutionCost
            , estimateTxCost feePerByte $ TxSkeleton
                { txWitnessTag = assumedTxWitnessTag utxoAssumptions
                , txInputCount = view #skeletonInputCount skeleton
                , txOutputs = view #skeletonOutputs skeleton
                , txChange = view #skeletonChange skeleton
                , txPaymentTemplate = view #template <$>
                    assumedInputScriptTemplate utxoAssumptions
                }
            ] <\> boringFee
        , maximumCollateralInputCount = withConstraints era $
            unsafeIntCast @Natural @Int $ pp ^. ppMaxCollateralInputsL
    , minimumCollateralPercentage =
        withConstraints era $ pp ^. ppCollateralPercentageL
    , maximumLengthChangeAddress =
        Convert.toWalletAddress $ maxLengthChangeAddress changeGen
    }

    selectionParams = SelectionParams
        -- The following fields are essentially adjusting the coin selection
        -- algorithm's notion of balance by @balance0 - sum inputs + sum
        -- outputs + fee0@ where @balance0@ is the balance of the
        -- partial tx.
        { extraValueIn =
            balancePositive <> valueOfOutputs <> W.TokenBundle.fromCoin fee0
        , extraValueOut =
            balanceNegative <> valueOfInputs
        -- NOTE: It is important that coin selection has the correct
        -- notion of fees, because it will be used to tell how much
        -- collateral is needed.
        , collateralRequirement
        , outputsToCover = outs
        , utxoAvailableForCollateral = UTxOSelection.availableMap utxoSelection
        , utxoAvailableForInputs = utxoSelection
        , selectionStrategy = selectionStrategy
        }
      where
        (balancePositive, balanceNegative) =
            posAndNegFromCardanoApiValue balance
        valueOfOutputs = F.foldMap' (view #tokens) outs
        valueOfInputs = UTxOSelection.selectedBalance utxoSelection

    mkLedgerTxOut
        :: HasCallStack
        => RecentEra era
        -> W.Address
        -> W.TokenBundle
        -> TxOut (ShelleyLedgerEra era)
    mkLedgerTxOut txOutEra address bundle =
        case txOutEra of
            RecentEraBabbage -> Convert.toBabbageTxOut txOut
            RecentEraConway -> Convert.toConwayTxOut txOut
          where
            txOut = W.TxOut address bundle

    txPlutusScriptExecutionCost = Convert.toWallet @W.Coin $
        if null redeemers
            then mempty
            else maxScriptExecutionCost era pp

    collateralRequirement =
        if txPlutusScriptExecutionCost > W.Coin 0
            then SelectionCollateralRequired
            else SelectionCollateralNotRequired

    feePerByte = getFeePerByte era pp

    boringFee =
        estimateTxCost
            feePerByte
            TxSkeleton
                { txWitnessTag = assumedTxWitnessTag utxoAssumptions
                , txInputCount = UTxOSelection.selectedSize utxoSelection
                , txOutputs = outs
                , txChange = []
                , txPaymentTemplate = Nothing
                }

    feePadding
        = Convert.toWallet . feeOfBytes feePerByte
        $ extraBytes + scriptIntegrityHashBytes
      where
        -- Could be made smarter by only padding for the script
        -- integrity hash when we intend to add one. [ADP-2621]
        scriptIntegrityHashBytes = 32 + 2

        -- Add padding to allow the fee value to increase.
        -- Out of caution, assume it can increase by the theoretical
        -- maximum of 8 bytes ('maximumCostOfIncreasingCoin').
        --
        -- NOTE: It's not convenient to import the constant at the
        -- moment because of the package split.
        --
        -- Any overestimation will be reduced by 'distributeSurplus'
        -- in the final stage of 'balanceTransaction'.
        extraBytes = 8

data ChangeAddressGen s = ChangeAddressGen
    { getChangeAddressGen :: s -> (Address, s)

    -- | Returns the longest address that the wallet can generate for a given
    --   key.
    --
    -- This is useful in situations where we want to compute some function of
    -- an output under construction (such as a minimum UTxO value), but don't
    -- yet have convenient access to a real address.
    --
    -- Please note that this address should:
    --
    --  - never be used for anything besides its length and validity properties.
    --  - never be used as a payment target within a real transaction.
    --
    , maxLengthChangeAddress :: Address
    }

-- | Augments the given outputs with new outputs. These new outputs correspond
-- to change outputs to which new addresses have been assigned. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: ChangeAddressGen s
    -> SelectionOf W.TokenBundle
    -> s
    -> (SelectionOf W.TxOut, s)
assignChangeAddresses (ChangeAddressGen genChange _) sel = runState $ do
    changeOuts <- forM (view #change sel) $ \bundle -> do
        addr <- state genChange
        pure $ W.TxOut (Convert.toWalletAddress addr) bundle
    pure $ (sel :: SelectionOf W.TokenBundle) { change = changeOuts }

-- | Convert a 'CardanoApi.Value' into a positive and negative component. Useful
-- to convert the potentially negative balance of a partial tx into
-- TokenBundles.
posAndNegFromCardanoApiValue
    :: CardanoApi.Value
    -> (W.TokenBundle, W.TokenBundle)
posAndNegFromCardanoApiValue
    = bimap
        (fromCardanoApiValue . CardanoApi.valueFromList)
        (fromCardanoApiValue . CardanoApi.valueFromList . L.map (second negate))
    . L.partition ((>= 0) . snd)
    . CardanoApi.valueToList

unsafeIntCast
    :: (HasCallStack, Integral a, Integral b, Bits a, Bits b, Show a)
    => a
    -> b
unsafeIntCast x = fromMaybe err $ intCastMaybe x
  where
    err = error $ "unsafeIntCast failed for " <> show x

--------------------------------------------------------------------------------
-- updateTx
--------------------------------------------------------------------------------

-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
    { extraInputs :: [(W.TxIn, W.TxOut)]
    , extraCollateral :: [W.TxIn]
       -- ^ Only used in the Alonzo era and later. Will be silently ignored in
       -- previous eras.
    , extraOutputs :: [W.TxOut]
    , extraInputScripts :: [CA.Script CA.KeyHash]
    , feeUpdate :: TxFeeUpdate
        -- ^ Set a new fee or use the old one.
    }

-- | For testing that
-- @
--   forall tx. updateTx noTxUpdate tx
--      == Right tx or Left
-- @
noTxUpdate :: TxUpdate
noTxUpdate = TxUpdate [] [] [] [] UseOldTxFee

-- | Method to use when updating the fee of a transaction.
data TxFeeUpdate
    = UseOldTxFee
        -- ^ Instead of updating the fee, just use the old fee of the
        -- Tx (no-op for fee update).
    | UseNewTxFee W.Coin
        -- ^ Specify a new fee to use instead.
    deriving (Eq, Show)

newtype ErrUpdateSealedTx
    = ErrExistingKeyWitnesses Int
    -- ^ The `SealedTx` could not be updated because the *n* existing
    -- key-witnesses would be rendered invalid.
    deriving (Generic, Eq, Show)

-- | Used to add inputs and outputs when balancing a transaction.
--
-- If the transaction contains existing key witnesses, it will return `Left`,
-- *even if `noTxUpdate` is used*. This last detail could be changed.
--
-- == Notes on implementation choices
--
-- We cannot rely on cardano-api here because `CardanoApi.TxBodyContent BuildTx`
-- cannot be extracted from an existing `TxBody`.
--
-- To avoid the need for `ledger -> wallet` conversions, this function can only
-- be used to *add* tx body content.
updateTx
    :: forall era. RecentEra era
    -> Tx (ShelleyLedgerEra era)
    -> TxUpdate
    -> Either ErrUpdateSealedTx (Tx (ShelleyLedgerEra era))
updateTx era tx extraContent = withConstraints era $ do
    let tx' = tx
            & over bodyTxL (modifyShelleyTxBody extraContent era)
            & over (witsTxL . scriptTxWitsL) (<> extraInputScripts')

    case numberOfExistingKeyWits of
        0 -> Right tx'
        n -> Left $ ErrExistingKeyWitnesses n
  where
    numberOfExistingKeyWits :: Int
    numberOfExistingKeyWits = withConstraints era $ sum
        [ Set.size $ tx ^. (witsTxL . addrTxWitsL)
        , Set.size $ tx ^. (witsTxL . bootAddrTxWitsL)
        ]

    TxUpdate _ _ _ extraInputScripts _ = extraContent

    extraInputScripts'
        :: Map (ScriptHash StandardCrypto) (Script (ShelleyLedgerEra era))
    extraInputScripts' = withConstraints era $
        Map.fromList $ map (pairWithHash . convert) extraInputScripts
      where
        pairWithHash s = (hashScript s, s)
        convert = flip toLedgerScript era

    toLedgerScript
        :: CA.Script CA.KeyHash
        -> RecentEra era
        -> Core.Script (ShelleyLedgerEra era)
    toLedgerScript s = \case
        RecentEraBabbage -> TimelockScript $ Convert.toLedgerTimelockScript s
        RecentEraConway -> TimelockScript $ Convert.toLedgerTimelockScript s

modifyShelleyTxBody
    :: forall era. TxUpdate
    -> RecentEra era
    -> TxBody (ShelleyLedgerEra era)
    -> TxBody (ShelleyLedgerEra era)
modifyShelleyTxBody txUpdate era = withConstraints era $
    over feeTxBodyL modifyFee
    . over outputsTxBodyL
        (<> extraOutputs')
    . over inputsTxBodyL
        (<> extraInputs')
    . over collateralInputsTxBodyL
        (<> extraCollateral')
  where
    TxUpdate extraInputs extraCollateral extraOutputs _ feeUpdate = txUpdate
    extraOutputs' = StrictSeq.fromList $ map (toLedgerTxOut era) extraOutputs
    extraInputs' = Set.fromList (Convert.toLedger . fst <$> extraInputs)
    extraCollateral' = Set.fromList $ Convert.toLedger <$> extraCollateral

    modifyFee old = case feeUpdate of
        UseNewTxFee c -> Convert.toLedger c
        UseOldTxFee -> old

--------------------------------------------------------------------------------
-- distributeSurplus
--------------------------------------------------------------------------------

-- | Indicates that it's impossible for 'distributeSurplus' to distribute a
-- surplus. As long as the surplus is larger than 'costOfIncreasingCoin', this
-- should never happen.
newtype ErrMoreSurplusNeeded = ErrMoreSurplusNeeded W.Coin
    deriving (Generic, Eq, Show)

-- | Small helper record to disambiguate between a fee and change Coin values.
-- Used by 'distributeSurplus'.
data TxFeeAndChange change = TxFeeAndChange
    { fee :: W.Coin
    , change :: change
    }
    deriving (Eq, Show)

-- | Manipulates a 'TxFeeAndChange' value.
--
mapTxFeeAndChange
    :: (W.Coin -> W.Coin)
    -- ^ A function to transform the fee
    -> (change1 -> change2)
    -- ^ A function to transform the change
    -> TxFeeAndChange change1
    -- ^ The original fee and change
    -> TxFeeAndChange change2
    -- ^ The transformed fee and change
mapTxFeeAndChange mapFee mapChange TxFeeAndChange {fee, change} =
    TxFeeAndChange (mapFee fee) (mapChange change)

-- | Calculate the cost of increasing a CBOR-encoded Coin-value by another Coin
-- with the lovelace/byte cost given by the 'FeePolicy'.
--
-- Outputs values in the range of [0, 8 * perByteFee]
--
-- >>> let p = FeePolicy (Quantity 0) (Quantity 44)
--
-- >>> costOfIncreasingCoin p 4294967295 1
-- Coin 176 -- (9 bytes - 5 bytes) * 44 lovelace/byte
--
-- >>> costOfIncreasingCoin p 0 4294967296
-- Coin 352 -- 8 bytes * 44 lovelace/byte
costOfIncreasingCoin
    :: FeePerByte
    -> W.Coin -- ^ Original coin
    -> W.Coin -- ^ Increment
    -> W.Coin
costOfIncreasingCoin (FeePerByte perByte) from delta =
    costOfCoin (from <> delta) <\> costOfCoin from
  where
    costOfCoin = W.Coin . (perByte *) . unTxSize . sizeOfCoin

-- The maximum cost increase 'costOfIncreasingCoin' can return, which is the
-- cost of 8 bytes.
maximumCostOfIncreasingCoin :: FeePerByte -> W.Coin
maximumCostOfIncreasingCoin (FeePerByte perByte) = W.Coin $ 8 * perByte

-- | Calculate the size of a coin when encoded as CBOR.
sizeOfCoin :: W.Coin -> TxSize
sizeOfCoin (W.Coin c)
    | c >= 4_294_967_296 = TxSize 9 -- c >= 2^32
    | c >=        65_536 = TxSize 5 -- c >= 2^16
    | c >=           256 = TxSize 3 -- c >= 2^ 8
    | c >=            24 = TxSize 2
    | otherwise          = TxSize 1

-- | Distributes a surplus transaction balance between the given change
-- outputs and the given fee. This function is aware of the fact that
-- any increase in a 'Coin' value could increase the size and fee
-- requirement of a transaction.
--
-- When comparing the original fee and change outputs to the adjusted
-- fee and change outputs, this function guarantees that:
--
--    - The number of the change outputs remains constant;
--
--    - The fee quantity either remains the same or increases.
--
--    - For each change output:
--        - the ada quantity either remains constant or increases.
--        - non-ada quantities remain the same.
--
--    - The surplus is conserved:
--        The total increase in the fee and change ada quantities is
--        exactly equal to the surplus.
--
--    - Any increase in cost is covered:
--        If the total cost has increased by 𝛿c, then the fee value
--        will have increased by at least 𝛿c.
--
-- If the cost of distributing the provided surplus is greater than the
-- surplus itself, the function will return 'ErrMoreSurplusNeeded'. If
-- the provided surplus is greater or equal to
-- @maximumCostOfIncreasingCoin feePolicy@, the function will always
-- return 'Right'.
distributeSurplus
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus transaction balance to distribute.
    -> TxFeeAndChange [W.TxOut]
    -- ^ Original fee and change outputs.
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.TxOut])
    -- ^ Adjusted fee and change outputs.
distributeSurplus feePolicy surplus fc@(TxFeeAndChange fee change) =
    distributeSurplusDelta feePolicy surplus
        (mapTxFeeAndChange id (fmap W.TxOut.coin) fc)
    <&> mapTxFeeAndChange
        (fee <>)
        (zipWith (flip W.TxOut.addCoin) change)

distributeSurplusDelta
    :: FeePerByte
    -> W.Coin
    -- ^ Surplus to distribute
    -> TxFeeAndChange [W.Coin]
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange [W.Coin])
distributeSurplusDelta feePolicy surplus (TxFeeAndChange fee change) =
    case change of
        changeHead : changeTail ->
            distributeSurplusDeltaWithOneChangeCoin feePolicy surplus
                (TxFeeAndChange fee changeHead)
            <&> mapTxFeeAndChange id
                (: (W.Coin 0 <$ changeTail))
        [] ->
            burnSurplusAsFees feePolicy surplus
                (TxFeeAndChange fee ())
            <&> mapTxFeeAndChange id
                (\() -> [])

distributeSurplusDeltaWithOneChangeCoin
    :: FeePerByte
    -> W.Coin -- ^ Surplus to distribute
    -> TxFeeAndChange W.Coin
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange W.Coin)
distributeSurplusDeltaWithOneChangeCoin
    feePolicy surplus fc@(TxFeeAndChange fee0 change0) =
    let
        -- We calculate the maximum possible fee increase, by assuming the
        -- **entire** surplus is added to the change.
        extraFee = findFixpointIncreasingFeeBy $
            costOfIncreasingCoin feePolicy change0 surplus
    in
        case surplus </> extraFee of
            Just extraChange ->
                Right $ TxFeeAndChange
                    { fee = extraFee
                    , change = extraChange
                    }
            Nothing ->
                -- The fee increase from adding the surplus to the change was
                -- greater than the surplus itself. This could happen if the
                -- surplus is small.
                burnSurplusAsFees feePolicy surplus
                    (mapTxFeeAndChange id (const ()) fc)
                        <&> mapTxFeeAndChange id (\() -> W.Coin 0)
  where
    -- Increasing the fee may itself increase the fee. If that is the case, this
    -- function will increase the fee further. The process repeats until the fee
    -- doesn't need to be increased.
    --
    -- The function will always converge because the result of
    -- 'costOfIncreasingCoin' is bounded to @8 * feePerByte@.
    --
    -- On mainnet it seems unlikely that the function would recurse more than
    -- one time, and certainly not more than twice. If the protocol parameters
    -- are updated to allow for slightly more expensive txs, it might be
    -- possible to hit the boundary at ≈4 ada where the fee would need 9 bytes
    -- rather than 5. This is already the largest boundary.
    --
    -- Note that both the argument and the result of this function are increases
    -- relative to 'fee0'.
    --
    -- == Example ==
    --
    -- In this more extreme example the fee is increased from increasing the fee
    -- itself:
    --
    -- @@
    --     let fee0 = 23
    --     let feePolicy = -- 300 lovelace / byte
    --
    --     findFixpointIncreasingFeeBy 1 = go 0 1
    --     -- Recurse:
    --     = go (0 + 1) (costOfIncreasingCoin feePolicy (23 + 0) 1)
    --     = go (0 + 1) 300
    --     -- Recurse:
    --     = go (1 + 300) (costOfIncreasingCoin feePolicy (23 + 1) 300)
    --     = go 301 300
    --     = go (301 + 300) (costOfIncreasingCoin feePolicy (23 + 301) 300)
    --     = go (301 + 300) 0
    --     = go 601 0
    --     = 601
    -- @@
    findFixpointIncreasingFeeBy = go mempty
      where
        go :: W.Coin -> W.Coin -> W.Coin
        go c (W.Coin 0) = c
        go c increase = go
            (c <> increase)
            (costOfIncreasingCoin feePolicy (c <> fee0) increase)

burnSurplusAsFees
    :: FeePerByte
    -> W.Coin -- Surplus
    -> TxFeeAndChange ()
    -> Either ErrMoreSurplusNeeded (TxFeeAndChange ())
burnSurplusAsFees feePolicy surplus (TxFeeAndChange fee0 ())
    | shortfall > W.Coin 0 =
        Left $ ErrMoreSurplusNeeded shortfall
    | otherwise =
        Right $ TxFeeAndChange surplus ()
  where
    costOfBurningSurplus = costOfIncreasingCoin feePolicy fee0 surplus
    shortfall = costOfBurningSurplus <\> surplus

toLedgerTxOut
    :: HasCallStack
    => RecentEra era
    -> W.TxOut
    -> TxOut (ShelleyLedgerEra era)
toLedgerTxOut txOutEra txOut =
    case txOutEra of
        RecentEraBabbage -> Convert.toBabbageTxOut txOut
        RecentEraConway -> Convert.toConwayTxOut txOut

toWalletTxOut
    :: RecentEra era
    -> TxOut (ShelleyLedgerEra era)
    -> W.TxOut
toWalletTxOut RecentEraBabbage = Convert.fromBabbageTxOut
toWalletTxOut RecentEraConway = Convert.fromConwayTxOut

-- | Maps an error from the coin selection API to a balanceTx error.
--
coinSelectionErrorToBalanceTxError
    :: RecentEra era
    -> SelectionError WalletSelectionContext
    -> ErrBalanceTx era
coinSelectionErrorToBalanceTxError era = withConstraints era $ \case
    SelectionBalanceErrorOf balanceErr ->
        case balanceErr of
            BalanceInsufficient e ->
                ErrBalanceTxAssetsInsufficient $
                ErrBalanceTxAssetsInsufficientError
                    { available =
                        Convert.toLedger (view #utxoBalanceAvailable e)
                    , required =
                        Convert.toLedger (view #utxoBalanceRequired e)
                    , shortfall =
                        Convert.toLedger (view #utxoBalanceShortfall e)
                    }
            UnableToConstructChange
                UnableToConstructChangeError {shortfall, requiredCost} ->
                    ErrBalanceTxUnableToCreateChange
                    ErrBalanceTxUnableToCreateChangeError
                        { shortfall = Convert.toLedgerCoin shortfall
                        , requiredCost = Convert.toLedgerCoin requiredCost
                        }
            EmptyUTxO ->
                ErrBalanceTxUnableToCreateInput
    SelectionCollateralErrorOf SelectionCollateralError
        { largestCombinationAvailable
        , minimumSelectionAmount
        } ->
        ErrBalanceTxInsufficientCollateral
        ErrBalanceTxInsufficientCollateralError
            { largestCombinationAvailable
                = largestCombinationAvailable
                & fmap W.TokenBundle.fromCoin
                & toExternalUTxOMap
                & fromWalletUTxO era
            , minimumCollateralAmount
                = Convert.toLedgerCoin minimumSelectionAmount
            }

--------------------------------------------------------------------------------
-- Validation of transaction outputs
--------------------------------------------------------------------------------

-- | A validation error for a user-specified transaction output.
--
data ErrBalanceTxOutputError = ErrBalanceTxOutputErrorOf
    { outputIndex :: Int
    , outputErrorInfo :: ErrBalanceTxOutputErrorInfo
    }
    deriving (Eq, Show)

data ErrBalanceTxOutputErrorInfo
    = ErrBalanceTxOutputAdaQuantityInsufficient
        ErrBalanceTxOutputAdaQuantityInsufficientError
    | ErrBalanceTxOutputSizeExceedsLimit
        ErrBalanceTxOutputSizeExceedsLimitError
    | ErrBalanceTxOutputTokenQuantityExceedsLimit
        ErrBalanceTxOutputTokenQuantityExceedsLimitError
    deriving (Eq, Show)

data ErrBalanceTxOutputAdaQuantityInsufficientError =
    ErrBalanceTxOutputAdaQuantityInsufficientError
    { minimumExpectedCoin :: Coin
    , output :: (Address, Value)
    }
    deriving (Eq, Generic, Show)

newtype ErrBalanceTxOutputSizeExceedsLimitError =
    ErrBalanceTxOutputSizeExceedsLimitError
    { outputThatExceedsLimit :: (Address, Value)
    }
    deriving (Eq, Generic, Show)

data ErrBalanceTxOutputTokenQuantityExceedsLimitError =
    ErrBalanceTxOutputTokenQuantityExceedsLimitError
    { address :: Address
      -- ^ The address to which this token quantity was to be sent.
    , policyId :: PolicyId
      -- ^ The policy identifier to which this token quantity corresponds.
    , assetName :: AssetName
      -- ^ The asset name to which this token quantity corresponds.
    , quantity :: Natural
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: Natural
      -- ^ The maximum allowable token quantity.
    }
    deriving (Eq, Generic, Show)

-- | Validates the given transaction outputs.
--
validateTxOutputs
    :: SelectionConstraints
    -> [(W.Address, W.TokenBundle)]
    -> Either ErrBalanceTxOutputError ()
validateTxOutputs constraints outs =
    -- If we encounter an error, just report the first error we encounter:
    case errors of
        e : _ -> Left e
        []    -> pure ()
  where
    errors :: [ErrBalanceTxOutputError]
    errors = uncurry ErrBalanceTxOutputErrorOf <$> foldMap withOutputsIndexed
        [ (fmap . fmap) ErrBalanceTxOutputSizeExceedsLimit
            . mapMaybe (traverse (validateTxOutputSize constraints))
        , (fmap . fmap) ErrBalanceTxOutputTokenQuantityExceedsLimit
            . foldMap (traverse validateTxOutputTokenQuantities)
        , (fmap . fmap) ErrBalanceTxOutputAdaQuantityInsufficient
            . mapMaybe (traverse (validateTxOutputAdaQuantity constraints))
        ]
      where
        withOutputsIndexed f = f $ zip [0 ..] outs

-- | Validates the size of a transaction output.
--
-- Returns an error if (and only if) the size exceeds the limit defined by the
-- protocol.
--
validateTxOutputSize
    :: SelectionConstraints
    -> (W.Address, W.TokenBundle)
    -> Maybe ErrBalanceTxOutputSizeExceedsLimitError
validateTxOutputSize cs out@(address, bundle) = case sizeAssessment of
    TokenBundleSizeWithinLimit ->
        Nothing
    TokenBundleSizeExceedsLimit ->
        Just $
        ErrBalanceTxOutputSizeExceedsLimitError
            (Convert.toLedger address, Convert.toLedger bundle)
  where
    sizeAssessment :: TokenBundleSizeAssessment
    sizeAssessment =
        (cs ^. (#tokenBundleSizeAssessor . #assessTokenBundleSize)) (snd out)

-- | Validates the token quantities of a transaction output.
--
-- Returns a list of token quantities that exceed the limit defined by the
-- protocol.
--
validateTxOutputTokenQuantities
    :: (W.Address, W.TokenBundle)
    -> [ErrBalanceTxOutputTokenQuantityExceedsLimitError]
validateTxOutputTokenQuantities out =
    [ ErrBalanceTxOutputTokenQuantityExceedsLimitError
        {address, policyId, assetName, quantity, quantityMaxBound}
    | let address = Convert.toLedgerAddress $ fst out
    , (W.AssetId p a, W.TokenQuantity quantity) <-
        W.TokenMap.toFlatList $ (snd out) ^. #tokens
    , let (policyId, assetName) = (Convert.toLedger p, Convert.toLedger a)
    , quantity > quantityMaxBound
    ]
  where
    quantityMaxBound = W.unTokenQuantity txOutMaxTokenQuantity

-- | Validates the ada quantity associated with a transaction output.
--
-- An output's ada quantity must be greater than or equal to the minimum
-- required quantity for that output.
--
validateTxOutputAdaQuantity
    :: SelectionConstraints
    -> (W.Address, W.TokenBundle)
    -> Maybe ErrBalanceTxOutputAdaQuantityInsufficientError
validateTxOutputAdaQuantity constraints output@(address, bundle)
    | isBelowMinimum =
        Just ErrBalanceTxOutputAdaQuantityInsufficientError
            { minimumExpectedCoin
            , output = (Convert.toLedger address, Convert.toLedger bundle)
            }
    | otherwise =
        Nothing
  where
    isBelowMinimum :: Bool
    isBelowMinimum = uncurry (constraints ^. #isBelowMinimumAdaQuantity) output

    minimumExpectedCoin :: Coin
    minimumExpectedCoin = Convert.toLedgerCoin $
        (constraints ^. #computeMinimumAdaQuantity)
        (fst output)
        (snd output ^. #tokens)

fromCardanoApiValue :: CardanoApi.Value -> W.TokenBundle
fromCardanoApiValue = Convert.toWalletTokenBundle . CardanoApi.toMaryValue
