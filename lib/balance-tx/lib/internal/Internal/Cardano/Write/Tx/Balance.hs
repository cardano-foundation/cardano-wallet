{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use ||" -}

module Internal.Cardano.Write.Tx.Balance
    (
    -- * Balancing transactions
      balanceTx
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
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
    , fromWalletUTxO
    , toWalletUTxO
    , splitSignedValue
    , mergeSignedValue

    -- ** updateTx
    , TxUpdate (..)
    , noTxUpdate
    , updateTx
    , TxFeeUpdate (..)
    , ErrUpdateTx (..)

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
    , coinTxOutL
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
import Cardano.Ledger.Mary.Value
    ( MaryValue (MaryValue)
    , MultiAsset (MultiAsset)
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
    )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT)
    , catchE
    , except
    , throwE
    , withExceptT
    )
import Control.Monad.Trans.State
    ( runState
    , state
    )
import Data.Bits
    ( Bits
    )
import Data.Function
    ( on
    , (&)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , set
    , view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.Group
    ( Group (invert)
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
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
import Data.Set
    ( Set
    )
import Data.Set.NonEmpty
    ( NESet
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
import Internal.Cardano.Write.Tx
    ( Address
    , AssetName
    , Coin (..)
    , IsRecentEra (..)
    , KeyWitnessCounts (..)
    , PParams
    , PolicyId
    , RecentEra (..)
    , Tx
    , TxBody
    , TxIn
    , TxOut
    , UTxO (..)
    , Value
    , computeMinimumCoinForTxOut
    , evaluateTransactionBalance
    , feeOfBytes
    , getFeePerByte
    , isBelowMinimumCoinForTxOut
    , maxScriptExecutionCost
    )
import Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( SelectionBalanceError (..)
    , SelectionCollateralError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionParams (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , WalletSelectionContext
    , WalletUTxO (..)
    , performSelection
    , toExternalUTxOMap
    , toInternalUTxOMap
    )
import Internal.Cardano.Write.Tx.Balance.Surplus
    ( ErrMoreSurplusNeeded (..)
    , TxFeeAndChange (..)
    , distributeSurplus
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
    ( TimelockKeyWitnessCounts (..)
    , estimateKeyWitnessCounts
    , estimateSignedTxMinFee
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
import Text.Pretty.Simple
    ( pShow
    )

import qualified Cardano.Address.Script as CA
import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Val as Val
    ( coin
    )
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Address as W
    ( Address
    )
import qualified Cardano.Wallet.Primitive.Types.AssetId as W
    ( AssetId (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W.Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W.TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
    ( TokenQuantity (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( TxSize (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (..)
    )
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
    ( UTxO (..)
    )
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.Strict.Extra as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Internal.Cardano.Write.Tx.Balance.CoinSelection as CoinSelection

-- | Indicates a failure to select a sufficient amount of collateral.
--
data ErrBalanceTxInsufficientCollateralError era =
    ErrBalanceTxInsufficientCollateralError
    { largestCombinationAvailable :: UTxO era
        -- ^ The largest available combination of pure ada UTxOs.
    , minimumCollateralAmount :: Coin
        -- ^ The minimum quantity of ada necessary for collateral.
    }
    deriving Generic

deriving instance IsRecentEra era =>
    Eq (ErrBalanceTxInsufficientCollateralError era)

deriving instance IsRecentEra era =>
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
    = ErrUnderestimatedFee Coin (Tx era) KeyWitnessCounts
    | ErrFailedBalancing Value

deriving instance IsRecentEra era => Eq (ErrBalanceTxInternalError era)
deriving instance IsRecentEra era => Show (ErrBalanceTxInternalError era)

-- | Errors that can occur when balancing transactions.
data ErrBalanceTx era
    = ErrBalanceTxAssetsInsufficient ErrBalanceTxAssetsInsufficientError
    | ErrBalanceTxMaxSizeLimitExceeded
    | ErrBalanceTxExistingKeyWitnesses Int
    -- ^ Indicates that a transaction could not be balanced because a given
    -- number of existing key witnesses would be rendered invalid.
    | ErrBalanceTxExistingCollateral
    | ErrBalanceTxExistingTotalCollateral
    | ErrBalanceTxExistingReturnCollateral
    | ErrBalanceTxInsufficientCollateral
        (ErrBalanceTxInsufficientCollateralError era)
    | ErrBalanceTxConflictingNetworks
    | ErrBalanceTxAssignRedeemers (ErrAssignRedeemers era)
    | ErrBalanceTxInternalError (ErrBalanceTxInternalError era)
    | ErrBalanceTxInputResolutionConflicts
        (NonEmpty (TxOut era, TxOut era))
    | ErrBalanceTxUnresolvedInputs (NESet TxIn)
    | ErrBalanceTxOutputError ErrBalanceTxOutputError
    | ErrBalanceTxUnableToCreateChange ErrBalanceTxUnableToCreateChangeError
    | ErrBalanceTxUnableToCreateInput
    -- ^ Returned when __both__ of the following conditions are true:
    --   - the given partial transaction has no existing inputs; and
    --   - the given UTxO index is empty.
    -- A transaction must have at least one input in order to be valid.
    deriving Generic

deriving instance IsRecentEra era => Eq (ErrBalanceTx era)
deriving instance IsRecentEra era => Show (ErrBalanceTx era)

-- | A 'PartialTx' is an an unbalanced transaction along with the necessary
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
    { tx :: Tx era
    , extraUTxO :: UTxO era
    , redeemers :: [Redeemer]
    , timelockKeyWitnessCounts :: TimelockKeyWitnessCounts
      -- ^ Specifying the intended number of timelock script key witnesses may
      -- save space and fees when constructing a transaction.
      --
      -- Timelock scripts without entries in this map will have their key
      -- witness counts estimated according to
      -- 'estimateMaxWitnessRequiredPerInput'.
    }
    deriving Generic

deriving instance IsRecentEra era => Eq (PartialTx era)
deriving instance IsRecentEra era => Show (PartialTx era)

instance IsRecentEra era => Buildable (PartialTx era)
  where
    build PartialTx {tx, extraUTxO, redeemers, timelockKeyWitnessCounts}
        = nameF "PartialTx" $ mconcat
            [ nameF "extraUTxO"
                (blockListF' "-" inF (Map.toList (unUTxO extraUTxO)))
            , nameF "redeemers" (pretty redeemers)
            , nameF "tx" (txF tx)
            , nameF "intended timelock key witness counts"
                $ blockListF' "-" (build . show)
                $ Map.toList
                $ getTimelockKeyWitnessCounts timelockKeyWitnessCounts
            ]
      where
        inF = build . show

        txF :: Tx era -> Builder
        txF tx' = pretty $ pShow tx'

data UTxOIndex era = UTxOIndex
    { availableUTxO :: !(UTxO era)
    -- ^ The set of UTxOs that are available to spend.
    , availableUTxOIndex :: !(UTxOIndex.UTxOIndex WalletUTxO)
    -- ^ The set of UTxOs that are available to spend, in indexed form.
    }

constructUTxOIndex
    :: IsRecentEra era
    => UTxO era
    -> UTxOIndex era
constructUTxOIndex availableUTxO =
    UTxOIndex {availableUTxO, availableUTxOIndex}
  where
    availableUTxOIndex =
        UTxOIndex.fromMap $ toInternalUTxOMap $ toWalletUTxO availableUTxO

fromWalletUTxO
    :: forall era. IsRecentEra era
    => W.UTxO
    -> UTxO era
fromWalletUTxO (W.UTxO m) = UTxO
    $ Map.mapKeys Convert.toLedger
    $ Map.map (toLedgerTxOut (recentEra @era)) m

toWalletUTxO
    :: forall era. IsRecentEra era
    => UTxO era
    -> W.UTxO
toWalletUTxO (UTxO m) = W.UTxO
    $ Map.mapKeys Convert.toWallet
    $ Map.map (toWalletTxOut (recentEra @era)) m

balanceTx
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => PParams era
    -- Protocol parameters. Can be retrieved via Local State Query to a
    -- local node.
    --
    -- If passed an incorrect value, a phase 1 script integrity hash mismatch
    -- will protect against collateral being forfeited.
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
    -- https://github.com/IntersectMBO/cardano-ledger/blob/fdec04e8c071060a003263cdcb37e7319fb4dbf3/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L428-L440
    -> UTxOAssumptions
    -> UTxOIndex era
    -> ChangeAddressGen changeState
    -> changeState
    -> PartialTx era
    -> ExceptT (ErrBalanceTx era) m (Tx era, changeState)
balanceTx
    pp
    timeTranslation
    utxoAssumptions
    UTxOIndex {availableUTxO, availableUTxOIndex}
    genChange
    changeState
    PartialTx {extraUTxO, tx, redeemers, timelockKeyWitnessCounts}
    = do
    guardExistingCollateral
    guardExistingReturnCollateral
    guardExistingTotalCollateral

    guardUTxOConsistency
    preselectedUTxOIndex <- indexPreselectedUTxO
    let utxoSelection =
            UTxOSelection.fromIndexPair
                (availableUTxOIndex, preselectedUTxOIndex)
    when (UTxOSelection.availableSize utxoSelection == 0) $
        throwE ErrBalanceTxUnableToCreateInput

    let adjustedPartialTx = assignMinimalAdaQuantitiesToOutputsWithoutAda pp tx
        balanceWith strategy =
            balanceTxInner
                pp
                timeTranslation
                utxoAssumptions
                utxoReference
                utxoSelection
                genChange
                changeState
                strategy
                redeemers
                timelockKeyWitnessCounts
                adjustedPartialTx
    balanceWith SelectionStrategyOptimal
        `catchE` \e ->
            if minimalStrategyIsWorthTrying e
            then balanceWith SelectionStrategyMinimal
            else throwE e
  where
    era = recentEra @era

    -- Creates an index of all UTxOs that are already spent as inputs of the
    -- partial transaction.
    --
    -- This function will fail if any of the inputs refers to a UTxO that
    -- cannot be found in the UTxO reference set.
    --
    indexPreselectedUTxO
        :: ExceptT (ErrBalanceTx era) m (UTxOIndex.UTxOIndex WalletUTxO)
    indexPreselectedUTxO
        | Just unresolvedTxIns <- maybeUnresolvedTxIns =
            throwE (ErrBalanceTxUnresolvedInputs unresolvedTxIns)
        | otherwise = pure $
            UTxOIndex.fromSequence (convertUTxO <$> Map.toList selectedUTxO)
      where
        convertUTxO :: (TxIn, TxOut era) -> (WalletUTxO, W.TokenBundle)
        convertUTxO (i, o) = (WalletUTxO (Convert.toWallet i) addr, bundle)
          where
            W.TxOut addr bundle = toWalletTxOut era o
        maybeUnresolvedTxIns :: Maybe (NESet TxIn)
        maybeUnresolvedTxIns =
            NESet.nonEmptySet $ txIns <\> Map.keysSet selectedUTxO
        selectedUTxO :: Map TxIn (TxOut era)
        selectedUTxO = Map.restrictKeys (unUTxO utxoReference) txIns
        txIns :: Set TxIn
        txIns = tx ^. bodyTxL . inputsTxBodyL

    -- The set of all UTxOs that may be referenced by a balanced transaction.
    --
    -- Note that when constructing this set, we give precedence to UTxOs
    -- provided as part of the 'PartialTx' object. This relies on the
    -- left-biased nature of the 'Semigroup' 'mappend' operation on UTxO sets.
    --
    utxoReference :: UTxO era
    utxoReference = mconcat [extraUTxO, availableUTxO]

    guardExistingCollateral :: ExceptT (ErrBalanceTx era) m ()
    guardExistingCollateral = do
        -- Coin selection does not support pre-defining collateral. In Sep 2021
        -- consensus was that we /could/ allow for it with just a day's work or
        -- so, but that the need for it was unclear enough that it was not in
        -- any way a priority.
        let collIns = tx ^. bodyTxL . collateralInputsTxBodyL
        unless (null collIns) $
            throwE ErrBalanceTxExistingCollateral

    guardExistingReturnCollateral :: ExceptT (ErrBalanceTx era) m ()
    guardExistingReturnCollateral = do
        let collRet = tx ^. bodyTxL . collateralReturnTxBodyL
        case collRet of
            SNothing -> return ()
            SJust _ -> throwE ErrBalanceTxExistingReturnCollateral

    guardExistingTotalCollateral :: ExceptT (ErrBalanceTx era) m ()
    guardExistingTotalCollateral = do
        let totColl = tx ^. bodyTxL . totalCollateralTxBodyL
        case totColl of
            SNothing -> return ()
            SJust _ -> throwE ErrBalanceTxExistingTotalCollateral

    -- | Ensures that the given UTxO sets are consistent with one another.
    --
    -- They are not consistent iff an input can be looked up in both UTxO sets
    -- with different @Address@, or @TokenBundle@ values.
    --
    guardUTxOConsistency :: ExceptT (ErrBalanceTx era) m ()
    guardUTxOConsistency =
        case NE.nonEmpty (F.toList (conflicts extraUTxO availableUTxO)) of
            Just cs -> throwE $ ErrBalanceTxInputResolutionConflicts cs
            Nothing -> return ()
      where
        conflicts :: UTxO era -> UTxO era -> Map TxIn (TxOut era, TxOut era)
        conflicts = Map.conflictsWith ((/=) `on` toWalletTxOut era) `on` unUTxO

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
    :: forall era. IsRecentEra era
    => PParams era
    -> Tx era
    -> Tx era
assignMinimalAdaQuantitiesToOutputsWithoutAda pp =
        over (bodyTxL . outputsTxBodyL) (fmap modifyTxOut)
  where
    modifyTxOut
        :: TxOut era
        -> TxOut era
    modifyTxOut out = flip (over coinTxOutL) out $ \c ->
        if c == mempty then computeMinimumCoinForTxOut pp out else c

-- | Internal helper to 'balanceTx'
balanceTxInner
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => PParams era
    -> TimeTranslation
    -> UTxOAssumptions
    -> UTxO era
    -- ^ The reference set of all UTxOs.
    -> UTxOSelection WalletUTxO
    -- ^ The set of UTxOs that may be spent by the resultant transaction.
    -- The subset of UTxOs that are already spent are pre-selected.
    -> ChangeAddressGen changeState
    -> changeState
    -> SelectionStrategy
    -> [Redeemer]
    -> TimelockKeyWitnessCounts
    -> Tx era
    -> ExceptT (ErrBalanceTx era) m (Tx era, changeState)
balanceTxInner
    pp
    timeTranslation
    utxoAssumptions
    utxoReference
    utxoSelection
    genChange
    changeState
    selectionStrategy
    redeemers
    timelockKeyWitnessCounts
    partialTx
    = do
    (balance0, minfee0, _) <- balanceAfterSettingMinFee partialTx

    -- NOTE: It is not possible to know the script execution cost in
    -- advance because it actually depends on the final transaction. Inputs
    -- selected as part of the fee balancing might have an influence on the
    -- execution cost.
    -- However, they are bounded so it is possible to balance the
    -- transaction considering only the maximum cost, and only after, try to
    -- adjust the change and ExUnits of each redeemer to something more
    -- sensible than the max execution cost.
    (selectAssetsResult, changeState')
        <- selectAssets
            pp
            utxoAssumptions
            (F.toList $ partialTx ^. bodyTxL . outputsTxBodyL)
            redeemers
            utxoSelection
            balance0
            (Convert.toWalletCoin minfee0)
            genChange
            selectionStrategy
            changeState
    let SelectAssetsResult
            { extraInputs
            , extraCollateral
            , extraOutputs
            , extraInputScripts
            }
            = selectAssetsResult

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

    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , extraInputScripts
        , feeUpdate = UseNewTxFee $ Convert.toWalletCoin minfee0
        }

    (balance, candidateMinFee, witCount) <-
        balanceAfterSettingMinFee candidateTx
    surplus <- case Val.coin balance of
        (Coin c)
            | c >= 0 ->
                pure $ W.Coin.unsafeFromIntegral c
            | otherwise ->
                throwE
                $ ErrBalanceTxInternalError
                $ ErrUnderestimatedFee
                    (Coin (-c))
                    candidateTx
                    witCount

    let feeAndChange = TxFeeAndChange
            (Convert.toWalletCoin candidateMinFee)
            (extraOutputs)
        feePerByte = getFeePerByte pp

    -- @distributeSurplus@ should never fail becase we have provided enough
    -- padding in @selectAssets@.
    TxFeeAndChange updatedFee updatedChange <- withExceptT
        (\(ErrMoreSurplusNeeded c) ->
            ErrBalanceTxInternalError
                $ ErrUnderestimatedFee
                    (Convert.toLedgerCoin c)
                    candidateTx
                    witCount
        )
        (ExceptT . pure $
            distributeSurplus feePerByte surplus feeAndChange)

    fmap ((, changeState')) . guardTxSize witCount
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
    guardTxSize
        :: KeyWitnessCounts
        -> Tx era
        -> ExceptT (ErrBalanceTx era) m (Tx era)
    guardTxSize witCount tx = do
        let maxSize =  W.TxSize $ intCast (pp ^. ppMaxTxSizeL)
        when (estimateSignedTxSize pp witCount tx > maxSize) $
            throwE ErrBalanceTxMaxSizeLimitExceeded
        pure tx

    guardTxBalanced
        :: Tx era
        -> ExceptT (ErrBalanceTx era) m (Tx era)
    guardTxBalanced tx = do
        let bal = txBalance tx
        if bal == mempty
            then pure tx
            else throwE
                $ ErrBalanceTxInternalError
                $ ErrFailedBalancing bal

    txBalance :: Tx era -> Value
    txBalance
        = evaluateTransactionBalance pp utxoReference
        . view bodyTxL

    balanceAfterSettingMinFee
        :: Tx era
        -> ExceptT (ErrBalanceTx era) m (Value, Coin, KeyWitnessCounts)
    balanceAfterSettingMinFee tx = ExceptT . pure $ do
        let witCount =
                estimateKeyWitnessCounts
                    utxoReference
                    tx
                    timelockKeyWitnessCounts
            minfee = Convert.toWalletCoin
                $ estimateSignedTxMinFee pp utxoReference tx witCount
            update = TxUpdate
                { extraInputs = mempty
                , extraCollateral = mempty
                , extraOutputs = []
                , extraInputScripts = []
                , feeUpdate = UseNewTxFee minfee
                }
        tx' <- left updateTxErrorToBalanceTxError $ updateTx tx update
        let balance = txBalance tx'
            minfee' = Convert.toLedgerCoin minfee
        return (balance, minfee', witCount)

    assembleTransaction
        :: TxUpdate
        -> ExceptT (ErrBalanceTx era) m (Tx era)
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left updateTxErrorToBalanceTxError
            $ updateTx partialTx update
        left ErrBalanceTxAssignRedeemers $
            assignScriptRedeemers pp timeTranslation utxoReference redeemers tx'

data SelectAssetsResult = SelectAssetsResult
    { extraInputs :: Set W.TxIn
    , extraCollateral :: Set W.TxIn
    , extraOutputs :: [W.TxOut]
    , extraInputScripts :: [CA.Script CA.KeyHash]
    } deriving (Eq, Show)

-- | Select assets to cover the specified balance and fee.
--
-- If the transaction contains redeemers, the function will also ensure the
-- selection covers the fees for the maximum allowed execution units of a
-- transaction. For this, and other reasons, the selection may include too
-- much ada.
selectAssets
    :: forall era m changeState.
        ( MonadRandom m
        , IsRecentEra era
        )
    => PParams era
    -> UTxOAssumptions
    -> [TxOut era]
    -> [Redeemer]
    -> UTxOSelection WalletUTxO
    -- ^ Specifies which UTxOs are pre-selected, and which UTxOs can be used as
    -- inputs or collateral.
    -> Value
    -- ^ Balance to cover.
    -> W.Coin
    -- ^ Current minimum fee (before selecting assets).
    -> ChangeAddressGen changeState
    -> SelectionStrategy
    -> changeState
    -> ExceptT (ErrBalanceTx era) m (SelectAssetsResult, changeState)
selectAssets pp utxoAssumptions outs' redeemers
    utxoSelection balance fee0 changeGen selectionStrategy changeState = do
        except validateTxOutputs'
        transformSelection <$> performSelection'
  where
    era = recentEra @era

    outs = map fromLedgerTxOut outs'

    fromLedgerTxOut :: TxOut era -> W.TxOut
    fromLedgerTxOut o = case era of
       RecentEraBabbage -> Convert.fromBabbageTxOut o
       RecentEraConway -> Convert.fromConwayTxOut o

    validateTxOutputs'
        :: Either (ErrBalanceTx era) ()
    validateTxOutputs'
        = left ErrBalanceTxOutputError
        $ validateTxOutputs selectionConstraints
            (outs <&> \out -> (view #address out, view #tokens out))

    performSelection'
        :: ExceptT (ErrBalanceTx era) m CoinSelection.Selection
    performSelection'
        = withExceptT coinSelectionErrorToBalanceTxError
        $ performSelection selectionConstraints selectionParams

    selectionConstraints = SelectionConstraints
        { tokenBundleSizeAssessor =
            mkTokenBundleSizeAssessor pp
        , computeMinimumAdaQuantity = \addr tokens -> Convert.toWallet $
            computeMinimumCoinForTxOut
                pp
                (mkLedgerTxOut era addr (W.TokenBundle W.txOutMaxCoin tokens))
        , isBelowMinimumAdaQuantity = \addr bundle ->
            isBelowMinimumCoinForTxOut pp (mkLedgerTxOut era addr bundle)
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
        , maximumCollateralInputCount =
            unsafeIntCast @Natural @Int $ pp ^. ppMaxCollateralInputsL
    , minimumCollateralPercentage =
        pp ^. ppCollateralPercentageL
    , maximumLengthChangeAddress =
        Convert.toWalletAddress changeGen.maxLengthChangeAddress
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
        (balanceNegative, balancePositive) = splitSignedValue balance
        valueOfOutputs = F.foldMap' (view #tokens) outs
        valueOfInputs = UTxOSelection.selectedBalance utxoSelection

    mkLedgerTxOut
        :: HasCallStack
        => RecentEra era
        -> W.Address
        -> W.TokenBundle
        -> TxOut era
    mkLedgerTxOut txOutEra address bundle =
        case txOutEra of
            RecentEraBabbage -> Convert.toBabbageTxOut txOut
            RecentEraConway -> Convert.toConwayTxOut txOut
          where
            txOut = W.TxOut address bundle

    txPlutusScriptExecutionCost = Convert.toWallet @W.Coin $
        if null redeemers
            then mempty
            else maxScriptExecutionCost pp

    collateralRequirement =
        if txPlutusScriptExecutionCost > W.Coin 0
            then SelectionCollateralRequired
            else SelectionCollateralNotRequired

    feePerByte = getFeePerByte pp

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
        -- in the final stage of 'balanceTx'.
        extraBytes = 8

    transformSelection
        :: CoinSelection.Selection
        -> (SelectAssetsResult, changeState)
    transformSelection sel =
        let
            (sel', changeState') =
                assignChangeAddresses changeGen sel changeState
            inputs = F.toList (sel' ^. #inputs)
            collateral = sel' ^. #collateral
            change = sel' ^. #change
            inputScripts =
                case utxoAssumptions of
                    AllKeyPaymentCredentials -> []
                    AllByronKeyPaymentCredentials -> []
                    AllScriptPaymentCredentialsFrom _template toInpScripts ->
                        ( toInpScripts
                        . Convert.toLedgerAddress
                        . view #address
                        . snd
                        ) <$> inputs <> collateral
            selectAssetsResult = SelectAssetsResult
                { extraInputs = Set.fromList (map fst inputs)
                -- TODO [ADP-3355] Filter out pre-selected inputs here
                --
                -- The correctness of balanceTx is currently not affected, but
                -- it is misleading.
                -- https://cardanofoundation.atlassian.net/browse/ADP-3355
                , extraCollateral = Set.fromList (map fst collateral)
                , extraOutputs = change
                , extraInputScripts = inputScripts
                }
        in
        (selectAssetsResult, changeState')

data ChangeAddressGen s = ChangeAddressGen
    {
    -- | Generates a new change address.
    --
    genChangeAddress :: s -> (Address, s)

    -- | Returns a /dummy/ change address of the maximum possible length for
    --   this generator.
    --
    -- Implementations must satisfy the following property:
    --
    -- @
    -- ∀ s. length (fst (genChangeAddress s)) <=
    --      length maxLengthChangeAddress
    -- @
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

-- | Assigns addresses to the change outputs of the given selection.
assignChangeAddresses
    :: ChangeAddressGen s
    -> CoinSelection.SelectionOf W.TokenBundle
    -> s
    -> (CoinSelection.SelectionOf W.TxOut, s)
assignChangeAddresses (ChangeAddressGen genChange _) sel = runState $ do
    changeOuts <- forM (view #change sel) $ \bundle -> do
        addr <- state genChange
        pure $ W.TxOut (Convert.toWalletAddress addr) bundle
    pure $ set #change changeOuts sel

unsafeIntCast
    :: (HasCallStack, Integral a, Integral b, Bits a, Bits b, Show a)
    => a
    -> b
unsafeIntCast x = fromMaybe err $ intCastMaybe x
  where
    err = error $ "unsafeIntCast failed for " <> show x

mergeSignedValue :: (W.TokenBundle, W.TokenBundle) -> Value
mergeSignedValue (bNegative, bPositive) = vNegative <> vPositive
  where
    vNegative = Convert.toLedger bNegative & invert
    vPositive = Convert.toLedger bPositive

splitSignedValue :: Value -> (W.TokenBundle, W.TokenBundle)
splitSignedValue v = (bNegative, bPositive)
  where
    bNegative = Convert.toWallet . filterPositive $ invert v
    bPositive = Convert.toWallet . filterPositive $        v

    filterPositive :: Value -> Value
    filterPositive (MaryValue (Coin a) (MultiAsset m)) =
        MaryValue (Coin aPositive) (MultiAsset mPositive)
      where
        aPositive = max a 0
        mPositive = Map.map (Map.filter (> 0)) m

--------------------------------------------------------------------------------
-- updateTx
--------------------------------------------------------------------------------

-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
    { extraInputs :: Set W.TxIn
    , extraCollateral :: Set W.TxIn
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
noTxUpdate = TxUpdate
    { extraInputs = mempty
    , extraCollateral = mempty
    , extraOutputs = []
    , extraInputScripts = []
    , feeUpdate = UseOldTxFee
    }

-- | Method to use when updating the fee of a transaction.
data TxFeeUpdate
    = UseOldTxFee
        -- ^ Instead of updating the fee, just use the old fee of the
        -- Tx (no-op for fee update).
    | UseNewTxFee W.Coin
        -- ^ Specify a new fee to use instead.
    deriving (Eq, Show)

newtype ErrUpdateTx
    = ErrUpdateTxExistingKeyWitnesses Int
    -- ^ The transaction could not be updated because the *n* existing
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
    :: forall era. IsRecentEra era
    => Tx era
    -> TxUpdate
    -> Either ErrUpdateTx (Tx era)
updateTx tx extraContent = do
    let tx' = tx
            & over bodyTxL (modifyShelleyTxBody extraContent)
            & over (witsTxL . scriptTxWitsL) (<> extraInputScripts')

    case numberOfExistingKeyWits of
        0 -> Right tx'
        n -> Left $ ErrUpdateTxExistingKeyWitnesses n
  where
    numberOfExistingKeyWits :: Int
    numberOfExistingKeyWits = sum
        [ Set.size $ tx ^. (witsTxL . addrTxWitsL)
        , Set.size $ tx ^. (witsTxL . bootAddrTxWitsL)
        ]

    TxUpdate _ _ _ extraInputScripts _ = extraContent

    extraInputScripts'
        :: Map (ScriptHash StandardCrypto) (Script era)
    extraInputScripts' =
        Map.fromList $ map (pairWithHash . convert) extraInputScripts
      where
        pairWithHash s = (hashScript s, s)
        convert = flip toLedgerScript (recentEra @era)

    toLedgerScript
        :: CA.Script CA.KeyHash
        -> RecentEra era
        -> Core.Script era
    toLedgerScript s = \case
        RecentEraBabbage -> TimelockScript $ Convert.toLedgerTimelockScript s
        RecentEraConway -> TimelockScript $ Convert.toLedgerTimelockScript s

modifyShelleyTxBody
    :: forall era. IsRecentEra era
    => TxUpdate
    -> TxBody era
    -> TxBody era
modifyShelleyTxBody txUpdate =
    over feeTxBodyL modifyFee
    . over outputsTxBodyL
        (<> extraOutputs')
    . over inputsTxBodyL
        (<> extraInputs')
    . over collateralInputsTxBodyL
        (<> extraCollateral')
  where
    era = recentEra @era
    TxUpdate {extraInputs, extraCollateral, extraOutputs, feeUpdate} = txUpdate
    extraOutputs' = StrictSeq.fromList $ map (toLedgerTxOut era) extraOutputs
    extraInputs' = Set.map Convert.toLedger extraInputs
    extraCollateral' = Set.map Convert.toLedger extraCollateral

    modifyFee old = case feeUpdate of
        UseNewTxFee c -> Convert.toLedger c
        UseOldTxFee -> old

toLedgerTxOut
    :: HasCallStack
    => RecentEra era
    -> W.TxOut
    -> TxOut era
toLedgerTxOut txOutEra txOut =
    case txOutEra of
        RecentEraBabbage -> Convert.toBabbageTxOut txOut
        RecentEraConway -> Convert.toConwayTxOut txOut

toWalletTxOut
    :: RecentEra era
    -> TxOut era
    -> W.TxOut
toWalletTxOut RecentEraBabbage = Convert.fromBabbageTxOut
toWalletTxOut RecentEraConway = Convert.fromConwayTxOut

-- | Maps an error from the coin selection API to a balanceTx error.
--
coinSelectionErrorToBalanceTxError
    :: forall era. IsRecentEra era
    => SelectionError WalletSelectionContext
    -> ErrBalanceTx era
coinSelectionErrorToBalanceTxError = \case
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
                & fromWalletUTxO
            , minimumCollateralAmount
                = Convert.toLedgerCoin minimumSelectionAmount
            }

updateTxErrorToBalanceTxError :: ErrUpdateTx -> ErrBalanceTx era
updateTxErrorToBalanceTxError = \case
    ErrUpdateTxExistingKeyWitnesses i -> ErrBalanceTxExistingKeyWitnesses i

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
        { minimumExpectedCoin :: Coin
        , output :: (Address, Value)
        }
    | ErrBalanceTxOutputSizeExceedsLimit
        { output :: (Address, Value)
        }
    | ErrBalanceTxOutputTokenQuantityExceedsLimit
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
    errors = uncurry ErrBalanceTxOutputErrorOf <$> F.fold
        [ mapMaybe (traverse (validateTxOutputSize constraints))
            outputsIndexed
        , foldMap (traverse validateTxOutputTokenQuantities)
            outputsIndexed
        , mapMaybe (traverse (validateTxOutputAdaQuantity constraints))
            outputsIndexed
        ]
      where
        outputsIndexed = zip [0 ..] outs

-- | Validates the size of a transaction output.
--
-- Returns an error if (and only if) the size exceeds the limit defined by the
-- protocol.
--
validateTxOutputSize
    :: SelectionConstraints
    -> (W.Address, W.TokenBundle)
    -> Maybe ErrBalanceTxOutputErrorInfo
validateTxOutputSize cs out@(address, bundle) = case sizeAssessment of
    TokenBundleSizeWithinLimit ->
        Nothing
    TokenBundleSizeExceedsLimit ->
        Just $
        ErrBalanceTxOutputSizeExceedsLimit
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
    -> [ErrBalanceTxOutputErrorInfo]
validateTxOutputTokenQuantities out =
    [ ErrBalanceTxOutputTokenQuantityExceedsLimit
        {address, policyId, assetName, quantity, quantityMaxBound}
    | let address = Convert.toLedgerAddress $ fst out
    , (W.AssetId p a, W.TokenQuantity quantity) <-
        W.TokenMap.toFlatList $ (snd out) ^. #tokens
    , let (policyId, assetName) = (Convert.toLedger p, Convert.toLedger a)
    , quantity > quantityMaxBound
    ]
  where
    quantityMaxBound = W.unTokenQuantity W.txOutMaxTokenQuantity

-- | Validates the ada quantity associated with a transaction output.
--
-- An output's ada quantity must be greater than or equal to the minimum
-- required quantity for that output.
--
validateTxOutputAdaQuantity
    :: SelectionConstraints
    -> (W.Address, W.TokenBundle)
    -> Maybe ErrBalanceTxOutputErrorInfo
validateTxOutputAdaQuantity constraints output@(address, bundle)
    | isBelowMinimum =
        Just ErrBalanceTxOutputAdaQuantityInsufficient
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
