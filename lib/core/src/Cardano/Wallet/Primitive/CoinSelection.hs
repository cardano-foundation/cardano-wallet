{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a high-level interface for coin selection.
--
-- It handles the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- Use the 'performSelection' function to perform a coin selection.
--
module Cardano.Wallet.Primitive.CoinSelection
    (
    -- * Performing selections
      performSelection
    , Selection
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionParams (..)

    -- * Output preparation
    , prepareOutputsWith
    , SelectionOutputInvalidError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)

    -- * Selection correctness
    , SelectionCorrectness (..)
    , verifySelection

    -- * Selection deltas
    , SelectionDelta (..)
    , selectionDelta
    , selectionDeltaAllAssets
    , selectionDeltaCoin
    , selectionHasValidSurplus
    , selectionMinimumCost

    -- * Selection collateral
    , SelectionCollateralRequirement (..)
    , selectionCollateral
    , selectionCollateralRequired
    , selectionHasSufficientCollateral
    , selectionMinimumCollateral

    -- * Selection reports
    , SelectionReport (..)
    , SelectionReportSummarized (..)
    , SelectionReportDetailed (..)
    , makeSelectionReport
    , makeSelectionReportSummarized
    , makeSelectionReportDetailed

    -- * Internal types and functions
    , ComputeMinimumCollateralParams (..)
    , computeMinimumCollateral
    , toBalanceConstraintsParams

    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionDelta (..), SelectionLimit, SelectionSkeleton )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..), TxIn, TxOut, txOutMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Control.Monad
    ( (<=<) )
import Control.Monad.Random.Class
    ( MonadRandom )
import Control.Monad.Trans.Except
    ( ExceptT (..), withExceptT )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over, set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isNothing, mapMaybe )
import Data.Ratio
    ( (%) )
import Data.Semigroup
    ( mtimesDefault )
import Fmt
    ( Buildable (..), genericF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.CoinSelection.Collateral as Collateral
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Provides a context for functions related to 'performSelection'.

type PerformSelection m a =
    SelectionConstraints ->
    SelectionParams ->
    ExceptT SelectionError m a

-- | Performs a coin selection.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- This function guarantees that if it successfully creates a 'Selection' @s@,
-- given a set of 'SelectionConstraints' @cs@ and 'SelectionParameters' @ps@,
-- then the following property will hold:
--
--    >>> verifySelection cs ps s == SelectionCorrect
--
performSelection
    :: (HasCallStack, MonadRandom m) => PerformSelection m Selection
performSelection cs = performSelectionInner cs <=< prepareOutputs cs

performSelectionInner
    :: (HasCallStack, MonadRandom m) => PerformSelection m Selection
performSelectionInner cs ps = do
    balanceResult <- performSelectionBalance cs ps
    collateralResult <- performSelectionCollateral balanceResult cs ps
    pure $ mkSelection ps balanceResult collateralResult

prepareOutputs
    :: Applicative m
    => PerformSelection m SelectionParams
prepareOutputs cs ps =
    withExceptT SelectionOutputError $ ExceptT $ pure $
    flip (set #outputsToCover) ps <$>
    prepareOutputsInternal cs (view #outputsToCover ps)

performSelectionBalance
    :: (HasCallStack, MonadRandom m)
    => PerformSelection m Balance.SelectionResult
performSelectionBalance cs ps =
    withExceptT SelectionBalanceError $ ExceptT $
    uncurry Balance.performSelection $ toBalanceConstraintsParams (cs, ps)

performSelectionCollateral
    :: Applicative m
    => Balance.SelectionResult
    -> PerformSelection m Collateral.SelectionResult
performSelectionCollateral balanceResult cs ps
    | selectionCollateralRequired ps =
        withExceptT SelectionCollateralError $ ExceptT $ pure $
        uncurry Collateral.performSelection $
        toCollateralConstraintsParams balanceResult (cs, ps)
    | otherwise =
        ExceptT $ pure $ Right Collateral.selectionResultEmpty

-- | Creates constraints and parameters for 'Balance.performSelection'.
--
toBalanceConstraintsParams
    :: (        SelectionConstraints,         SelectionParams)
    -> (Balance.SelectionConstraints, Balance.SelectionParams)
toBalanceConstraintsParams (constraints, params) =
    (balanceConstraints, balanceParams)
  where
    balanceConstraints = Balance.SelectionConstraints
        { computeMinimumAdaQuantity =
            view #computeMinimumAdaQuantity constraints
        , computeMinimumCost =
            view #computeMinimumCost constraints
                & adjustComputeMinimumCost
        , computeSelectionLimit =
            view #computeSelectionLimit constraints
                & adjustComputeSelectionLimit
        , assessTokenBundleSize =
            view #assessTokenBundleSize constraints
        }
      where
        adjustComputeMinimumCost
            :: (SelectionSkeleton -> Coin)
            -> (SelectionSkeleton -> Coin)
        adjustComputeMinimumCost =
            whenCollateralRequired params (. adjustSelectionSkeleton)
          where
            -- When collateral is required, we reserve space for collateral
            -- inputs ahead of time by adding the maximum allowed number of
            -- collateral inputs (defined by 'maximumCollateralInputCount')
            -- to the skeleton input count.
            --
            -- This ensures that the collateral inputs are already paid for
            -- when 'Balance.performSelection' is generating change outputs.
            --
            -- In many cases, the maximum allowed number of collateral inputs
            -- will be greater than the number eventually required, which will
            -- lead to a fee that is slightly higher than necessary.
            --
            -- However, since the maximum number of collateral inputs is very
            -- small, and since the marginal cost of a single extra input is
            -- relatively small, this fee increase is likely to be very small.
            --
            adjustSelectionSkeleton :: SelectionSkeleton -> SelectionSkeleton
            adjustSelectionSkeleton = over #skeletonInputCount
                (+ view #maximumCollateralInputCount constraints)

        adjustComputeSelectionLimit
            :: ([TxOut] -> SelectionLimit)
            -> ([TxOut] -> SelectionLimit)
        adjustComputeSelectionLimit =
            whenCollateralRequired params (fmap adjustSelectionLimit)
          where
            -- When collateral is required, we reserve space for collateral
            -- inputs ahead of time by subtracting the maximum allowed number
            -- of collateral inputs (defined by 'maximumCollateralInputCount')
            -- from the selection limit.
            --
            -- This ensures that when we come to perform collateral selection,
            -- there is still space available.
            --
            adjustSelectionLimit :: SelectionLimit -> SelectionLimit
            adjustSelectionLimit = flip Balance.reduceSelectionLimitBy
                (view #maximumCollateralInputCount constraints)

    balanceParams = Balance.SelectionParams
        { assetsToBurn =
            view #assetsToBurn params
        , assetsToMint =
            view #assetsToMint params
        , extraCoinSource =
            view #rewardWithdrawal params <>
            mtimesDefault
                (view #certificateDepositsReturned params)
                (view #certificateDepositAmount constraints)
        , extraCoinSink =
            mtimesDefault
                (view #certificateDepositsTaken params)
                (view #certificateDepositAmount constraints)
        , outputsToCover =
            view #outputsToCover params
        , utxoAvailable =
            view #utxoAvailableForInputs params
        }

-- | Creates constraints and parameters for 'Collateral.performSelection'.
--
toCollateralConstraintsParams
    :: Balance.SelectionResult
    -> (           SelectionConstraints,            SelectionParams)
    -> (Collateral.SelectionConstraints, Collateral.SelectionParams)
toCollateralConstraintsParams balanceResult (constraints, params) =
    (collateralConstraints, collateralParams)
  where
    collateralConstraints = Collateral.SelectionConstraints
        { maximumSelectionSize =
            view #maximumCollateralInputCount constraints
        , searchSpaceLimit =
            -- We use the default search space limit here, as this value is
            -- used in the test suite for 'Collateral.performSelection'. We
            -- can therefore be reasonably confident that the process of
            -- selecting collateral will not use inordinate amounts of time
            -- and space:
            Collateral.searchSpaceLimitDefault
        }
    collateralParams = Collateral.SelectionParams
        { coinsAvailable =
            Map.mapMaybeWithKey
                (curry (view #utxoSuitableForCollateral constraints))
                (unUTxO (view #utxoAvailableForCollateral params))
        , minimumSelectionAmount =
            computeMinimumCollateral ComputeMinimumCollateralParams
                { minimumCollateralPercentage =
                    view #minimumCollateralPercentage constraints
                , transactionFee =
                    Balance.selectionSurplusCoin balanceResult
                }
        }

-- | Creates a 'Selection' from selections of inputs and collateral.
--
mkSelection
    :: SelectionParams
    -> Balance.SelectionResult
    -> Collateral.SelectionResult
    -> Selection
mkSelection params balanceResult collateralResult = Selection
    { inputs = view #inputsSelected balanceResult
    , collateral = UTxO.toList $
        view #utxoAvailableForCollateral params
        `UTxO.restrictedBy`
        Map.keysSet (view #coinsSelected collateralResult)
    , outputs = view #outputsCovered balanceResult
    , change = view #changeGenerated balanceResult
    , assetsToMint = view #assetsToMint balanceResult
    , assetsToBurn = view #assetsToBurn balanceResult
    , extraCoinSource = view #extraCoinSource balanceResult
    , extraCoinSink = view #extraCoinSink balanceResult
    }

-- | Converts a 'Selection' to a balance result.
--
toBalanceResult :: Selection -> Balance.SelectionResult
toBalanceResult selection = Balance.SelectionResult
    { inputsSelected = view #inputs selection
    , outputsCovered = view #outputs selection
    , changeGenerated = view #change selection
    , assetsToMint = view #assetsToMint selection
    , assetsToBurn = view #assetsToBurn selection
    , extraCoinSource = view #extraCoinSource selection
    , extraCoinSink = view #extraCoinSink selection
    }

--------------------------------------------------------------------------------
-- Selection correctness
--------------------------------------------------------------------------------

-- | Indicates whether or not a selection is correct.
--
data SelectionCorrectness
    = SelectionCorrect
    | SelectionIncorrect SelectionCorrectnessError
    deriving (Eq, Show)

-- | Indicates that a selection is incorrect.
--
data SelectionCorrectnessError
    = SelectionCollateralInsufficient
      SelectionCollateralInsufficientError
    | SelectionCollateralUnsuitable
      SelectionCollateralUnsuitableError
    | SelectionDeltaInvalid
      SelectionDeltaInvalidError
    | SelectionLimitExceeded
      SelectionLimitExceededError
    deriving (Eq, Show)

-- | The type of all selection property verification functions.
--
type VerifySelectionProperty error =
    SelectionConstraints ->
    SelectionParams ->
    Selection ->
    Maybe error

-- | Verifies a selection for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a selection is incorrect in some way.
--
verifySelection
    :: SelectionConstraints
    -> SelectionParams
    -> Selection
    -> SelectionCorrectness
verifySelection cs ps selection =
    either SelectionIncorrect (const SelectionCorrect) verifyAll
  where
    verifyAll :: Either SelectionCorrectnessError ()
    verifyAll = do
        verifySelectionCollateralSufficiency cs ps selection
            `failWith` SelectionCollateralInsufficient
        verifySelectionCollateralSuitability cs ps selection
            `failWith` SelectionCollateralUnsuitable
        verifySelectionDelta cs ps selection
            `failWith` SelectionDeltaInvalid
        verifySelectionLimit cs ps selection
            `failWith` SelectionLimitExceeded

    failWith :: Maybe e1 -> (e1 -> e2) -> Either e2 ()
    onError `failWith` thisError = maybe (Right ()) (Left . thisError) onError

--------------------------------------------------------------------------------
-- Selection correctness: collateral sufficiency
--------------------------------------------------------------------------------

data SelectionCollateralInsufficientError = SelectionCollateralInsufficientError
    { collateralSelected :: Coin
    , collateralRequired :: Coin
    }
    deriving (Eq, Show)

verifySelectionCollateralSufficiency
    :: VerifySelectionProperty SelectionCollateralInsufficientError
verifySelectionCollateralSufficiency cs ps selection
    | collateralSelected >= collateralRequired =
        Nothing
    | otherwise =
        Just SelectionCollateralInsufficientError
            {collateralSelected, collateralRequired}
  where
    collateralSelected = selectionCollateral selection
    collateralRequired = selectionMinimumCollateral cs ps selection

--------------------------------------------------------------------------------
-- Selection correctness: collateral suitability
--------------------------------------------------------------------------------

data SelectionCollateralUnsuitableError = SelectionCollateralUnsuitableError
    { collateralSelected
        :: [(TxIn, TxOut)]
    , collateralSelectedButUnsuitable
        :: [(TxIn, TxOut)]
    }
    deriving (Eq, Show)

verifySelectionCollateralSuitability
    :: VerifySelectionProperty SelectionCollateralUnsuitableError
verifySelectionCollateralSuitability cs _ps selection
    | null collateralSelectedButUnsuitable =
        Nothing
    | otherwise =
        Just SelectionCollateralUnsuitableError
            {collateralSelected, collateralSelectedButUnsuitable}
  where
    collateralSelected =
        selection ^. #collateral
    collateralSelectedButUnsuitable =
        filter utxoUnsuitableForCollateral collateralSelected

    utxoUnsuitableForCollateral :: (TxIn, TxOut) -> Bool
    utxoUnsuitableForCollateral = isNothing . (cs ^. #utxoSuitableForCollateral)

--------------------------------------------------------------------------------
-- Selection correctness: delta validity
--------------------------------------------------------------------------------

data SelectionDeltaInvalidError = SelectionDeltaInvalidError
    { delta
        :: SelectionDelta TokenBundle
    , minimumCost
        :: Coin
    }
    deriving (Eq, Show)

verifySelectionDelta
    :: VerifySelectionProperty SelectionDeltaInvalidError
verifySelectionDelta cs ps selection
    | selectionHasValidSurplus cs ps selection =
        Nothing
    | otherwise =
        Just SelectionDeltaInvalidError {..}
  where
    delta = selectionDeltaAllAssets selection
    minimumCost = selectionMinimumCost cs ps selection

--------------------------------------------------------------------------------
-- Selection correctness: selection limit
--------------------------------------------------------------------------------

data SelectionLimitExceededError = SelectionLimitExceededError
    { collateralInputCount
        :: Int
    , ordinaryInputCount
        :: Int
    , totalInputCount
        :: Int
    , selectionLimit
        :: SelectionLimit
    }
    deriving (Eq, Show)

verifySelectionLimit
    :: VerifySelectionProperty SelectionLimitExceededError
verifySelectionLimit cs _ps selection
    | Balance.MaximumInputLimit totalInputCount <= selectionLimit =
        Nothing
    | otherwise =
        Just SelectionLimitExceededError {..}
  where
    collateralInputCount = length (selection ^. #collateral)
    ordinaryInputCount = length (selection ^. #inputs)
    totalInputCount = collateralInputCount + ordinaryInputCount
    selectionLimit = (cs ^. #computeSelectionLimit) (selection ^. #outputs)

--------------------------------------------------------------------------------
-- Selection deltas
--------------------------------------------------------------------------------

-- | Computes the ada surplus of a selection, assuming there is a surplus.
--
-- This function is a convenient synonym for 'selectionSurplusCoin' that is
-- polymorphic over the type of change.
--
selectionDelta
    :: (change -> Coin)
    -- ^ A function to extract the coin value from a change value.
    -> SelectionOf change
    -> Coin
selectionDelta getChangeCoin selection
    = selectionSurplusCoin
    $ selection & over #change (fmap $ TokenBundle.fromCoin . getChangeCoin)

-- | Calculates the selection delta for all assets.
--
-- See 'SelectionDelta'.
--
selectionDeltaAllAssets :: Selection -> SelectionDelta TokenBundle
selectionDeltaAllAssets = Balance.selectionDeltaAllAssets . toBalanceResult

-- | Calculates the ada selection delta.
--
-- See 'SelectionDelta'.
--
selectionDeltaCoin :: Selection -> SelectionDelta Coin
selectionDeltaCoin = fmap TokenBundle.getCoin . selectionDeltaAllAssets

-- | Indicates whether or not a selection has a valid surplus.
--
-- This function returns 'True' if and only if the selection has a delta that
-- is a *surplus*, and that surplus is greater than or equal to the result of
-- 'selectionMinimumCost'.
--
-- See 'SelectionDelta'.
--
selectionHasValidSurplus
    :: SelectionConstraints
    -> SelectionParams
    -> Selection
    -> Bool
selectionHasValidSurplus constraints params selection =
    Balance.selectionHasValidSurplus
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Computes the minimum required cost of a selection.
--
selectionMinimumCost
    :: SelectionConstraints
    -> SelectionParams
    -> Selection
    -> Coin
selectionMinimumCost constraints params selection =
    Balance.selectionMinimumCost
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Calculates the ada selection surplus, assuming there is a surplus.
--
-- If there is a surplus, then this function returns that surplus.
-- If there is a deficit, then this function returns zero.
--
-- Use 'selectionDeltaCoin' if you wish to handle the case where there is
-- a deficit.
--
selectionSurplusCoin :: Selection -> Coin
selectionSurplusCoin = Balance.selectionSurplusCoin . toBalanceResult

--------------------------------------------------------------------------------
-- Selection collateral
--------------------------------------------------------------------------------

-- | Indicates the collateral requirement for a selection.
--
data SelectionCollateralRequirement
    = SelectionCollateralRequired
    -- ^ Indicates that collateral is required.
    | SelectionCollateralNotRequired
    -- ^ Indicates that collateral is not required.
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Indicates 'True' if and only if collateral is required.
--
selectionCollateralRequired :: SelectionParams -> Bool
selectionCollateralRequired params = case view #collateralRequirement params of
    SelectionCollateralRequired    -> True
    SelectionCollateralNotRequired -> False

-- | Applies the given transformation function only when collateral is required.
--
whenCollateralRequired
    :: SelectionParams
    -> (a -> a)
    -> (a -> a)
whenCollateralRequired params f
    | selectionCollateralRequired params = f
    | otherwise = id

-- | Computes the total amount of collateral within a selection.
--
selectionCollateral :: Selection -> Coin
selectionCollateral selection =
    F.foldMap
        (view (#tokens . #coin) . snd)
        (view #collateral selection)

-- | Indicates whether or not a selection has sufficient collateral.
--
selectionHasSufficientCollateral
    :: SelectionConstraints
    -> SelectionParams
    -> Selection
    -> Bool
selectionHasSufficientCollateral constraints params selection =
    actual >= required
  where
    actual = selectionCollateral selection
    required = selectionMinimumCollateral constraints params selection

-- | Computes the minimum required amount of collateral for a selection.
--
selectionMinimumCollateral
    :: SelectionConstraints
    -> SelectionParams
    -> Selection
    -> Coin
selectionMinimumCollateral constraints params selection
    | selectionCollateralRequired params =
        view #minimumSelectionAmount $ snd $
        toCollateralConstraintsParams
            (toBalanceResult selection)
            (constraints, params)
    | otherwise = Coin 0

-- | Parameters for 'computeMinimumCollateral'.

data ComputeMinimumCollateralParams = ComputeMinimumCollateralParams
    { minimumCollateralPercentage :: Natural
    , transactionFee :: Coin
    }
    deriving (Eq, Generic, Show)

-- | Computes the minimum required amount of collateral given a fee and a
--   minimum collateral percentage.
--
computeMinimumCollateral
    :: ComputeMinimumCollateralParams
    -> Coin
computeMinimumCollateral params =
    Coin . ceiling . (% 100) . unCoin $
    mtimesDefault
        (view #minimumCollateralPercentage params)
        (view #transactionFee params)

-- | Specifies all constraints required for coin selection.
--
-- Selection constraints:
--
--    - place limits on the coin selection algorithm, enabling it to produce
--      selections that are acceptable to the ledger.
--
--    - are dependent on the current set of protocol parameters.
--
--    - are not specific to a given selection.
--
data SelectionConstraints = SelectionConstraints
    { assessTokenBundleSize
        :: TokenBundle -> TokenBundleSizeAssessment
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output. See documentation for
        -- the 'TokenBundleSizeAssessor' type to learn about the expected
        -- properties of this field.
    , certificateDepositAmount
        :: Coin
        -- ^ Amount that should be taken from/returned back to the wallet for
        -- each stake key registration/de-registration in the transaction.
    , computeMinimumAdaQuantity
        :: TokenMap -> Coin
        -- ^ Computes the minimum ada quantity required for a given output.
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , computeSelectionLimit
        :: [TxOut] -> SelectionLimit
        -- ^ Computes an upper bound for the number of ordinary inputs to
        -- select, given a current set of outputs.
    , maximumCollateralInputCount
        :: Int
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
    , minimumCollateralPercentage
        :: Natural
        -- ^ Specifies the minimum required amount of collateral as a
        -- percentage of the total transaction fee.
    , utxoSuitableForCollateral
        :: (TxIn, TxOut) -> Maybe Coin
        -- ^ Indicates whether an individual UTxO entry is suitable for use as
        -- a collateral input. This function should return a 'Coin' value if
        -- (and only if) the given UTxO is suitable for use as collateral.
    }
    deriving Generic

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams = SelectionParams
    { assetsToBurn
        :: !TokenMap
        -- ^ Specifies a set of assets to burn.
    , assetsToMint
        :: !TokenMap
        -- ^ Specifies a set of assets to mint.
    , outputsToCover
        :: ![TxOut]
        -- ^ Specifies a set of outputs that must be paid for.
    , rewardWithdrawal
        :: !Coin
        -- ^ Specifies the value of a withdrawal from a reward account.
    , certificateDepositsTaken
        :: !Natural
        -- ^ Number of deposits for stake key registrations.
    , certificateDepositsReturned
        :: !Natural
        -- ^ Number of deposits from stake key de-registrations.
    , collateralRequirement
        :: !SelectionCollateralRequirement
        -- ^ Specifies the collateral requirement for this selection.
    , utxoAvailableForCollateral
        :: !UTxO
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- collateral inputs.
        --
        -- This set is allowed to intersect with 'utxoAvailableForInputs',
        -- since the ledger does not require that these sets are disjoint.
    , utxoAvailableForInputs
        :: !UTxOSelection
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- ordinary inputs and optionally, a subset that has already been
        -- selected.
        --
        -- Further entries from this set will be selected to cover any deficit.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that an error occurred while performing a coin selection.
--
data SelectionError
    = SelectionBalanceError
        Balance.SelectionError
    | SelectionCollateralError
        Collateral.SelectionError
    | SelectionOutputError
        SelectionOutputInvalidError
    deriving (Eq, Show)

-- | Represents a balanced selection.
--
data SelectionOf change = Selection
    { inputs
        :: !(NonEmpty (TxIn, TxOut))
        -- ^ Selected inputs.
    , collateral
        :: ![(TxIn, TxOut)]
        -- ^ Selected collateral inputs.
    , outputs
        :: ![TxOut]
        -- ^ User-specified outputs
    , change
        :: ![change]
        -- ^ Generated change outputs.
    , assetsToMint
        :: !TokenMap
        -- ^ Assets to mint.
    , assetsToBurn
        :: !TokenMap
        -- ^ Assets to burn.
    , extraCoinSource
        :: !Coin
        -- ^ An extra source of ada.
    , extraCoinSink
        :: !Coin
        -- ^ An extra sink for ada.
    }
    deriving (Generic, Eq, Show)

-- | The default type of selection.
--
-- In this type of selection, change values do not have addresses assigned.
--
type Selection = SelectionOf TokenBundle

--------------------------------------------------------------------------------
-- Preparing outputs
--------------------------------------------------------------------------------

-- | Prepares the given user-specified outputs, ensuring that they are valid.
--
prepareOutputsInternal
    :: SelectionConstraints
    -> [TxOut]
    -> Either SelectionOutputInvalidError [TxOut]
prepareOutputsInternal constraints outputsUnprepared
    | e : _ <- excessivelyLargeBundles =
        Left $
        -- We encountered one or more excessively large token bundles.
        -- Just report the first such bundle:
        SelectionOutputSizeExceedsLimit e
    | e : _ <- excessiveTokenQuantities =
        Left $
        -- We encountered one or more excessive token quantities.
        -- Just report the first such quantity:
        SelectionOutputTokenQuantityExceedsLimit e
    | otherwise =
        pure outputsToCover
  where
    SelectionConstraints
        { computeMinimumAdaQuantity
        } = constraints

    -- The complete list of token bundles whose serialized lengths are greater
    -- than the limit of what is allowed in a transaction output:
    excessivelyLargeBundles :: [SelectionOutputSizeExceedsLimitError]
    excessivelyLargeBundles =
        mapMaybe (verifyOutputSize constraints) outputsToCover

    -- The complete list of token quantities that exceed the maximum quantity
    -- allowed in a transaction output:
    excessiveTokenQuantities :: [SelectionOutputTokenQuantityExceedsLimitError]
    excessiveTokenQuantities = verifyOutputTokenQuantities =<< outputsToCover

    outputsToCover =
        prepareOutputsWith computeMinimumAdaQuantity outputsUnprepared

-- | Transforms a set of outputs (provided by users) into valid Cardano outputs.
--
-- Every output in Cardano needs to have a minimum quantity of ada, in order to
-- prevent attacks that flood the network with worthless UTxOs.
--
-- However, we do not require users to specify the minimum ada quantity
-- themselves. Most users would prefer to send '10 Apple' rather than
-- '10 Apple & 1.2 Ada'.
--
-- Therefore, unless a coin quantity is explicitly specified, we assign a coin
-- quantity manually for each non-ada output. That quantity is the minimum
-- quantity required to make a particular output valid.
--
prepareOutputsWith :: Functor f => (TokenMap -> Coin) -> f TxOut -> f TxOut
prepareOutputsWith minCoinValueFor =
    fmap $ over #tokens augmentBundle
  where
    augmentBundle :: TokenBundle -> TokenBundle
    augmentBundle bundle
        | TokenBundle.getCoin bundle == Coin 0 =
            bundle & set #coin (minCoinValueFor (view #tokens bundle))
        | otherwise =
            bundle

-- | Indicates a problem when preparing outputs for a coin selection.
--
data SelectionOutputInvalidError
    = SelectionOutputSizeExceedsLimit
        SelectionOutputSizeExceedsLimitError
    | SelectionOutputTokenQuantityExceedsLimit
        SelectionOutputTokenQuantityExceedsLimitError
    deriving (Eq, Generic, Show)

data SelectionOutputSizeExceedsLimitError =
    SelectionOutputSizeExceedsLimitError
    { address :: !Address
      -- ^ The address to which this token bundle was to be sent.
    , assetCount :: !Int
      -- ^ The number of assets within the token bundle.
    }
    deriving (Eq, Generic, Show)

-- | Verifies the size of an output.
--
-- Returns 'SelectionOutputSizeExceedsLimitError' if and only if the size
-- exceeds the limit defined by the protocol.
--
verifyOutputSize
    :: SelectionConstraints
    -> TxOut
    -> Maybe SelectionOutputSizeExceedsLimitError
verifyOutputSize cs out
    | withinLimit =
        Nothing
    | otherwise =
        Just SelectionOutputSizeExceedsLimitError
            { address = out ^. #address
            , assetCount = TokenMap.size (out ^. (#tokens . #tokens))
            }
  where
    withinLimit :: Bool
    withinLimit =
        case (cs ^. #assessTokenBundleSize) (out ^. #tokens) of
            TokenBundleSizeWithinLimit -> True
            OutputTokenBundleSizeExceedsLimit -> False

-- | Indicates that a token quantity exceeds the maximum quantity that can
--   appear in a transaction output's token bundle.
--
data SelectionOutputTokenQuantityExceedsLimitError =
    SelectionOutputTokenQuantityExceedsLimitError
    { address :: !Address
      -- ^ The address to which this token quantity was to be sent.
    , asset :: !AssetId
      -- ^ The asset identifier to which this token quantity corresponds.
    , quantity :: !TokenQuantity
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: !TokenQuantity
      -- ^ The maximum allowable token quantity.
    }
    deriving (Eq, Generic, Show)

-- | Verifies the token quantities of an output.
--
-- Returns a list of token quantities that exceed the limit defined by the
-- protocol.
--
verifyOutputTokenQuantities
    :: TxOut -> [SelectionOutputTokenQuantityExceedsLimitError]
verifyOutputTokenQuantities out =
    [ SelectionOutputTokenQuantityExceedsLimitError
        {address, asset, quantity, quantityMaxBound = txOutMaxTokenQuantity}
    | let address = out ^. #address
    , (asset, quantity) <- TokenMap.toFlatList $ out ^. #tokens . #tokens
    , quantity > txOutMaxTokenQuantity
    ]

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | Includes both summarized and detailed information about a selection.
--
data SelectionReport = SelectionReport
    { summary :: SelectionReportSummarized
    , detail :: SelectionReportDetailed
    }
    deriving (Eq, Generic, Show)

-- | Includes summarized information about a selection.
--
-- Each data point can be serialized as a single line of text.
--
data SelectionReportSummarized = SelectionReportSummarized
    { computedFee :: Coin
    , adaBalanceOfSelectedInputs :: Coin
    , adaBalanceOfExtraCoinSource :: Coin
    , adaBalanceOfExtraCoinSink :: Coin
    , adaBalanceOfRequestedOutputs :: Coin
    , adaBalanceOfGeneratedChangeOutputs :: Coin
    , numberOfSelectedInputs :: Int
    , numberOfSelectedCollateralInputs :: Int
    , numberOfRequestedOutputs :: Int
    , numberOfGeneratedChangeOutputs :: Int
    , numberOfUniqueNonAdaAssetsInSelectedInputs :: Int
    , numberOfUniqueNonAdaAssetsInRequestedOutputs :: Int
    , numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs :: Int
    }
    deriving (Eq, Generic, Show)

-- | Includes detailed information about a selection.
--
data SelectionReportDetailed = SelectionReportDetailed
    { selectedInputs :: [(TxIn, TxOut)]
    , selectedCollateral :: [(TxIn, TxOut)]
    , requestedOutputs :: [TxOut]
    , generatedChangeOutputs :: [TokenBundle.Flat TokenBundle]
    }
    deriving (Eq, Generic, Show)

instance Buildable SelectionReport where
    build = genericF
instance Buildable SelectionReportSummarized where
    build = genericF
instance Buildable SelectionReportDetailed where
    build = genericF

makeSelectionReport :: Selection -> SelectionReport
makeSelectionReport s = SelectionReport
    { summary = makeSelectionReportSummarized s
    , detail = makeSelectionReportDetailed s
    }

makeSelectionReportSummarized :: Selection -> SelectionReportSummarized
makeSelectionReportSummarized s = SelectionReportSummarized {..}
  where
    computedFee
        = selectionDelta TokenBundle.getCoin s
    adaBalanceOfSelectedInputs
        = F.foldMap (view (#tokens . #coin) . snd) $ view #inputs s
    adaBalanceOfExtraCoinSource
        = view #extraCoinSource s
    adaBalanceOfExtraCoinSink
        = view #extraCoinSink s
    adaBalanceOfGeneratedChangeOutputs
        = F.foldMap (view #coin) $ view #change s
    adaBalanceOfRequestedOutputs
        = F.foldMap (view (#tokens . #coin)) $ view #outputs s
    numberOfSelectedInputs
        = length $ view #inputs s
    numberOfSelectedCollateralInputs
        = length $ view #collateral s
    numberOfRequestedOutputs
        = length $ view #outputs s
    numberOfGeneratedChangeOutputs
        = length $ view #change s
    numberOfUniqueNonAdaAssetsInSelectedInputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens . snd)
        $ view #inputs s
    numberOfUniqueNonAdaAssetsInRequestedOutputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens)
        $ view #outputs s
    numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs
        = Set.size
        $ F.foldMap TokenBundle.getAssets
        $ view #change s

makeSelectionReportDetailed :: Selection -> SelectionReportDetailed
makeSelectionReportDetailed s = SelectionReportDetailed
    { selectedInputs
        = F.toList $ view #inputs s
    , selectedCollateral
        = F.toList $ view #collateral s
    , requestedOutputs
        = view #outputs s
    , generatedChangeOutputs
        = TokenBundle.Flat <$> view #change s
    }

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionOf TokenBundle' value.
instance Buildable (SelectionOf TokenBundle) where
    build = build . makeSelectionReport

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionOf TxOut' value.
instance Buildable (SelectionOf TxOut) where
    build = build
        . makeSelectionReport
        . over #change (fmap $ view #tokens)
