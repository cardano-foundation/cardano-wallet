{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides INTERNAL functions and types for coin selection.
--
-- It is recommended to import from 'Cardano.Wallet.CoinSelection' instead.
--
module Cardano.Wallet.CoinSelection.Internal
    (
    -- * Performing selections
      performSelection
    , Selection (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionParams (..)
    , SelectionSkeleton (..)

    -- * Output preparation
    , prepareOutputsWith
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)

    -- * Verification of post conditions
    , VerificationResult (..)

    -- * Verification of selections and selection errors
    , verifySelection
    , verifySelectionError

    -- * Selection deltas
    , SelectionDelta (..)
    , selectionDeltaAllAssets
    , selectionDeltaCoin
    , selectionHasValidSurplus
    , selectionMinimumCost
    , selectionSurplusCoin

    -- * Selection collateral
    , SelectionCollateralRequirement (..)
    , selectionCollateral
    , selectionCollateralRequired
    , selectionHasSufficientCollateral
    , selectionMinimumCollateral

    -- * Internal types and functions
    , ComputeMinimumCollateralParams (..)
    , computeMinimumCollateral
    , toBalanceConstraintsParams

    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.CoinSelection.Internal.Balance
    ( SelectionBalanceError (..)
    , SelectionDelta (..)
    , SelectionLimit
    , SelectionSkeleton
    , SelectionStrategy (..)
    )
import Cardano.Wallet.CoinSelection.Internal.Collateral
    ( SelectionCollateralError )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..), txOutMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Control.Monad
    ( (<=<) )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Random.Extra
    ( NonRandom (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( over, set, view, (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Ratio
    ( (%) )
import Data.Semigroup
    ( All (..), mtimesDefault )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.CoinSelection.Internal.Balance as Balance
import qualified Cardano.Wallet.CoinSelection.Internal.Collateral as Collateral
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Specifies all constraints required for coin selection.
--
-- Selection constraints:
--
--    - are dependent on the current set of protocol parameters.
--
--    - are not specific to a given selection.
--
--    - place limits on the coin selection algorithm, enabling it to produce
--      selections that are acceptable to the ledger.
--
data SelectionConstraints address = SelectionConstraints
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
        :: SelectionSkeleton address -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , computeSelectionLimit
        :: [(address, TokenBundle)] -> SelectionLimit
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
    }
    deriving Generic

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams address u = SelectionParams
    { assetsToBurn
        :: !TokenMap
        -- ^ Specifies a set of assets to burn.
    , assetsToMint
        :: !TokenMap
        -- ^ Specifies a set of assets to mint.
    , extraCoinIn
        :: !Coin
       -- ^ Specifies extra 'Coin' in.
    , extraCoinOut
        :: !Coin
        -- ^ Specifies extra 'Coin' out.
    , outputsToCover
        :: ![(address, TokenBundle)]
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
        :: !(Map u Coin)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- collateral inputs.
        --
        -- This set is allowed to intersect with 'utxoAvailableForInputs',
        -- since the ledger does not require that these sets are disjoint.
    , utxoAvailableForInputs
        :: !(UTxOSelection u)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- ordinary inputs and optionally, a subset that has already been
        -- selected.
        --
        -- Further entries from this set will be selected to cover any deficit.
    , selectionStrategy
        :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that an error occurred while performing a coin selection.
--
data SelectionError address u
    = SelectionBalanceErrorOf
      (SelectionBalanceError address u)
    | SelectionCollateralErrorOf
      (SelectionCollateralError u)
    | SelectionOutputErrorOf
      (SelectionOutputError address)
    deriving (Eq, Show)

-- | Represents a balanced selection.
--
data Selection address u = Selection
    { inputs
        :: !(NonEmpty (u, TokenBundle))
        -- ^ Selected inputs.
    , collateral
        :: ![(u, Coin)]
        -- ^ Selected collateral inputs.
    , outputs
        :: ![(address, TokenBundle)]
        -- ^ User-specified outputs
    , change
        :: ![TokenBundle]
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

-- | Provides a context for functions related to 'performSelection'.

type PerformSelection m address a u =
    SelectionConstraints address ->
    SelectionParams address u ->
    ExceptT (SelectionError address u) m a

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

-- | Performs a coin selection.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- This function guarantees that given a set of 'SelectionConstraints' @cs@
-- and 'SelectionParams' @ps@:
--
--  - if creation of a selection succeeds, a value @s@ of type 'Selection'
--    will be returned for which the following property holds:
--
--      >>> verifySelection cs ps s == VerificationSuccess
--
--  - if creation of a selection fails, a value @e@ of type 'SelectionError'
--    will be returned for which the following property holds:
--
--      >>> verifySelectionError cs ps e == VerificationSuccess
--
performSelection
    :: (HasCallStack, MonadRandom m, Ord u, Show address, Show u)
    => PerformSelection m address (Selection address u) u
performSelection cs = performSelectionInner cs <=< prepareOutputs cs

performSelectionInner
    :: (HasCallStack, MonadRandom m, Ord u, Show address, Show u)
    => PerformSelection m address (Selection address u) u
performSelectionInner cs ps = do
    balanceResult <- performSelectionBalance cs ps
    collateralResult <- performSelectionCollateral balanceResult cs ps
    pure $ mkSelection ps balanceResult collateralResult

prepareOutputs
    :: Applicative m
    => PerformSelection m address (SelectionParams address u) u
prepareOutputs cs ps =
    withExceptT SelectionOutputErrorOf $ ExceptT $ pure $
    flip (set #outputsToCover) ps <$>
    prepareOutputsInternal cs (view #outputsToCover ps)

performSelectionBalance
    :: (HasCallStack, MonadRandom m, Ord u, Show address, Show u)
    => PerformSelection m address (Balance.SelectionResult address u) u
performSelectionBalance cs ps =
    withExceptT SelectionBalanceErrorOf $ ExceptT $
    uncurry Balance.performSelection $ toBalanceConstraintsParams (cs, ps)

performSelectionCollateral
    :: (Applicative m, Ord u)
    => Balance.SelectionResult address u
    -> PerformSelection m address (Collateral.SelectionResult u) u
performSelectionCollateral balanceResult cs ps
    | selectionCollateralRequired ps =
        withExceptT SelectionCollateralErrorOf $ ExceptT $ pure $
        uncurry Collateral.performSelection $
        toCollateralConstraintsParams balanceResult (cs, ps)
    | otherwise =
        ExceptT $ pure $ Right Collateral.selectionResultEmpty

-- | Returns a selection's ordinary outputs and change outputs in a single list.
--
-- Since change outputs do not have addresses at the point of generation,
-- this function assigns all change outputs with a dummy change address.
--
selectionAllOutputs :: Selection address u -> [(address, TokenBundle)]
selectionAllOutputs selection = (<>)
    (selection ^. #outputs)
    (selection ^. #change <&> (dummyChangeaddress, ))
  where
    dummyChangeaddress :: address
    dummyChangeaddress =
        -- TODO: ADP-1448
        --
        -- Replace this call to 'error' with a call to a function that
        -- generates a dummy change address.
        --
        error "change address"

-- | Creates constraints and parameters for 'Balance.performSelection'.
--
toBalanceConstraintsParams
    :: (        SelectionConstraints address,         SelectionParams address u)
    -> (Balance.SelectionConstraints address, Balance.SelectionParams address u)
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
            :: (SelectionSkeleton address -> Coin)
            -> (SelectionSkeleton address -> Coin)
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
            adjustSelectionSkeleton
                :: SelectionSkeleton address
                -> SelectionSkeleton address
            adjustSelectionSkeleton = over #skeletonInputCount
                (+ view #maximumCollateralInputCount constraints)

        adjustComputeSelectionLimit
            :: ([(address, TokenBundle)] -> SelectionLimit)
            -> ([(address, TokenBundle)] -> SelectionLimit)
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
            view #rewardWithdrawal params <> view #extraCoinIn params <>
            mtimesDefault
                (view #certificateDepositsReturned params)
                (view #certificateDepositAmount constraints)
        , extraCoinSink = view #extraCoinOut params <>
            mtimesDefault
                (view #certificateDepositsTaken params)
                (view #certificateDepositAmount constraints)
        , outputsToCover =
            view #outputsToCover params
        , utxoAvailable =
            view #utxoAvailableForInputs params
        , selectionStrategy =
            view #selectionStrategy params
        }

-- | Creates constraints and parameters for 'Collateral.performSelection'.
--
toCollateralConstraintsParams
    :: Balance.SelectionResult a u
    -> (           SelectionConstraints a,            SelectionParams a u)
    -> (Collateral.SelectionConstraints  , Collateral.SelectionParams   u)
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
            view #utxoAvailableForCollateral params
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
    :: SelectionParams address u
    -> Balance.SelectionResult address u
    -> Collateral.SelectionResult u
    -> Selection address u
mkSelection _params balanceResult collateralResult = Selection
    { inputs = view #inputsSelected balanceResult
    , collateral = Map.toList $ view #coinsSelected collateralResult
    , outputs = view #outputsCovered balanceResult
    , change = view #changeGenerated balanceResult
    , assetsToMint = view #assetsToMint balanceResult
    , assetsToBurn = view #assetsToBurn balanceResult
    , extraCoinSource = view #extraCoinSource balanceResult
    , extraCoinSink = view #extraCoinSink balanceResult
    }

-- | Converts a 'Selection' to a balance result.
--
toBalanceResult :: Selection address u -> Balance.SelectionResult address u
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
-- Verification of post conditions
--------------------------------------------------------------------------------

-- | The result of verifying a post condition.
--
data VerificationResult
    = VerificationSuccess
    | VerificationFailure (NonEmpty VerificationFailureReason)
    deriving Show

-- | Represents a reason for verification failure.
--
data VerificationFailureReason =
    forall failureReason. Show failureReason =>
    VerificationFailureReason failureReason
deriving instance Show VerificationFailureReason

instance Eq VerificationResult where
    r1 == r2 = show r1 == show r2

instance Monoid VerificationResult where
    mempty = VerificationSuccess

instance Semigroup VerificationResult where
    r1 <> r2 = verificationResultFromFailureReasons $ (<>)
        (verificationResultToFailureReasons r1)
        (verificationResultToFailureReasons r2)

-- | Constructs a singleton verification failure.
--
verificationFailure
    :: forall failureReason. Show failureReason
    => failureReason
    -> VerificationResult
verificationFailure a = VerificationFailure (VerificationFailureReason a :| [])

-- | Constructs a 'VerificationResult' from a list of failure reasons.
--
verificationResultFromFailureReasons
    :: [VerificationFailureReason] -> VerificationResult
verificationResultFromFailureReasons =
    maybe VerificationSuccess VerificationFailure . NE.nonEmpty

-- | Deconstructs a 'VerificationResult' into a list of failure reasons.
--
verificationResultToFailureReasons
    :: VerificationResult -> [VerificationFailureReason]
verificationResultToFailureReasons = \case
    VerificationSuccess -> []
    VerificationFailure reasons -> NE.toList reasons

-- | Verifies the given condition.
--
-- If the given condition is 'True', returns 'VerificationSuccess'.
--
-- Otherwise, returns 'VerificationFailure' with the given reason.
--
verify
    :: forall failureReason. Show failureReason
    => Bool
    -> failureReason
    -> VerificationResult
verify condition failureReason =
    if condition
    then VerificationSuccess
    else verificationFailure failureReason

-- | Verifies all of the given conditions.
--
-- If the given conditions are all 'True', returns 'VerificationSuccess'.
--
-- Otherwise, returns 'VerificationFailure' with the given reason.
--
verifyAll
    :: forall f failureReason. (Foldable f, Show failureReason)
    => f Bool
    -> failureReason
    -> VerificationResult
verifyAll conditions = verify (getAll $ F.foldMap All conditions)

-- | Verifies that the given list is empty.
--
-- If the given list is empty, returns 'VerificationSuccess'.
--
-- Otherwise, returns 'VerificationFailure', with given reason constructor
-- applied to the non-empty list.
--
verifyEmpty
    :: forall failureReason a. Show failureReason
    => [a]
    -> (NonEmpty a -> failureReason)
    -> VerificationResult
verifyEmpty xs failureReason =
    maybe
        (VerificationSuccess)
        (verificationFailure . failureReason)
        (NE.nonEmpty xs)

--------------------------------------------------------------------------------
-- Selection verification
--------------------------------------------------------------------------------

-- | The type of all 'Selection' verification functions.
--
type VerifySelection address u =
    SelectionConstraints address ->
    SelectionParams address u ->
    Selection address u ->
    VerificationResult

-- | Verifies a 'Selection' for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a 'Selection' is incorrect in some way.
--
verifySelection :: (Ord u, Show address, Show u) => VerifySelection address u
verifySelection = mconcat
    [ verifySelectionCollateralSufficient
    , verifySelectionCollateralSuitable
    , verifySelectionDeltaValid
    , verifySelectionInputCountWithinLimit
    , verifySelectionOutputCoinsSufficient
    , verifySelectionOutputSizesWithinLimit
    , verifySelectionOutputTokenQuantitiesWithinLimit
    ]

--------------------------------------------------------------------------------
-- Selection verification: collateral sufficiency
--------------------------------------------------------------------------------

data FailureToVerifySelectionCollateralSufficient =
    FailureToVerifySelectionCollateralSufficient
    { collateralSelected :: Coin
    , collateralRequired :: Coin
    }
    deriving (Eq, Show)

verifySelectionCollateralSufficient :: VerifySelection address u
verifySelectionCollateralSufficient cs ps selection =
    verify
        (collateralSelected >= collateralRequired)
        (FailureToVerifySelectionCollateralSufficient {..})
  where
    collateralSelected = selectionCollateral selection
    collateralRequired = selectionMinimumCollateral cs ps selection

--------------------------------------------------------------------------------
-- Selection verification: collateral suitability
--------------------------------------------------------------------------------

data FailureToVerifySelectionCollateralSuitable u =
    FailureToVerifySelectionCollateralSuitable
    { collateralSelected
        :: [(u, Coin)]
    , collateralSelectedButUnsuitable
        :: [(u, Coin)]
    }
    deriving (Eq, Show)

verifySelectionCollateralSuitable
    :: forall address u. (Ord u, Show u) => VerifySelection address u
verifySelectionCollateralSuitable _cs ps selection =
    verify
        (null collateralSelectedButUnsuitable)
        (FailureToVerifySelectionCollateralSuitable {..})
  where
    collateralSelected =
        selection ^. #collateral
    collateralSelectedButUnsuitable =
        filter (not . utxoSuitableForCollateral) collateralSelected

    -- Since the caller of 'performSelection' is responsible for verifying that
    -- all entries within 'utxoAvailableForCollateral' are suitable for use as
    -- collateral, here we merely verify that the selected entry is indeed a
    -- member of this set.
    utxoSuitableForCollateral :: (u, Coin) -> Bool
    utxoSuitableForCollateral (i, c) =
        Map.singleton i c
        `Map.isSubmapOf`
        view #utxoAvailableForCollateral ps

--------------------------------------------------------------------------------
-- Selection verification: delta validity
--------------------------------------------------------------------------------

data FailureToVerifySelectionDeltaValid = FailureToVerifySelectionDeltaValid
    { delta
        :: SelectionDelta TokenBundle
    , minimumCost
        :: Coin
    , maximumCost
        :: Coin
    }
    deriving (Eq, Show)

verifySelectionDeltaValid :: VerifySelection address u
verifySelectionDeltaValid cs ps selection =
    verify
        (selectionHasValidSurplus cs ps selection)
        (FailureToVerifySelectionDeltaValid {..})
  where
    delta = selectionDeltaAllAssets selection
    minimumCost = selectionMinimumCost cs ps selection
    maximumCost = selectionMaximumCost cs ps selection

--------------------------------------------------------------------------------
-- Selection verification: selection limit
--------------------------------------------------------------------------------

data FailureToVerifySelectionInputCountWithinLimit =
    FailureToVerifySelectionInputCountWithinLimit
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

verifySelectionInputCountWithinLimit :: VerifySelection address u
verifySelectionInputCountWithinLimit cs _ps selection =
    verify
        (Balance.MaximumInputLimit totalInputCount <= selectionLimit)
        (FailureToVerifySelectionInputCountWithinLimit {..})
  where
    collateralInputCount = length (selection ^. #collateral)
    ordinaryInputCount = length (selection ^. #inputs)
    totalInputCount = collateralInputCount + ordinaryInputCount
    selectionLimit = (cs ^. #computeSelectionLimit) (selection ^. #outputs)

--------------------------------------------------------------------------------
-- Selection verification: minimum ada quantities
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputCoinsSufficient address =
    FailureToVerifySelectionOutputCoinsSufficient
    (NonEmpty (SelectionOutputCoinInsufficientError address))
    deriving (Eq, Show)

data SelectionOutputCoinInsufficientError address =
    SelectionOutputCoinInsufficientError
        { minimumExpectedCoin :: Coin
        , output :: (address, TokenBundle)
        }
    deriving (Eq, Show)

verifySelectionOutputCoinsSufficient
    :: forall address u. Show address => VerifySelection address u
verifySelectionOutputCoinsSufficient cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputCoinsSufficient
  where
    errors :: [SelectionOutputCoinInsufficientError address]
    errors = mapMaybe maybeError (selectionAllOutputs selection)

    maybeError
        :: (address, TokenBundle)
        -> Maybe (SelectionOutputCoinInsufficientError address)
    maybeError output
        | snd output ^. #coin < minimumExpectedCoin =
            Just SelectionOutputCoinInsufficientError
                {minimumExpectedCoin, output}
        | otherwise =
            Nothing
      where
        minimumExpectedCoin :: Coin
        minimumExpectedCoin =
            (cs ^. #computeMinimumAdaQuantity)
            (snd output ^. #tokens)

--------------------------------------------------------------------------------
-- Selection verification: output sizes
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputSizesWithinLimit address =
    FailureToVerifySelectionOutputSizesWithinLimit
    (NonEmpty (SelectionOutputSizeExceedsLimitError address))
    deriving (Eq, Show)

verifySelectionOutputSizesWithinLimit
    :: forall address u. Show address => VerifySelection address u
verifySelectionOutputSizesWithinLimit cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputSizesWithinLimit
  where
    errors :: [SelectionOutputSizeExceedsLimitError address]
    errors = mapMaybe (verifyOutputSize cs) (selectionAllOutputs selection)

--------------------------------------------------------------------------------
-- Selection verification: output token quantities
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputTokenQuantitiesWithinLimit address =
    FailureToVerifySelectionOutputTokenQuantitiesWithinLimit
    (NonEmpty (SelectionOutputTokenQuantityExceedsLimitError address))
    deriving (Eq, Show)

verifySelectionOutputTokenQuantitiesWithinLimit
    :: forall address u. Show address
    => VerifySelection address u
verifySelectionOutputTokenQuantitiesWithinLimit _cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputTokenQuantitiesWithinLimit
  where
    errors :: [SelectionOutputTokenQuantityExceedsLimitError address]
    errors = verifyOutputTokenQuantities =<< selectionAllOutputs selection

--------------------------------------------------------------------------------
-- Selection error verification
--------------------------------------------------------------------------------

-- | The type of all 'SelectionError' verification functions.
--
type VerifySelectionError e address u =
    SelectionConstraints address ->
    SelectionParams address u ->
    e ->
    VerificationResult

-- | Verifies a 'SelectionError' for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a 'SelectionError' is incorrect in some way.
--
verifySelectionError
    :: (Show address, Ord u, Show u)
    => VerifySelectionError (SelectionError address u) address u
verifySelectionError cs ps = \case
    SelectionBalanceErrorOf e ->
        verifySelectionBalanceError cs ps e
    SelectionCollateralErrorOf e ->
        verifySelectionCollateralError cs ps e
    SelectionOutputErrorOf e ->
        verifySelectionOutputError cs ps e

--------------------------------------------------------------------------------
-- Selection error verification: balance errors
--------------------------------------------------------------------------------

verifySelectionBalanceError
    :: (Show address, Ord u, Show u)
    => VerifySelectionError (SelectionBalanceError address u) address u
verifySelectionBalanceError cs ps = \case
    Balance.BalanceInsufficient e ->
        verifyBalanceInsufficientError cs ps e
    Balance.EmptyUTxO ->
        verifyEmptyUTxOError cs ps ()
    Balance.InsufficientMinCoinValues es ->
        F.foldMap (verifyInsufficientMinCoinValueError cs ps) es
    Balance.UnableToConstructChange e->
        verifyUnableToConstructChangeError cs ps e
    Balance.SelectionLimitReached e ->
        verifySelectionLimitReachedError cs ps e

--------------------------------------------------------------------------------
-- Selection error verification: balance insufficient errors
--------------------------------------------------------------------------------

data FailureToVerifyBalanceInsufficientError =
    FailureToVerifyBalanceInsufficientError
    { utxoBalanceAvailable :: TokenBundle
    , utxoBalanceRequired :: TokenBundle
    }
    deriving (Eq, Show)

verifyBalanceInsufficientError
    :: VerifySelectionError Balance.BalanceInsufficientError address u
verifyBalanceInsufficientError cs ps e =
    verifyAll
        [ not (utxoBalanceRequired `leq` utxoBalanceAvailable)
        , not (Balance.isUTxOBalanceSufficient balanceParams)
        ]
        FailureToVerifyBalanceInsufficientError {..}
  where
    balanceParams = snd $ toBalanceConstraintsParams (cs, ps)
    utxoBalanceAvailable = e ^. #utxoBalanceAvailable
    utxoBalanceRequired = e ^. #utxoBalanceRequired

--------------------------------------------------------------------------------
-- Selection error verification: empty UTxO errors
--------------------------------------------------------------------------------

newtype FailureToVerifyEmptyUTxOError u = FailureToVerifyEmptyUTxOError
    { utxoAvailableForInputs :: UTxOSelection u }
    deriving (Eq, Show)

verifyEmptyUTxOError :: (Eq u, Show u) => VerifySelectionError () address u
verifyEmptyUTxOError _cs SelectionParams {utxoAvailableForInputs} _e =
    verify
        (utxoAvailableForInputs == UTxOSelection.empty)
        (FailureToVerifyEmptyUTxOError {utxoAvailableForInputs})

--------------------------------------------------------------------------------
-- Selection error verification: insufficient minimum ada quantity errors
--------------------------------------------------------------------------------

data FailureToVerifyInsufficientMinCoinValueError address =
    FailureToVerifyInsufficientMinCoinValueError
    { reportedOutput :: (address, TokenBundle)
    , reportedMinCoinValue :: Coin
    , verifiedMinCoinValue :: Coin
    }
    deriving (Eq, Show)

verifyInsufficientMinCoinValueError
    :: Show address
    => VerifySelectionError
        (Balance.InsufficientMinCoinValueError address) address u
verifyInsufficientMinCoinValueError cs _ps e =
    verifyAll
        [ reportedMinCoinValue == verifiedMinCoinValue
        , reportedMinCoinValue > snd reportedOutput ^. #coin
        ]
        FailureToVerifyInsufficientMinCoinValueError {..}
  where
    reportedOutput = e ^. #outputWithInsufficientAda
    reportedMinCoinValue = e ^. #expectedMinCoinValue
    verifiedMinCoinValue =
        (cs ^. #computeMinimumAdaQuantity)
        (snd reportedOutput ^. #tokens)

--------------------------------------------------------------------------------
-- Selection error verification: selection limit errors
--------------------------------------------------------------------------------

data FailureToVerifySelectionLimitReachedError u =
    FailureToVerifySelectionLimitReachedError
        { selectedInputs
            :: [(u, TokenBundle)]
            -- ^ The inputs that were actually selected.
        , selectedInputCount
            :: Int
            -- ^ The number of inputs that were actually selected.
        , selectionLimitOriginal
            :: SelectionLimit
            -- ^ The selection limit before accounting for collateral inputs.
        , selectionLimitAdjusted
            :: SelectionLimit
            -- ^ The selection limit after accounting for collateral inputs.
        }
    deriving (Eq, Show)

-- | Verifies a 'Balance.SelectionLimitReachedError'.
--
-- This function verifies that the number of the selected inputs is correct
-- given the amount of space we expect to be reserved for collateral inputs.
--
verifySelectionLimitReachedError
    :: forall address u. Show u
    => VerifySelectionError
        (Balance.SelectionLimitReachedError address u) address u
verifySelectionLimitReachedError cs ps e =
    verify
        (Balance.MaximumInputLimit selectedInputCount >= selectionLimitAdjusted)
        (FailureToVerifySelectionLimitReachedError {..})
  where
    selectedInputs :: [(u, TokenBundle)]
    selectedInputs = e ^. #inputsSelected

    selectedInputCount :: Int
    selectedInputCount = F.length selectedInputs

    selectionLimitAdjusted :: SelectionLimit
    selectionLimitAdjusted = toBalanceConstraintsParams (cs, ps)
        & fst
        & view #computeSelectionLimit
        & ($ F.toList $ e ^. #outputsToCover)

    selectionLimitOriginal :: SelectionLimit
    selectionLimitOriginal = cs
        & view #computeSelectionLimit
        & ($ F.toList $ e ^. #outputsToCover)

--------------------------------------------------------------------------------
-- Selection error verification: change construction errors
--------------------------------------------------------------------------------

data FailureToVerifyUnableToConstructChangeError address u =
    FailureToVerifyUnableToConstructChangeError
        { errorOriginal
            :: Balance.UnableToConstructChangeError
            -- ^ The original error.
        , errorWithMinimalConstraints
            :: SelectionError address u
            -- ^ An error encountered when attempting to re-run the selection
            -- process with minimal constraints.
        }
    deriving (Eq, Show)

-- | Verifies a 'Balance.UnableToConstructChangeError'.
--
-- This function verifies that it's possible to successfully re-run the
-- selection process with exactly the same parameters /if/ we modify the
-- constraints to be minimal, where we have:
--
--   - a minimum cost function that always returns zero.
--   - a minimum ada quantity function that always returns zero.
--
-- Such an attempt should always succeed, since 'UnableToConstructChangeError'
-- should be returned if (and only if) the available UTxO balance:
--
--  - is sufficient to cover the desired total output balance; but
--
--  - is NOT sufficient to cover:
--
--      a. the minimum cost of the transaction;
--      b. the minimum ada quantities of generated change outputs.
--
-- If the available UTxO balance is not sufficient to cover the desired total
-- output balance, then 'performSelection' should explicitly indicate that the
-- balance is insufficient by returning a 'BalanceInsufficientError' instead.
--
verifyUnableToConstructChangeError
    :: forall address u. (Ord u, Show address, Show u)
    => VerifySelectionError
        Balance.UnableToConstructChangeError address u
verifyUnableToConstructChangeError cs ps errorOriginal =
    case resultWithMinimalConstraints of
        Left errorWithMinimalConstraints ->
            verificationFailure
            FailureToVerifyUnableToConstructChangeError {..}
        Right _ ->
            VerificationSuccess
  where
    -- The result of attempting to re-run the selection process with minimal
    -- constraints, where we have:
    --
    --   - a minimum cost function that always returns zero.
    --   - a minimum ada quantity function that always returns zero.
    --
    resultWithMinimalConstraints
        :: Either (SelectionError address u) (Selection address u)
    resultWithMinimalConstraints =
        -- The 'performSelection' function requires a 'MonadRandom' context so
        -- that it can select entries at random from the available UTxO set.
        --
        -- However, for this verification step, we don't actually require UTxO
        -- selection to be random. We only require that the 'performSelection'
        -- function is able to select some sequence of UTxOs that collectively
        -- covers the desired output amount.
        --
        -- To satisfy the requirement to provide a 'MonadRandom' context, we use
        -- the 'NonRandom' type, for which a 'MonadRandom' instance is provided.
        -- This instance, when asked to provide a random value from within a
        -- range of values, will always provide the same value.
        --
        -- This means that each internal step of 'performSelection' will select
        -- a value from the same relative position of the leftover available
        -- UTxO set. However, since selecting a UTxO entry removes it from the
        -- leftover set, every subsequent step will select a different entry,
        -- and thus the selection algorithm will always be able to make forward
        -- progress.
        --
        runNonRandom $ runExceptT $ performSelection cs' ps
      where
        -- A modified set of constraints that should always allow the
        -- successful creation of a selection:
        cs' = cs
            { computeMinimumAdaQuantity = const $ Coin 0
            , computeMinimumCost = const $ Coin 0
            , computeSelectionLimit = const Balance.NoLimit
            }

--------------------------------------------------------------------------------
-- Selection error verification: collateral errors
--------------------------------------------------------------------------------

data FailureToVerifySelectionCollateralError u =
    FailureToVerifySelectionCollateralError
        { largestCombination
            :: Map u Coin
            -- ^ The largest available UTxO combination reported.
        , largestCombinationValue
            :: Coin
            -- ^ The total balance of the largest available UTxO combination.
        , largestCombinationSize
            :: Int
            -- ^ The size of the largest available UTxO combination.
        , largestCombinationUnsuitableSubset
            :: Map u Coin
            -- ^ The subset of UTxOs in the largest available combination that
            -- are not suitable for use as collateral.
            --
            -- UTxOs that are not suitable for collateral should never be made
            -- available to the collateral selection algorithm, and should
            -- therefore never be included in any error reported by the
            -- collateral selection algorithm.
        , maximumSelectionSize
            :: Int
            -- ^ The maximum number of entries permitted in the largest
            -- combination, determined by the maximum allowable number of
            -- collateral inputs.
        , minimumSelectionAmount
            :: Coin
            -- ^ The reported minimum selection amount.
        }
        deriving (Eq, Show)

verifySelectionCollateralError
    :: forall address u. (Ord u, Show u)
    => VerifySelectionError (SelectionCollateralError u) address u
verifySelectionCollateralError cs ps e =
    verifyAll
        [ Map.null largestCombinationUnsuitableSubset
        , largestCombinationSize <= maximumSelectionSize
        , largestCombinationValue < minimumSelectionAmount
        ]
        (FailureToVerifySelectionCollateralError {..})
  where
    largestCombination :: Map u Coin
    largestCombination = e ^. #largestCombinationAvailable
    largestCombinationSize :: Int
    largestCombinationSize = Map.size largestCombination
    largestCombinationValue :: Coin
    largestCombinationValue = F.fold largestCombination

    largestCombinationUnsuitableSubset :: Map u Coin
    largestCombinationUnsuitableSubset = Map.withoutKeys
        (largestCombination)
        (Map.keysSet $ ps ^. #utxoAvailableForCollateral)

    maximumSelectionSize :: Int
    maximumSelectionSize = cs ^. #maximumCollateralInputCount
    minimumSelectionAmount :: Coin
    minimumSelectionAmount = e ^. #minimumSelectionAmount

--------------------------------------------------------------------------------
-- Selection error verification: output errors
--------------------------------------------------------------------------------

verifySelectionOutputError
    :: Show address
    => VerifySelectionError (SelectionOutputError address) address u
verifySelectionOutputError cs ps = \case
    SelectionOutputSizeExceedsLimit e ->
        verifySelectionOutputSizeExceedsLimitError cs ps e
    SelectionOutputTokenQuantityExceedsLimit e ->
        verifySelectionOutputTokenQuantityExceedsLimitError cs ps e

--------------------------------------------------------------------------------
-- Selection error verification: output size errors
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputSizeExceedsLimitError address =
    FailureToVerifySelectionOutputSizeExceedsLimitError
        { outputReportedAsExceedingLimit :: (address, TokenBundle) }
    deriving (Eq, Show)

verifySelectionOutputSizeExceedsLimitError
    :: Show address
    => VerifySelectionError
        (SelectionOutputSizeExceedsLimitError address) address u
verifySelectionOutputSizeExceedsLimitError cs _ps e =
    verify
        (not isWithinLimit)
        (FailureToVerifySelectionOutputSizeExceedsLimitError {..})
  where
    isWithinLimit = case (cs ^. #assessTokenBundleSize) bundle of
        TokenBundleSizeWithinLimit -> True
        TokenBundleSizeExceedsLimit -> False
      where
        bundle = snd outputReportedAsExceedingLimit

    outputReportedAsExceedingLimit = e ^. #outputThatExceedsLimit

--------------------------------------------------------------------------------
-- Selection error verification: output token quantity errors
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputTokenQuantityExceedsLimitError address =
    FailureToVerifySelectionOutputTokenQuantityExceedsLimitError
        { reportedError
            :: SelectionOutputTokenQuantityExceedsLimitError address
        }
    deriving (Eq, Show)

verifySelectionOutputTokenQuantityExceedsLimitError
    :: Show address
    => VerifySelectionError
        (SelectionOutputTokenQuantityExceedsLimitError address) address u
verifySelectionOutputTokenQuantityExceedsLimitError _cs _ps e =
    verify
        (e ^. #quantity > e ^. #quantityMaxBound)
        (FailureToVerifySelectionOutputTokenQuantityExceedsLimitError e)

--------------------------------------------------------------------------------
-- Selection deltas
--------------------------------------------------------------------------------

-- | Calculates the selection delta for all assets.
--
-- See 'SelectionDelta'.
--
selectionDeltaAllAssets :: Selection address u -> SelectionDelta TokenBundle
selectionDeltaAllAssets = Balance.selectionDeltaAllAssets . toBalanceResult

-- | Calculates the ada selection delta.
--
-- See 'SelectionDelta'.
--
selectionDeltaCoin :: Selection address u -> SelectionDelta Coin
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
    :: SelectionConstraints address
    -> SelectionParams address u
    -> Selection address u
    -> Bool
selectionHasValidSurplus constraints params selection =
    Balance.selectionHasValidSurplus
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Computes the minimum required cost of a selection.
--
selectionMinimumCost
    :: SelectionConstraints address
    -> SelectionParams address u
    -> Selection address u
    -> Coin
selectionMinimumCost constraints params selection =
    Balance.selectionMinimumCost
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Computes the maximum acceptable cost of a selection.
--
selectionMaximumCost
    :: SelectionConstraints address
    -> SelectionParams address u
    -> Selection address u
    -> Coin
selectionMaximumCost constraints params selection =
    Balance.selectionMaximumCost
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
selectionSurplusCoin :: Selection address u -> Coin
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
selectionCollateralRequired :: SelectionParams address u -> Bool
selectionCollateralRequired params = case view #collateralRequirement params of
    SelectionCollateralRequired    -> True
    SelectionCollateralNotRequired -> False

-- | Applies the given transformation function only when collateral is required.
--
whenCollateralRequired
    :: SelectionParams address u
    -> (a -> a)
    -> (a -> a)
whenCollateralRequired params f
    | selectionCollateralRequired params = f
    | otherwise = id

-- | Computes the total amount of collateral within a selection.
--
selectionCollateral :: Selection address u -> Coin
selectionCollateral = F.foldMap snd . view #collateral

-- | Indicates whether or not a selection has sufficient collateral.
--
selectionHasSufficientCollateral
    :: SelectionConstraints address
    -> SelectionParams address u
    -> Selection address u
    -> Bool
selectionHasSufficientCollateral constraints params selection =
    actual >= required
  where
    actual = selectionCollateral selection
    required = selectionMinimumCollateral constraints params selection

-- | Computes the minimum required amount of collateral for a selection.
--
selectionMinimumCollateral
    :: SelectionConstraints address
    -> SelectionParams address u
    -> Selection address u
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

--------------------------------------------------------------------------------
-- Preparing outputs
--------------------------------------------------------------------------------

-- | Prepares the given user-specified outputs, ensuring that they are valid.
--
prepareOutputsInternal
    :: forall address. SelectionConstraints address
    -> [(address, TokenBundle)]
    -> Either (SelectionOutputError address) [(address, TokenBundle)]
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
    excessivelyLargeBundles :: [SelectionOutputSizeExceedsLimitError address]
    excessivelyLargeBundles =
        mapMaybe (verifyOutputSize constraints) outputsToCover

    -- The complete list of token quantities that exceed the maximum quantity
    -- allowed in a transaction output:
    excessiveTokenQuantities
        :: [SelectionOutputTokenQuantityExceedsLimitError address]
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
prepareOutputsWith
    :: Functor f
    => (TokenMap -> Coin)
    -> f (address, TokenBundle)
    -> f (address, TokenBundle)
prepareOutputsWith minCoinValueFor =
    fmap $ fmap augmentBundle
  where
    augmentBundle :: TokenBundle -> TokenBundle
    augmentBundle bundle
        | TokenBundle.getCoin bundle == Coin 0 =
            bundle & set #coin (minCoinValueFor (view #tokens bundle))
        | otherwise =
            bundle

-- | Indicates a problem when preparing outputs for a coin selection.
--
data SelectionOutputError address
    = SelectionOutputSizeExceedsLimit
        (SelectionOutputSizeExceedsLimitError address)
    | SelectionOutputTokenQuantityExceedsLimit
        (SelectionOutputTokenQuantityExceedsLimitError address)
    deriving (Eq, Generic, Show)

newtype SelectionOutputSizeExceedsLimitError address =
    SelectionOutputSizeExceedsLimitError
    { outputThatExceedsLimit :: (address, TokenBundle)
    }
    deriving (Eq, Generic, Show)

-- | Verifies the size of an output.
--
-- Returns 'SelectionOutputSizeExceedsLimitError' if and only if the size
-- exceeds the limit defined by the protocol.
--
verifyOutputSize
    :: SelectionConstraints address
    -> (address, TokenBundle)
    -> Maybe (SelectionOutputSizeExceedsLimitError address)
verifyOutputSize cs out
    | withinLimit =
        Nothing
    | otherwise =
        Just $ SelectionOutputSizeExceedsLimitError out
  where
    withinLimit :: Bool
    withinLimit =
        case (cs ^. #assessTokenBundleSize) (snd out) of
            TokenBundleSizeWithinLimit -> True
            TokenBundleSizeExceedsLimit -> False

-- | Indicates that a token quantity exceeds the maximum quantity that can
--   appear in a transaction output's token bundle.
--
data SelectionOutputTokenQuantityExceedsLimitError address =
    SelectionOutputTokenQuantityExceedsLimitError
    { address :: !address
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
    :: (address, TokenBundle)
    -> [SelectionOutputTokenQuantityExceedsLimitError address]
verifyOutputTokenQuantities out =
    [ SelectionOutputTokenQuantityExceedsLimitError
        {address, asset, quantity, quantityMaxBound = txOutMaxTokenQuantity}
    | let address = fst out
    , (asset, quantity) <- TokenMap.toFlatList $ (snd out) ^. #tokens
    , quantity > txOutMaxTokenQuantity
    ]
