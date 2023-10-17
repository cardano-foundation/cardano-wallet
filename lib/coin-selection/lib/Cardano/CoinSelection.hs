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
module Cardano.CoinSelection
    (
    -- * Performing selections
      performSelection
    , Selection (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionParams (..)
    , SelectionSkeleton (..)

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
    , SelectionCollateralError (..)
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
import Cardano.CoinSelection.Balance
    ( SelectionBalanceError (..)
    , SelectionDelta (..)
    , SelectionSkeleton
    , SelectionStrategy (..)
    )
import Cardano.CoinSelection.Context
    ( SelectionContext (..) )
import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessment (..), TokenBundleSizeAssessor (..) )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxTokenQuantity )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Random.NonRandom
    ( NonRandom (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
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

import qualified Cardano.CoinSelection.Balance as Balance
import qualified Cardano.CoinSelection.Collateral as Collateral
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
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
data SelectionConstraints ctx = SelectionConstraints
    { tokenBundleSizeAssessor
        :: TokenBundleSizeAssessor
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output.
    , computeMinimumAdaQuantity
        :: Address ctx -> TokenMap -> Coin
        -- ^ Computes the minimum ada quantity required for a given output.
    , isBelowMinimumAdaQuantity
        :: Address ctx -> TokenBundle -> Bool
      -- ^ Returns 'True' if the given 'TokenBundle' has a 'Coin' value that is
      -- below the minimum required.
    , computeMinimumCost
        :: SelectionSkeleton ctx -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , maximumCollateralInputCount
        :: Int
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
    , minimumCollateralPercentage
        :: Natural
        -- ^ Specifies the minimum required amount of collateral as a
        -- percentage of the total transaction fee.
    , maximumOutputAdaQuantity
        :: Coin
        -- ^ Specifies the largest ada quantity that can appear in the token
        -- bundle of an output.
    , maximumOutputTokenQuantity
        :: TokenQuantity
        -- ^ Specifies the largest non-ada quantity that can appear in the
        -- token bundle of an output.
    , maximumLengthChangeAddress
        :: Address ctx
    , nullAddress
        :: Address ctx
    }
    deriving Generic

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams ctx = SelectionParams
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
        :: ![(Address ctx, TokenBundle)]
        -- ^ Specifies a set of outputs that must be paid for.
    , collateralRequirement
        :: !SelectionCollateralRequirement
        -- ^ Specifies the collateral requirement for this selection.
    , utxoAvailableForCollateral
        :: !(Map (UTxO ctx) Coin)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- collateral inputs.
        --
        -- This set is allowed to intersect with 'utxoAvailableForInputs',
        -- since the ledger does not require that these sets are disjoint.
    , utxoAvailableForInputs
        :: !(UTxOSelection (UTxO ctx))
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- ordinary inputs and optionally, a subset that has already been
        -- selected.
        --
        -- Further entries from this set will be selected to cover any deficit.
    , selectionStrategy
        :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving Generic

deriving instance SelectionContext ctx => Eq (SelectionParams ctx)
deriving instance SelectionContext ctx => Show (SelectionParams ctx)

-- | Indicates that an error occurred while performing a coin selection.
--
data SelectionError ctx
    = SelectionBalanceErrorOf
      (SelectionBalanceError ctx)
    | SelectionCollateralErrorOf
      (SelectionCollateralError ctx)

deriving instance SelectionContext ctx => Eq (SelectionError ctx)
deriving instance SelectionContext ctx => Show (SelectionError ctx)

-- | Represents an unsuccessful attempt to select collateral.
--
data SelectionCollateralError ctx = SelectionCollateralError
    { largestCombinationAvailable :: Map (UTxO ctx) Coin
        -- ^ The largest combination of coins available.
    , minimumSelectionAmount :: Coin
        -- ^ A lower bound on the sum of coins to be selected as collateral.
    }
    deriving Generic

deriving instance SelectionContext ctx => Eq (SelectionCollateralError ctx)
deriving instance SelectionContext ctx => Show (SelectionCollateralError ctx)

-- | Represents a balanced selection.
--
data Selection ctx = Selection
    { inputs
        :: !(NonEmpty (UTxO ctx, TokenBundle))
        -- ^ Selected inputs.
    , collateral
        :: ![(UTxO ctx, Coin)]
        -- ^ Selected collateral inputs.
    , outputs
        :: ![(Address ctx, TokenBundle)]
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
    deriving Generic

deriving instance SelectionContext ctx => Eq (Selection ctx)
deriving instance SelectionContext ctx => Show (Selection ctx)

-- | Provides a context for functions related to 'performSelection'.

type PerformSelection m ctx a =
    SelectionConstraints ctx ->
    SelectionParams ctx ->
    ExceptT (SelectionError ctx) m a

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
    :: (HasCallStack, MonadRandom m, SelectionContext ctx)
    => PerformSelection m ctx (Selection ctx)
performSelection cs ps = do
    balanceResult <- performSelectionBalance cs ps
    collateralResult <- performSelectionCollateral balanceResult cs ps
    pure $ mkSelection ps balanceResult collateralResult

performSelectionBalance
    :: (HasCallStack, MonadRandom m, SelectionContext ctx)
    => PerformSelection m ctx (Balance.SelectionResult ctx)
performSelectionBalance cs ps =
    withExceptT SelectionBalanceErrorOf $ ExceptT $
    uncurry Balance.performSelection $ toBalanceConstraintsParams (cs, ps)

performSelectionCollateral
    :: (Applicative m, SelectionContext ctx)
    => Balance.SelectionResult ctx
    -> PerformSelection m ctx (Collateral.SelectionResult (UTxO ctx))
performSelectionCollateral balanceResult cs ps
    | selectionCollateralRequired ps =
        withExceptT mkCollateralError $ ExceptT $ pure $
        uncurry Collateral.performSelection $
        toCollateralConstraintsParams balanceResult (cs, ps)
    | otherwise =
        ExceptT $ pure $ Right Collateral.selectionResultEmpty
  where
    mkCollateralError
        :: Collateral.SelectionCollateralError (UTxO ctx)
        -> SelectionError ctx
    mkCollateralError Collateral.SelectionCollateralError {..} =
        SelectionCollateralErrorOf
        SelectionCollateralError {..}

-- | Returns a selection's change outputs with dummy addresses.
--
-- Since change outputs do not have addresses at the point of generation,
-- this function assigns all change outputs with a dummy change address
-- of the maximum possible length.
--
selectionChangeOutputsWithDummyAddresses
    :: SelectionConstraints ctx
    -> Selection ctx
    -> [(Address ctx, TokenBundle)]
selectionChangeOutputsWithDummyAddresses constraints selection =
    (selection ^. #change <&> (maximumLengthChangeAddress constraints, ))

-- | Creates constraints and parameters for 'Balance.performSelection'.
--
toBalanceConstraintsParams
    :: forall ctx.
       (        SelectionConstraints ctx,         SelectionParams ctx)
    -> (Balance.SelectionConstraints ctx, Balance.SelectionParams ctx)
toBalanceConstraintsParams (constraints, params) =
    (balanceConstraints, balanceParams)
  where
    balanceConstraints = Balance.SelectionConstraints
        { computeMinimumAdaQuantity =
            view #computeMinimumAdaQuantity constraints
        , computeMinimumCost =
            view #computeMinimumCost constraints
                & adjustComputeMinimumCost
        , tokenBundleSizeAssessor =
            view #tokenBundleSizeAssessor constraints
        , maximumOutputAdaQuantity =
            view #maximumOutputAdaQuantity constraints
        , maximumOutputTokenQuantity =
            view #maximumOutputTokenQuantity constraints
        , maximumLengthChangeAddress =
            view #maximumLengthChangeAddress constraints
        , nullAddress =
            view #nullAddress constraints
        }
      where
        adjustComputeMinimumCost
            :: (SelectionSkeleton ctx -> Coin)
            -> (SelectionSkeleton ctx -> Coin)
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
                :: SelectionSkeleton ctx
                -> SelectionSkeleton ctx
            adjustSelectionSkeleton = over #skeletonInputCount
                (+ view #maximumCollateralInputCount constraints)

    balanceParams = Balance.SelectionParams
        { assetsToBurn =
            view #assetsToBurn params
        , assetsToMint =
            view #assetsToMint params
        , extraCoinSource =
            view #extraCoinIn params
        , extraCoinSink =
            view #extraCoinOut params
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
    :: Balance.SelectionResult ctx
    ->  ( SelectionConstraints ctx
        , SelectionParams ctx
        )
    ->  ( Collateral.SelectionConstraints
        , Collateral.SelectionParams (UTxO ctx)
        )
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
    :: SelectionParams ctx
    -> Balance.SelectionResult ctx
    -> Collateral.SelectionResult (UTxO ctx)
    -> Selection ctx
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
toBalanceResult :: Selection ctx -> Balance.SelectionResult ctx
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
type VerifySelection ctx =
    SelectionConstraints ctx ->
    SelectionParams ctx ->
    Selection ctx ->
    VerificationResult

-- | Verifies a 'Selection' for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a 'Selection' is incorrect in some way.
--
verifySelection :: SelectionContext ctx => VerifySelection ctx
verifySelection = mconcat
    [ verifySelectionCollateralSufficient
    , verifySelectionCollateralSuitable
    , verifySelectionDeltaValid
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

verifySelectionCollateralSufficient :: VerifySelection ctx
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
    :: forall ctx. SelectionContext ctx => VerifySelection ctx
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
    utxoSuitableForCollateral :: (UTxO ctx, Coin) -> Bool
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

verifySelectionDeltaValid :: VerifySelection ctx
verifySelectionDeltaValid cs ps selection =
    verify
        (selectionHasValidSurplus cs ps selection)
        (FailureToVerifySelectionDeltaValid {..})
  where
    delta = selectionDeltaAllAssets selection
    minimumCost = selectionMinimumCost cs ps selection
    maximumCost = selectionMaximumCost cs ps selection

--------------------------------------------------------------------------------
-- Selection verification: minimum ada quantities
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputCoinsSufficient ctx =
    FailureToVerifySelectionOutputCoinsSufficient
    (NonEmpty (SelectionOutputCoinInsufficientError ctx))
    deriving (Eq, Show)

data SelectionOutputCoinInsufficientError ctx =
    SelectionOutputCoinInsufficientError
        { minimumExpectedCoin :: Coin
        , output :: (Address ctx, TokenBundle)
        }
    deriving Generic

deriving instance SelectionContext ctx =>
    Eq (SelectionOutputCoinInsufficientError ctx)
deriving instance SelectionContext ctx =>
    Show (SelectionOutputCoinInsufficientError ctx)

verifySelectionOutputCoinsSufficient
    :: forall ctx. SelectionContext ctx => VerifySelection ctx
verifySelectionOutputCoinsSufficient cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputCoinsSufficient
  where
    errors :: [SelectionOutputCoinInsufficientError ctx]
    errors = mapMaybe maybeError
        (selectionChangeOutputsWithDummyAddresses cs selection)

    maybeError
        :: (Address ctx, TokenBundle)
        -> Maybe (SelectionOutputCoinInsufficientError ctx)
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
            (fst output)
            (snd output ^. #tokens)

--------------------------------------------------------------------------------
-- Selection verification: output sizes
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputSizesWithinLimit address =
    FailureToVerifySelectionOutputSizesWithinLimit
    (NonEmpty (SelectionOutputSizeExceedsLimitError address))
    deriving (Eq, Show)

verifySelectionOutputSizesWithinLimit
    :: forall ctx. (SelectionContext ctx) => VerifySelection ctx
verifySelectionOutputSizesWithinLimit cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputSizesWithinLimit
  where
    errors :: [SelectionOutputSizeExceedsLimitError ctx]
    errors = mapMaybe (verifyOutputSize cs)
        (selectionChangeOutputsWithDummyAddresses cs selection)

--------------------------------------------------------------------------------
-- Selection verification: output token quantities
--------------------------------------------------------------------------------

newtype FailureToVerifySelectionOutputTokenQuantitiesWithinLimit address =
    FailureToVerifySelectionOutputTokenQuantitiesWithinLimit
    (NonEmpty (SelectionOutputTokenQuantityExceedsLimitError address))
    deriving (Eq, Show)

verifySelectionOutputTokenQuantitiesWithinLimit
    :: forall ctx. SelectionContext ctx => VerifySelection ctx
verifySelectionOutputTokenQuantitiesWithinLimit cs _ps selection =
    verifyEmpty errors FailureToVerifySelectionOutputTokenQuantitiesWithinLimit
  where
    errors :: [SelectionOutputTokenQuantityExceedsLimitError ctx]
    errors = verifyOutputTokenQuantities cs =<<
        selectionChangeOutputsWithDummyAddresses cs selection

--------------------------------------------------------------------------------
-- Selection error verification
--------------------------------------------------------------------------------

-- | The type of all 'SelectionError' verification functions.
--
type VerifySelectionError e ctx =
    SelectionConstraints ctx -> SelectionParams ctx -> e -> VerificationResult

-- | Verifies a 'SelectionError' for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a 'SelectionError' is incorrect in some way.
--
verifySelectionError
    :: SelectionContext ctx => VerifySelectionError (SelectionError ctx) ctx
verifySelectionError cs ps = \case
    SelectionBalanceErrorOf e ->
        verifySelectionBalanceError cs ps e
    SelectionCollateralErrorOf e ->
        verifySelectionCollateralError cs ps e

--------------------------------------------------------------------------------
-- Selection error verification: balance errors
--------------------------------------------------------------------------------

verifySelectionBalanceError
    :: SelectionContext ctx
    => VerifySelectionError (SelectionBalanceError ctx) ctx
verifySelectionBalanceError cs ps = \case
    Balance.BalanceInsufficient e ->
        verifyBalanceInsufficientError cs ps e
    Balance.EmptyUTxO ->
        verifyEmptyUTxOError cs ps ()
    Balance.UnableToConstructChange e->
        verifyUnableToConstructChangeError cs ps e

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
    :: VerifySelectionError Balance.BalanceInsufficientError ctx
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

verifyEmptyUTxOError :: SelectionContext ctx => VerifySelectionError () ctx
verifyEmptyUTxOError _cs SelectionParams {utxoAvailableForInputs} _e =
    verify
        (utxoAvailableForInputs == UTxOSelection.empty)
        (FailureToVerifyEmptyUTxOError {utxoAvailableForInputs})

--------------------------------------------------------------------------------
-- Selection error verification: change construction errors
--------------------------------------------------------------------------------

data FailureToVerifyUnableToConstructChangeError ctx =
    FailureToVerifyUnableToConstructChangeError
        { errorOriginal
            :: Balance.UnableToConstructChangeError
            -- ^ The original error.
        , errorWithMinimalConstraints
            :: SelectionError ctx
            -- ^ An error encountered when attempting to re-run the selection
            -- process with minimal constraints.
        }

deriving instance SelectionContext ctx =>
    Eq (FailureToVerifyUnableToConstructChangeError ctx)
deriving instance SelectionContext ctx =>
    Show (FailureToVerifyUnableToConstructChangeError ctx)

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
    :: forall ctx. SelectionContext ctx
    => VerifySelectionError Balance.UnableToConstructChangeError ctx
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
    resultWithMinimalConstraints :: Either (SelectionError ctx) (Selection ctx)
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
            { computeMinimumAdaQuantity = const $ const $ Coin 0
            , computeMinimumCost = const $ Coin 0
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
    :: forall ctx. SelectionContext ctx
    => VerifySelectionError (SelectionCollateralError ctx) ctx
verifySelectionCollateralError cs ps e =
    verifyAll
        [ Map.null largestCombinationUnsuitableSubset
        , largestCombinationSize <= maximumSelectionSize
        , largestCombinationValue < minimumSelectionAmount
        ]
        (FailureToVerifySelectionCollateralError {..})
  where
    largestCombination :: Map (UTxO ctx) Coin
    largestCombination = e ^. #largestCombinationAvailable
    largestCombinationSize :: Int
    largestCombinationSize = Map.size largestCombination
    largestCombinationValue :: Coin
    largestCombinationValue = F.fold largestCombination

    largestCombinationUnsuitableSubset :: Map (UTxO ctx) Coin
    largestCombinationUnsuitableSubset = Map.withoutKeys
        (largestCombination)
        (Map.keysSet $ ps ^. #utxoAvailableForCollateral)

    maximumSelectionSize :: Int
    maximumSelectionSize = cs ^. #maximumCollateralInputCount
    minimumSelectionAmount :: Coin
    minimumSelectionAmount = e ^. #minimumSelectionAmount

--------------------------------------------------------------------------------
-- Selection deltas
--------------------------------------------------------------------------------

-- | Calculates the selection delta for all assets.
--
-- See 'SelectionDelta'.
--
selectionDeltaAllAssets :: Selection ctx -> SelectionDelta TokenBundle
selectionDeltaAllAssets = Balance.selectionDeltaAllAssets . toBalanceResult

-- | Calculates the ada selection delta.
--
-- See 'SelectionDelta'.
--
selectionDeltaCoin :: Selection ctx -> SelectionDelta Coin
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
    :: SelectionConstraints ctx -> SelectionParams ctx -> Selection ctx -> Bool
selectionHasValidSurplus constraints params selection =
    Balance.selectionHasValidSurplus
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Computes the minimum required cost of a selection.
--
selectionMinimumCost
    :: SelectionConstraints ctx -> SelectionParams ctx -> Selection ctx -> Coin
selectionMinimumCost constraints params selection =
    Balance.selectionMinimumCost
        (fst $ toBalanceConstraintsParams (constraints, params))
        (toBalanceResult selection)

-- | Computes the maximum acceptable cost of a selection.
--
selectionMaximumCost
    :: SelectionConstraints ctx -> SelectionParams ctx -> Selection ctx -> Coin
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
selectionSurplusCoin :: Selection ctx -> Coin
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
selectionCollateralRequired :: SelectionParams ctx -> Bool
selectionCollateralRequired params = case view #collateralRequirement params of
    SelectionCollateralRequired    -> True
    SelectionCollateralNotRequired -> False

-- | Applies the given transformation function only when collateral is required.
--
whenCollateralRequired
    :: SelectionParams ctx
    -> (a -> a)
    -> (a -> a)
whenCollateralRequired params f
    | selectionCollateralRequired params = f
    | otherwise = id

-- | Computes the total amount of collateral within a selection.
--
selectionCollateral :: Selection ctx -> Coin
selectionCollateral = F.foldMap snd . view #collateral

-- | Indicates whether or not a selection has sufficient collateral.
--
selectionHasSufficientCollateral
    :: SelectionConstraints ctx -> SelectionParams ctx -> Selection ctx -> Bool
selectionHasSufficientCollateral constraints params selection =
    actual >= required
  where
    actual = selectionCollateral selection
    required = selectionMinimumCollateral constraints params selection

-- | Computes the minimum required amount of collateral for a selection.
--
selectionMinimumCollateral
    :: SelectionConstraints ctx -> SelectionParams ctx -> Selection ctx -> Coin
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
-- Validating outputs
--------------------------------------------------------------------------------

newtype SelectionOutputSizeExceedsLimitError ctx =
    SelectionOutputSizeExceedsLimitError
    { outputThatExceedsLimit :: (Address ctx, TokenBundle)
    }
    deriving Generic

deriving instance SelectionContext ctx =>
    Eq (SelectionOutputSizeExceedsLimitError ctx)
deriving instance SelectionContext ctx =>
    Show (SelectionOutputSizeExceedsLimitError ctx)

-- | Verifies the size of an output.
--
-- Returns 'SelectionOutputSizeExceedsLimitError' if and only if the size
-- exceeds the limit defined by the protocol.
--
verifyOutputSize
    :: SelectionConstraints ctx
    -> (Address ctx, TokenBundle)
    -> Maybe (SelectionOutputSizeExceedsLimitError ctx)
verifyOutputSize cs out = case isWithinLimit (snd out) of
    TokenBundleSizeWithinLimit ->
        Nothing
    TokenBundleSizeExceedsLimit ->
        Just $ SelectionOutputSizeExceedsLimitError out
  where
    isWithinLimit = cs ^. (#tokenBundleSizeAssessor . #assessTokenBundleSize)

-- | Indicates that a token quantity exceeds the maximum quantity that can
--   appear in a transaction output's token bundle.
--
data SelectionOutputTokenQuantityExceedsLimitError ctx =
    SelectionOutputTokenQuantityExceedsLimitError
    { address :: !(Address ctx)
      -- ^ The address to which this token quantity was to be sent.
    , asset :: !AssetId
      -- ^ The asset identifier to which this token quantity corresponds.
    , quantity :: !TokenQuantity
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: !TokenQuantity
      -- ^ The maximum allowable token quantity.
    }
    deriving Generic

deriving instance SelectionContext ctx =>
    Eq (SelectionOutputTokenQuantityExceedsLimitError ctx)
deriving instance SelectionContext ctx =>
    Show (SelectionOutputTokenQuantityExceedsLimitError ctx)

-- | Verifies the token quantities of an output.
--
-- Returns a list of token quantities that exceed the limit defined by the
-- protocol.
--
verifyOutputTokenQuantities
    :: SelectionConstraints ctx
    -> (Address ctx, TokenBundle)
    -> [SelectionOutputTokenQuantityExceedsLimitError ctx]
verifyOutputTokenQuantities _cs out =
    [ SelectionOutputTokenQuantityExceedsLimitError
        {address, asset, quantity, quantityMaxBound = txOutMaxTokenQuantity}
    | let address = fst out
    , (asset, quantity) <- TokenMap.toFlatList $ (snd out) ^. #tokens
    , quantity > txOutMaxTokenQuantity
    ]
