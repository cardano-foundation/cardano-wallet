{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Provides an algorithm for producing a balanced coin selection with change,
-- where the fee is paid for.
--
-- This module uses the Random-Round-Robin coin selection algorithm for
-- multi-asset UTxO sets.
--
-- See documentation for the 'performSelection' function for more details on
-- how to perform a selection.
--
module Cardano.Wallet.CoinSelection.Internal.Balance
    (
    -- * Performing a selection
      PerformSelection
    , performSelection
    , performSelectionEmpty
    , SelectionConstraints (..)
    , SelectionParams
    , SelectionParamsOf (..)
    , SelectionSkeleton (..)
    , SelectionResult
    , SelectionResultOf (..)
    , SelectionStrategy (..)
    , SelectionBalanceError (..)
    , BalanceInsufficientError (..)
    , InsufficientMinCoinValueError (..)
    , UnableToConstructChangeError (..)

    -- * Selection limits
    , SelectionLimit
    , SelectionLimitOf (..)
    , selectionLimitExceeded
    , SelectionLimitReachedError (..)
    , reduceSelectionLimitBy

    -- * Querying selections
    , SelectionDelta (..)
    , selectionDeltaAllAssets
    , selectionDeltaCoin
    , selectionHasValidSurplus
    , selectionSurplusCoin
    , selectionMinimumCost
    , selectionMaximumCost
    , selectionSkeleton

    -- * Querying parameters
    , UTxOBalanceSufficiency (..)
    , UTxOBalanceSufficiencyInfo (..)
    , computeBalanceInOut
    , computeDeficitInOut
    , computeUTxOBalanceAvailable
    , computeUTxOBalanceRequired
    , computeUTxOBalanceSufficiency
    , computeUTxOBalanceSufficiencyInfo
    , isUTxOBalanceSufficient

    -- * Running a selection (without making change)
    , runSelection
    , runSelectionNonEmpty
    , runSelectionNonEmptyWith
    , RunSelectionParams (..)

    -- * Running a selection step
    , runSelectionStep
    , SelectionLens (..)
    , assetSelectionLens
    , coinSelectionLens

    -- * Making change
    , MakeChangeCriteria (..)
    , makeChange
    , makeChangeForCoin
    , makeChangeForUserSpecifiedAsset
    , makeChangeForNonUserSpecifiedAsset
    , makeChangeForNonUserSpecifiedAssets
    , assignCoinsToChangeMaps
    , collateNonUserSpecifiedAssetQuantities
    , addMintValueToChangeMaps
    , addMintValuesToChangeMaps
    , removeBurnValueFromChangeMaps
    , removeBurnValuesFromChangeMaps
    , reduceTokenQuantities

    -- * Splitting bundles
    , splitBundleIfAssetCountExcessive
    , splitBundlesWithExcessiveAssetCounts
    , splitBundlesWithExcessiveTokenQuantities

    -- * Grouping and ungrouping
    , groupByKey
    , ungroupByKey

    -- * Round-robin processing
    , runRoundRobin
    , runRoundRobinM

    -- * Utility classes
    , AssetCount (..)

    -- * Utility functions
    , distance
    , mapMaybe
    , balanceMissing
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( padCoalesce )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, Flat (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( SelectionFilter (..), UTxOIndex (..) )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( IsUTxOSelection, UTxOSelection, UTxOSelectionNonEmpty )
import Control.Monad.Extra
    ( andM )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Bifunctor
    ( first )
import Data.Either.Extra
    ( maybeToEither )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( comparing )
import Data.Semigroup
    ( mtimesDefault )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), Builder, blockMapF, nameF, unlinesF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

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
data SelectionConstraints address = SelectionConstraints
    { assessTokenBundleSize
        :: TokenBundle -> TokenBundleSizeAssessment
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output. See documentation for
        -- the 'TokenBundleSizeAssessor' type to learn about the expected
        -- properties of this field.
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
    }
    deriving Generic

type SelectionParams = SelectionParamsOf []

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParamsOf outputs address u = SelectionParams
    { outputsToCover
        :: !(outputs (address, TokenBundle))
        -- ^ The complete set of outputs to be covered.
    , utxoAvailable
        :: !(UTxOSelection u)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- inputs and optionally, a subset that has already been selected.
        --
        -- Further entries from this set will be selected to cover any deficit.
    , extraCoinSource
        :: !Coin
        -- ^ An extra source of ada.
    , extraCoinSink
        :: !Coin
        -- ^ An extra sink for ada.
    , assetsToMint
        :: !TokenMap
        -- ^ Assets to mint: these provide input value to a transaction.
        --
        -- By minting tokens, we generally decrease the burden of the selection
        -- algorithm, allowing it to select fewer UTxO entries in order to
        -- cover the required outputs.
    , assetsToBurn
        :: !TokenMap
        -- ^ Assets to burn: these consume output value from a transaction.
        --
        -- By burning tokens, we generally increase the burden of the selection
        -- algorithm, requiring it to select more UTxO entries in order to
        -- cover the burn.
    , selectionStrategy
        :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving Generic

deriving instance
    (Eq (outputs (address, TokenBundle)), Eq u) =>
        Eq (SelectionParamsOf outputs address u)

deriving instance
    (Show (outputs (address, TokenBundle)), Show u) =>
        Show (SelectionParamsOf outputs address u)

-- | Indicates a choice of selection strategy.
--
-- A 'SelectionStrategy' determines __how much__ of each asset the selection
-- algorithm will attempt to select from the available UTxO set, relative to
-- the minimum amount necessary to make the selection balance.
--
-- The default 'SelectionStrategy' is 'SelectionStrategyOptimal', which when
-- specified will cause the selection algorithm to attempt to select around
-- __/twice/__ the minimum possible amount of each asset from the available
-- UTxO set, making it possible to generate change outputs that are roughly
-- the same sizes and shapes as the user-specified outputs.
--
-- Specifying 'SelectionStrategyMinimal' will cause the selection algorithm to
-- only select __just enough__ of each asset from the available UTxO set to
-- meet the minimum amount. The selection process will terminate as soon as
-- the minimum amount of each asset is covered.
--
-- The "optimal" strategy is recommended for most situations, as using this
-- strategy will help to ensure that a wallet's UTxO distribution can evolve
-- over time to resemble the typical distribution of payments made by the
-- wallet owner.  This increases the likelihood that future selections will
-- succeed, and lowers the amortized cost of future transactions.
--
-- The "minimal" strategy is recommended only for situations where it is not
-- possible to create a selection with the "optimal" strategy. It is advised to
-- use this strategy only when necessary, as it increases the likelihood of
-- generating change outputs that are much smaller than user-specified outputs.
-- If this strategy is used regularly, the UTxO set can evolve to a state where
-- the distribution no longer resembles the typical distribution of payments
-- made by the user. This increases the likelihood that future selections will
-- not succeed, and increases the amortized cost of future transactions.
--
data SelectionStrategy
    = SelectionStrategyMinimal
    | SelectionStrategyOptimal
    deriving (Bounded, Enum, Eq, Show)

-- | Indicates whether the balance of available UTxO entries is sufficient.
--
-- See 'computeUTxOBalanceSufficiency'.
--
data UTxOBalanceSufficiency
    = UTxOBalanceSufficient
      -- ^ Indicates that the UTxO balance is sufficient.
    | UTxOBalanceInsufficient
      -- ^ Indicates that the UTxO balance is insufficient.
    deriving (Eq, Show)

-- | Gives more information about UTxO balance sufficiency.
--
-- See 'computeUTxOBalanceSufficiencyInfo'.
--
data UTxOBalanceSufficiencyInfo = UTxOBalanceSufficiencyInfo
    { available :: TokenBundle
      -- ^ See 'computeUTxOBalanceAvailable'.
    , required :: TokenBundle
      -- ^ See 'computeUTxOBalanceRequired'.
    , difference :: TokenBundle
      -- ^ The difference between 'available' and 'required'.
    , sufficiency :: UTxOBalanceSufficiency
      -- ^ Whether or not the balance is sufficient.
    }
    deriving (Eq, Generic, Show)

-- | Computes the balance of UTxO entries available for selection.
--
computeUTxOBalanceAvailable
    :: SelectionParamsOf outputs address u -> TokenBundle
computeUTxOBalanceAvailable =
    UTxOSelection.availableBalance . view #utxoAvailable

-- | Computes the balance of UTxO entries required to be selected.
--
computeUTxOBalanceRequired
    :: Foldable outputs => SelectionParamsOf outputs address u -> TokenBundle
computeUTxOBalanceRequired = fst . computeDeficitInOut

computeBalanceInOut
    :: Foldable outputs
    => SelectionParamsOf outputs address u
    -> (TokenBundle, TokenBundle)
computeBalanceInOut params =
    (balanceIn, balanceOut)
  where
    balanceIn =
        TokenBundle.fromTokenMap (view #assetsToMint params)
        `TokenBundle.add`
        TokenBundle.fromCoin (view #extraCoinSource params)
    balanceOut =
        TokenBundle.fromTokenMap (view #assetsToBurn params)
        `TokenBundle.add`
        TokenBundle.fromCoin (view #extraCoinSink params)
        `TokenBundle.add`
        F.foldMap snd (view #outputsToCover params)

computeDeficitInOut
    :: Foldable outputs
    => SelectionParamsOf outputs address u
    -> (TokenBundle, TokenBundle)
computeDeficitInOut params =
    (deficitIn, deficitOut)
  where
    deficitIn =
        TokenBundle.difference balanceOut balanceIn
    deficitOut =
        TokenBundle.difference balanceIn balanceOut
    (balanceIn, balanceOut) =
        computeBalanceInOut params

-- | Computes the UTxO balance sufficiency.
--
-- See 'UTxOBalanceSufficiency'.
--
computeUTxOBalanceSufficiency
    :: Foldable outputs
    => SelectionParamsOf outputs address u
    -> UTxOBalanceSufficiency
computeUTxOBalanceSufficiency = sufficiency . computeUTxOBalanceSufficiencyInfo

-- | Computes information about the UTxO balance sufficiency.
--
-- See 'UTxOBalanceSufficiencyInfo'.
--
computeUTxOBalanceSufficiencyInfo
    :: Foldable outputs
    => SelectionParamsOf outputs address u
    -> UTxOBalanceSufficiencyInfo
computeUTxOBalanceSufficiencyInfo params =
    UTxOBalanceSufficiencyInfo {available, required, difference, sufficiency}
  where
    available = computeUTxOBalanceAvailable params
    required = computeUTxOBalanceRequired params
    sufficiency =
        if required `leq` available
        then UTxOBalanceSufficient
        else UTxOBalanceInsufficient
    difference =
        if sufficiency == UTxOBalanceSufficient
        then TokenBundle.difference available required
        else TokenBundle.difference required available

-- | Indicates whether or not the UTxO balance is sufficient.
--
-- The balance of available UTxO entries is sufficient if (and only if) it
-- is greater than or equal to the required balance.
--
isUTxOBalanceSufficient
    :: Foldable outputs => SelectionParamsOf outputs address u -> Bool
isUTxOBalanceSufficient params =
    case computeUTxOBalanceSufficiency params of
        UTxOBalanceSufficient   -> True
        UTxOBalanceInsufficient -> False

-- | A skeleton selection that can be used to estimate the cost of a final
--   selection.
--
-- Change outputs are deliberately stripped of their asset quantities, as the
-- fee estimation function must be agnostic to the magnitudes of these
-- quantities.
--
-- Increasing or decreasing the quantity of a particular asset in a change
-- output must not change the estimated cost of a selection.
--
data SelectionSkeleton address = SelectionSkeleton
    { skeletonInputCount
        :: !Int
    , skeletonOutputs
        :: ![(address, TokenBundle)]
    , skeletonChange
        :: ![Set AssetId]
    }
    deriving (Eq, Generic, Show)

-- | Specifies a limit to adhere to when performing a selection.
--
type SelectionLimit = SelectionLimitOf Int

data SelectionLimitOf a
    = NoLimit
      -- ^ Indicates that there is no limit.
    | MaximumInputLimit a
      -- ^ Indicates a maximum limit on the number of inputs to select.
    deriving (Eq, Functor, Show)

instance Ord a => Ord (SelectionLimitOf a) where
    compare a b = case (a, b) of
        (NoLimit            , NoLimit            ) -> EQ
        (MaximumInputLimit _, NoLimit            ) -> LT
        (NoLimit            , MaximumInputLimit _) -> GT
        (MaximumInputLimit x, MaximumInputLimit y) -> compare x y

-- | Indicates whether or not the given selection limit has been exceeded.
--
selectionLimitExceeded :: IsUTxOSelection s u => s u -> SelectionLimit -> Bool
selectionLimitExceeded s = \case
    NoLimit -> False
    MaximumInputLimit n -> UTxOSelection.selectedSize s > n

-- | Reduces a selection limit by a given reduction amount.
--
-- If the given reduction amount is positive, then this function will reduce
-- the selection limit by that amount.
--
-- If the given reduction amount is zero or negative, then this function will
-- return the original limit unchanged.
--
reduceSelectionLimitBy :: SelectionLimit -> Int -> SelectionLimit
reduceSelectionLimitBy limit reduction
    | reduction <= 0 =
        limit
    | otherwise =
        subtract reduction <$> limit

type SelectionResult = SelectionResultOf []

-- | The result of performing a successful selection.
--
data SelectionResultOf outputs address u = SelectionResult
    { inputsSelected
        :: !(NonEmpty (u, TokenBundle))
        -- ^ A (non-empty) list of inputs selected from 'utxoAvailable'.
    , extraCoinSource
        :: !Coin
        -- ^ An extra source of ada.
    , extraCoinSink
        :: !Coin
        -- ^ An extra sink for ada.
    , outputsCovered
        :: !(outputs (address, TokenBundle))
        -- ^ A list of outputs covered.
    , changeGenerated
        :: ![TokenBundle]
        -- ^ A list of generated change outputs.
    , assetsToMint
        :: !TokenMap
        -- ^ The assets to mint.
    , assetsToBurn
        :: !TokenMap
        -- ^ The assets to burn.
    }
    deriving Generic

deriving instance
    (Eq (outputs (address, TokenBundle)), Eq u) =>
        Eq (SelectionResultOf outputs address u)
deriving instance
    (Show (outputs (address, TokenBundle)), Show u) =>
        Show (SelectionResultOf outputs address u)

-- | Indicates the difference between total input value and total output value
--   of a 'SelectionResult'.
--
-- There are two possibilities:
--
--  - 'SelectionSurplus'
--
--    Indicates a surplus, when the total input value is greater than or equal
--    to the total output value.
--
--  - 'SelectionDeficit'
--
--    Indicates a deficit, when the total input value is NOT greater than or
--    equal to the total output value.
--
data SelectionDelta a
    = SelectionSurplus a
    | SelectionDeficit a
    deriving (Eq, Functor, Show)

instance Buildable a => Buildable (SelectionDelta a) where
    build d = case d of
        SelectionSurplus surplus -> buildMap [("surplus", build surplus)]
        SelectionDeficit deficit -> buildMap [("deficit", build deficit)]
      where
        buildMap :: [(String, Builder)] -> Builder
        buildMap = blockMapF . fmap (first $ id @String)

-- | Calculates the selection delta for all assets.
--
-- See 'SelectionDelta'.
--
selectionDeltaAllAssets
    :: Foldable outputs
    => SelectionResultOf outputs address u
    -> SelectionDelta TokenBundle
selectionDeltaAllAssets result
    | balanceOut `leq` balanceIn =
        SelectionSurplus $ TokenBundle.difference balanceIn balanceOut
    | otherwise =
        SelectionDeficit $ TokenBundle.difference balanceOut balanceIn
  where
    balanceIn =
        TokenBundle.fromTokenMap assetsToMint
        `TokenBundle.add`
        TokenBundle.fromCoin extraCoinSource
        `TokenBundle.add`
        F.foldMap snd inputsSelected
    balanceOut =
        TokenBundle.fromTokenMap assetsToBurn
        `TokenBundle.add`
        TokenBundle.fromCoin extraCoinSink
        `TokenBundle.add`
        F.foldMap snd outputsCovered
        `TokenBundle.add`
        F.fold changeGenerated
    SelectionResult
        { assetsToMint
        , assetsToBurn
        , extraCoinSource
        , extraCoinSink
        , inputsSelected
        , outputsCovered
        , changeGenerated
        } = result

-- | Calculates the ada selection delta.
--
-- See 'SelectionDelta'.
--
selectionDeltaCoin
    :: Foldable outputs
    => SelectionResultOf outputs address u
    -> SelectionDelta Coin
selectionDeltaCoin = fmap TokenBundle.getCoin . selectionDeltaAllAssets

-- | Indicates whether or not a selection result has a valid surplus.
--
selectionHasValidSurplus
    :: Foldable outputs
    => SelectionConstraints address
    -> SelectionResultOf outputs address u
    -> Bool
selectionHasValidSurplus constraints selection =
    case selectionDeltaAllAssets selection of
        SelectionSurplus s -> surplusIsValid s
        SelectionDeficit _ -> False
  where
    surplusIsValid :: TokenBundle -> Bool
    surplusIsValid = andM
        [ surplusHasNoNonAdaAssets
        , surplusNotBelowMinimumCost
        , surplusNotAboveMaximumCost
        ]

    -- None of the non-ada assets can have a surplus.
    surplusHasNoNonAdaAssets :: TokenBundle -> Bool
    surplusHasNoNonAdaAssets surplus =
        view #tokens surplus == TokenMap.empty

    -- The surplus must not be less than the minimum cost.
    surplusNotBelowMinimumCost :: TokenBundle -> Bool
    surplusNotBelowMinimumCost surplus =
        view #coin surplus >= selectionMinimumCost constraints selection

    -- The surplus must not be greater than the maximum cost.
    surplusNotAboveMaximumCost :: TokenBundle -> Bool
    surplusNotAboveMaximumCost surplus =
        view #coin surplus <= selectionMaximumCost constraints selection

-- | Calculates the ada selection surplus, assuming there is a surplus.
--
-- If there is a surplus, then this function returns that surplus.
-- If there is a deficit, then this function returns zero.
--
-- Use 'selectionDeltaCoin' if you wish to handle the case where there is
-- a deficit.
--
selectionSurplusCoin
    :: Foldable outputs
    => SelectionResultOf outputs address u
    -> Coin
selectionSurplusCoin result =
    case selectionDeltaCoin result of
        SelectionSurplus surplus -> surplus
        SelectionDeficit _       -> Coin 0

-- | Converts a selection into a skeleton.
--
selectionSkeleton
    :: Foldable outputs
    => SelectionResultOf outputs address u
    -> SelectionSkeleton address
selectionSkeleton s = SelectionSkeleton
    { skeletonInputCount = F.length (view #inputsSelected s)
    , skeletonOutputs = F.toList (view #outputsCovered s)
    , skeletonChange = TokenBundle.getAssets <$> view #changeGenerated s
    }

-- | Computes the minimum required cost of a selection.
--
selectionMinimumCost
    :: Foldable outputs
    => SelectionConstraints address
    -> SelectionResultOf outputs address u
    -> Coin
selectionMinimumCost c = view #computeMinimumCost c . selectionSkeleton

-- | Computes the maximum acceptable cost of a selection.
--
-- This function acts as a safety limit to ensure that fees of selections
-- produced by 'performSelection' are not excessively high.
--
-- Ideally, we'd always be able to generate selections with fees that are
-- precisely equal to 'selectionMinimumCost'. However, in some situations
-- it may be necessary to exceed this cost very slightly.
--
-- This function provides a conservative upper bound to a selection cost
-- that we can reference from within property tests.
--
-- See 'selectionHasValidSurplus'.
--
selectionMaximumCost
    :: Foldable outputs
    => SelectionConstraints address
    -> SelectionResultOf outputs address u
    -> Coin
selectionMaximumCost c = mtimesDefault (2 :: Int) . selectionMinimumCost c

-- | Represents the set of errors that may occur while performing a selection.
--
data SelectionBalanceError address u
    = BalanceInsufficient
        BalanceInsufficientError
    | SelectionLimitReached
        (SelectionLimitReachedError address u)
    | InsufficientMinCoinValues
        (NonEmpty (InsufficientMinCoinValueError address))
    | UnableToConstructChange
        UnableToConstructChangeError
    | EmptyUTxO
    deriving (Generic, Eq, Show)

-- | Indicates that the balance of selected UTxO entries was insufficient to
--   cover the balance required while remaining within the selection limit.
--
data SelectionLimitReachedError address u = SelectionLimitReachedError
    { utxoBalanceRequired
        :: !TokenBundle
      -- ^ The UTXO balance required.
    , inputsSelected
        :: ![(u, TokenBundle)]
      -- ^ The inputs that could be selected while satisfying the
      -- 'selectionLimit'.
    , outputsToCover
        :: !(NonEmpty (address, TokenBundle))
    } deriving (Generic, Eq, Show)

-- | Indicates that the balance of available UTxO entries is insufficient to
--   cover the balance required.
--
-- See 'computeUTxOBalanceSufficiency'.
--
data BalanceInsufficientError = BalanceInsufficientError
    { utxoBalanceAvailable
        :: !TokenBundle
      -- ^ The balance of 'utxoAvailable'.
    , utxoBalanceRequired
        :: !TokenBundle
      -- ^ The balance of 'outputsToCover'.
    } deriving (Generic, Eq, Show)

-- | Calculate the missing balance from a @BalanceInsufficientError@.
balanceMissing :: BalanceInsufficientError -> TokenBundle
balanceMissing (BalanceInsufficientError available required) =
    TokenBundle.difference required available

-- | Indicates that a particular output does not have the minimum coin quantity
--   expected by the protocol.
--
-- See also: 'prepareOutputs'.
--
data InsufficientMinCoinValueError address = InsufficientMinCoinValueError
    { outputWithInsufficientAda
        :: !(address, TokenBundle)
        -- ^ The particular output that does not have the minimum coin quantity
        -- expected by the protocol.
    , expectedMinCoinValue
        :: !Coin
        -- ^ The minimum coin quantity expected for this output.
    } deriving (Generic, Eq, Show)

instance Buildable address =>
    Buildable (InsufficientMinCoinValueError address)
  where
    build (InsufficientMinCoinValueError (a, b) c) = unlinesF
        [ nameF "Expected min coin value" (build c)
        , nameF "Address" (build a)
        , nameF "Token bundle" (build (Flat b))
        ]

data UnableToConstructChangeError = UnableToConstructChangeError
    { requiredCost
        :: !Coin
        -- ^ The minimal required cost needed for the transaction to be
        -- considered valid. This does not include min Ada values.
    , shortfall
        :: !Coin
        -- ^ The additional coin quantity that would be required to cover the
        -- selection cost and minimum coin quantity of each change output.
    } deriving (Generic, Eq, Show)

type PerformSelection m outputs address u =
    SelectionConstraints address ->
    SelectionParamsOf outputs address u ->
    m (
        Either
            (SelectionBalanceError address u)
            (SelectionResultOf outputs address u)
    )

-- | Performs a coin selection and generates change bundles in one step.
--
-- Provided that 'isUTxOBalanceSufficient' returns 'True' for the given
-- selection criteria, this function guarantees to return a 'SelectionResult'
-- for which 'selectionHasValidSurplus' returns 'True'.
--
performSelection
    :: forall m address u.
        (HasCallStack, MonadRandom m, Ord u, Show address, Show u)
    => PerformSelection m [] address u
performSelection = performSelectionEmpty performSelectionNonEmpty

-- | Transforms a coin selection function that requires a non-empty list of
--   outputs into a function that accepts an empty list of outputs.
--
-- If the original list is already non-empty, this function does not alter the
-- parameters or the result in any way, such that:
--
--    params == transformParams params
--    result == transformResult result
--
-- If the original list is empty, this function:
--
--   1. applies a balance-preserving transformation to the parameters, adding
--      a single minimal ada-only output to act as a change generation target,
--      such that:
--
--          computeUTxOBalanceSufficiencyInfo params ==
--          computeUTxOBalanceSufficiencyInfo (transformParams params)
--
--   2. applies an inverse transformation to the result, removing the output,
--      such that:
--
--          selectionSurplus result ==
--          selectionSurplus (transformResult result)
--
--          selectionHasValidSurplus constraints result ==>
--          selectionHasValidSurplus constraints (transformResult result)
--
performSelectionEmpty
    :: forall m address u. Functor m
    => PerformSelection m NonEmpty address u
    -> PerformSelection m []       address u
performSelectionEmpty performSelectionFn constraints params =
    fmap transformResult <$>
    performSelectionFn constraints (transformParams params)
  where
    transformParams
        :: SelectionParamsOf []       address u
        -> SelectionParamsOf NonEmpty address u
    transformParams
        = over #extraCoinSource
            (transform (`Coin.add` minCoin) (const id))
        . over #outputsToCover
            (transform (const (dummyOutput :| [])) (const . id))

    transformResult
        :: SelectionResultOf NonEmpty address u
        -> SelectionResultOf []       address u
    transformResult
        = over #extraCoinSource
            (transform (`Coin.difference` minCoin) (const id))
        . over #outputsCovered
            (transform (const []) (const . F.toList))

    transform :: a -> (NonEmpty (address, TokenBundle) -> a) -> a
    transform x y = maybe x y $ NE.nonEmpty $ view #outputsToCover params

    dummyOutput :: (address, TokenBundle)
    dummyOutput =
        -- TODO: ADP-1448
        --
        -- Replace this call to 'error' with a call to a function that
        -- generates a dummy address.
        (error "dummy address", TokenBundle.fromCoin minCoin)

    -- The 'performSelectionNonEmpty' function imposes a precondition that all
    -- outputs must have at least the minimum ada quantity. Therefore, the
    -- dummy output must also satisfy this condition.
    --
    -- However, we must also ensure that the value is non-zero, since:
    --
    --   1. Under some cost models, the 'computeMinimumAdaQuantity' function
    --      has a constant value of zero.
    --
    --   2. The change generation algorithm requires that the total ada balance
    --      of all outputs is non-zero.
    --
    minCoin :: Coin
    minCoin = max
        (Coin 1)
        (view #computeMinimumAdaQuantity constraints TokenMap.empty)

performSelectionNonEmpty
    :: forall m address u.
        (HasCallStack, MonadRandom m, Ord u, Show address, Show u)
    => PerformSelection m NonEmpty address u
performSelectionNonEmpty constraints params
    -- Is the total available UTXO balance sufficient?
    | not utxoBalanceSufficient =
        pure $ Left $ BalanceInsufficient $ BalanceInsufficientError
            {utxoBalanceAvailable, utxoBalanceRequired}

    -- Are the minimum ada quantities of the outputs too small?
    | not (null insufficientMinCoinValues) =
        pure $ Left $ InsufficientMinCoinValues $
            NE.fromList insufficientMinCoinValues

    | otherwise = do
        maybeSelection <- runSelectionNonEmpty RunSelectionParams
            { selectionLimit
            , utxoAvailable
            , minimumBalance = utxoBalanceRequired
            , selectionStrategy
            }
        case maybeSelection of
            Nothing | utxoAvailable == UTxOSelection.empty ->
                pure $ Left EmptyUTxO
            Nothing ->
                selectionLimitReachedError []
            Just selection | selectionLimitExceeded selection selectionLimit ->
                selectionLimitReachedError $ F.toList $
                    UTxOSelection.selectedList selection
            Just selection -> do
                let utxoSelected = UTxOSelection.selectedIndex selection
                let utxoBalanceSelected = UTxOIndex.balance utxoSelected
                if utxoBalanceRequired `leq` utxoBalanceSelected
                then makeChangeRepeatedly selection
                else selectionLimitReachedError (UTxOIndex.toList utxoSelected)
  where
    SelectionConstraints
        { assessTokenBundleSize
        , computeMinimumAdaQuantity
        , computeMinimumCost
        , computeSelectionLimit
        } = constraints
    SelectionParams
        { outputsToCover
        , utxoAvailable
        , extraCoinSource
        , extraCoinSink
        , assetsToMint
        , assetsToBurn
        , selectionStrategy
        } = params

    selectionLimitReachedError
        :: [(u, TokenBundle)] -> m (Either (SelectionBalanceError address u) a)
    selectionLimitReachedError inputsSelected =
        pure $ Left $ SelectionLimitReached $ SelectionLimitReachedError
            { inputsSelected
            , utxoBalanceRequired
            , outputsToCover
            }

    selectionLimit :: SelectionLimit
    selectionLimit = computeSelectionLimit $ F.toList outputsToCover

    utxoBalanceAvailable :: TokenBundle
    utxoBalanceAvailable = computeUTxOBalanceAvailable params

    utxoBalanceRequired :: TokenBundle
    utxoBalanceRequired = computeUTxOBalanceRequired params

    utxoBalanceSufficient :: Bool
    utxoBalanceSufficient = isUTxOBalanceSufficient params

    insufficientMinCoinValues :: [InsufficientMinCoinValueError address]
    insufficientMinCoinValues =
        mapMaybe mkInsufficientMinCoinValueError outputsToCover
      where
        mkInsufficientMinCoinValueError
            :: (address, TokenBundle)
            -> Maybe (InsufficientMinCoinValueError address)
        mkInsufficientMinCoinValueError o
            | view #coin (snd o) >= expectedMinCoinValue =
                Nothing
            | otherwise =
                Just $ InsufficientMinCoinValueError
                    { expectedMinCoinValue
                    , outputWithInsufficientAda = o
                    }
          where
            expectedMinCoinValue = computeMinimumAdaQuantity
                (view #tokens $ snd o)

    -- Given a UTxO index that corresponds to a valid selection covering
    -- 'outputsToCover', 'predictChange' yields a non-empty list of assets
    -- expected for change outputs.
    --
    -- There's a chicken-and-egg situation when it comes to calculating
    -- transaction fees. On the one hand, we need to know the shape of the
    -- final transaction to calculate its cost. But in order to construct the
    -- transaction, we need to know what its cost is.
    --
    -- So, in order to not duplicate the logic from 'makeChange', we first
    -- calculate a pre-selection considering the case where we have no fees to
    -- pay, and no minimum value. This is *guaranteed to succeed* and will
    -- yield a selection with change outputs in the final shape (modulo
    -- amounts).
    --
    -- The result of calling 'predictChange' with a valid input selection
    -- should satisfy:
    --
    --     length predictedChange === length outputsToCover
    --
    --     flat predictChange `isSubsetOf` assets selectedInputs
    --
    --     ∃ params. / isRight (performSelection params) =>
    --         Right predictedChange === assets <$> performSelection params
    --
    --     (That is, the predicted change is necessarily equal to the change
    --     assets of the final resulting selection).
    --
    predictChange :: UTxOSelectionNonEmpty u -> [Set AssetId]
    predictChange s = either
        (const $ invariantResultWithNoCost $ UTxOSelection.selectedIndex s)
        (fmap (TokenMap.getAssets . view #tokens))
        (makeChange MakeChangeCriteria
            { minCoinFor = noMinimumCoin
            , bundleSizeAssessor = TokenBundleSizeAssessor assessTokenBundleSize
            , requiredCost = noCost
            , extraCoinSource
            , extraCoinSink
            , inputBundles
            , outputBundles
            , assetsToMint
            , assetsToBurn
            }
        )
      where
        inputBundles = snd <$> UTxOSelection.selectedList s
        outputBundles = snd <$> outputsToCover

        noMinimumCoin :: TokenMap -> Coin
        noMinimumCoin = const (Coin 0)

        noCost :: Coin
        noCost = Coin 0

    -- This function takes the given selection skeleton as a way to evaluate
    -- the cost of a final selection, and then calls 'makeChange' repeatedly
    -- until it succeeds.
    --
    -- Between each call, it selects an extra ada-only input to inject
    -- additional ada to construct change outputs.
    --
    -- Eventually it returns just a final selection, or 'Nothing' if no more
    -- ada-only inputs are available.
    --
    -- This function also takes a set of tokens that are to be burned, and
    -- hence although one or more inputs will be consumed for them, this
    -- function won't make associated outputs for them.
    --
    makeChangeRepeatedly
        :: UTxOSelectionNonEmpty u
        -> m
            (Either
                (SelectionBalanceError address u)
                (SelectionResultOf NonEmpty address u)
            )
    makeChangeRepeatedly s = case mChangeGenerated of

        Right change | length change >= length outputsToCover ->
            -- We've succeeded in making at least the optimal number of change
            -- outputs, and can terminate here.
            --
            -- Note that we can't use an exact length equality check here, as
            -- the 'makeChange' function will split up change outputs if they
            -- are oversized in any way. (See 'splitOversizedMaps'.)
            --
            -- It is therefore possible for 'makeChange' to generate more change
            -- outputs than the number of user-specified outputs.
            --
            pure $ Right $ mkSelectionResult change

        Right change ->
            -- We've succeeded in making change outputs, but the number of
            -- change outputs is fewer than optimal, because the supply of ada
            -- was insufficient. Try again with more ada to see if it leads to
            -- an improvement:
            selectOneEntry s >>= \case
                Just s' ->
                    makeChangeRepeatedly s'
                Nothing ->
                    -- There is no more ada available. Terminate with a
                    -- less-than-optimal number of change outputs.
                    pure $ Right $ mkSelectionResult change

        Left changeErr ->
            -- We've failed to make any change outputs, because the supply of
            -- ada was insufficient. Try again with more ada.
            selectOneEntry s >>= \case
                Just s' ->
                    makeChangeRepeatedly s'
                Nothing ->
                    -- There is no more ada available, and we were unable to
                    -- make any change. At this point we must simply give up.
                    pure $ Left $ UnableToConstructChange changeErr
      where
        mChangeGenerated :: Either UnableToConstructChangeError [TokenBundle]
        mChangeGenerated = makeChange MakeChangeCriteria
            { minCoinFor = computeMinimumAdaQuantity
            , bundleSizeAssessor = TokenBundleSizeAssessor assessTokenBundleSize
            , requiredCost
            , extraCoinSource
            , extraCoinSink
            , inputBundles = snd <$> inputsSelected
            , outputBundles = snd <$> outputsToCover
            , assetsToMint
            , assetsToBurn
            }

        mkSelectionResult
            :: [TokenBundle]
            -> SelectionResultOf NonEmpty address u
        mkSelectionResult changeGenerated = SelectionResult
            { inputsSelected
            , extraCoinSource
            , extraCoinSink
            , changeGenerated = changeGenerated
            , outputsCovered = outputsToCover
            , assetsToMint
            , assetsToBurn
            }

        selectOneEntry = selectCoinQuantity selectionLimit

        requiredCost = computeMinimumCost SelectionSkeleton
            { skeletonInputCount = UTxOSelection.selectedSize s
            , skeletonOutputs = NE.toList outputsToCover
            , skeletonChange
            }

        skeletonChange = predictChange s
        inputsSelected = UTxOSelection.selectedList s

    invariantResultWithNoCost inputs_ = error $ unlines
        -- This should be impossible, as the 'makeChange' function should
        -- always succeed if there's no extra cost or minimum value to assign.
        -- This is because it is called with the result of 'runSelection',
        -- which only terminates successfully if the target was satisfied.
        [ "performSelection: couldn't construct change for a selection with no "
        , "minimum coin value and no cost!"
        , "inputs: " <> show inputs_
        , "extra coin source: " <> show extraCoinSource
        , "extra coin sink: " <> show extraCoinSink
        , "outputs: " <> show outputsToCover
        ]

--------------------------------------------------------------------------------
-- Running a selection (without making change)
--------------------------------------------------------------------------------

-- | Parameters for 'runSelection'.
--
data RunSelectionParams u = RunSelectionParams
    { selectionLimit :: SelectionLimit
        -- ^ A limit to adhere to when performing a selection.
    , utxoAvailable :: (UTxOSelection u)
        -- ^ UTxO entries available for selection.
    , minimumBalance :: TokenBundle
        -- ^ Minimum balance to cover.
    , selectionStrategy :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving (Eq, Generic, Show)

runSelectionNonEmpty
    :: (MonadRandom m, Ord u)
    => RunSelectionParams u
    -> m (Maybe (UTxOSelectionNonEmpty u))
runSelectionNonEmpty = (=<<)
    <$> runSelectionNonEmptyWith . selectCoinQuantity . view #selectionLimit
    <*> runSelection

runSelectionNonEmptyWith
    :: Monad m
    => (UTxOSelection u -> m (Maybe (UTxOSelectionNonEmpty u)))
    -> UTxOSelection u
    -> m (Maybe (UTxOSelectionNonEmpty u))
runSelectionNonEmptyWith selectSingleEntry result =
    UTxOSelection.toNonEmpty result & maybe
        (result & selectSingleEntry)
        (pure . Just)

runSelection
    :: forall m u. (MonadRandom m, Ord u)
    => RunSelectionParams u
    -> m (UTxOSelection u)
runSelection params =
    runRoundRobinM utxoAvailable UTxOSelection.fromNonEmpty selectors
  where
    RunSelectionParams
        { selectionLimit
        , utxoAvailable
        , minimumBalance
        , selectionStrategy
        } = params

    -- NOTE: We run the 'coinSelector' last, because we know that every input
    -- necessarily has a non-zero ada amount. By running the other selectors
    -- first, we increase the probability that the coin selector will be able
    -- to terminate without needing to select an additional coin.
    selectors :: [UTxOSelection u -> m (Maybe (UTxOSelectionNonEmpty u))]
    selectors =
        reverse (coinSelector : fmap assetSelector minimumAssetQuantities)
      where
        assetSelector = runSelectionStep .
            assetSelectionLens selectionLimit selectionStrategy
        coinSelector = runSelectionStep $
            coinSelectionLens selectionLimit selectionStrategy
            minimumCoinQuantity

    (minimumCoinQuantity, minimumAssetQuantities) =
        TokenBundle.toFlatList minimumBalance

assetSelectionLens
    :: (MonadRandom m, Ord u)
    => SelectionLimit
    -> SelectionStrategy
    -> (AssetId, TokenQuantity)
    -> SelectionLens m (UTxOSelection u) (UTxOSelectionNonEmpty u)
assetSelectionLens limit strategy (asset, minimumAssetQuantity) = SelectionLens
    { currentQuantity = selectedAssetQuantity asset
    , updatedQuantity = selectedAssetQuantity asset
    , minimumQuantity = unTokenQuantity minimumAssetQuantity
    , selectQuantity = selectAssetQuantity asset limit
    , selectionStrategy = strategy
    }

coinSelectionLens
    :: (MonadRandom m, Ord u)
    => SelectionLimit
    -> SelectionStrategy
    -> Coin
    -- ^ Minimum coin quantity.
    -> SelectionLens m (UTxOSelection u) (UTxOSelectionNonEmpty u)
coinSelectionLens limit strategy minimumCoinQuantity = SelectionLens
    { currentQuantity = selectedCoinQuantity
    , updatedQuantity = selectedCoinQuantity
    , minimumQuantity = intCast $ unCoin minimumCoinQuantity
    , selectQuantity  = selectCoinQuantity limit
    , selectionStrategy = strategy
    }

-- | Specializes 'selectMatchingQuantity' to a particular asset.
--
selectAssetQuantity
    :: (MonadRandom m, Ord u)
    => IsUTxOSelection utxoSelection u
    => AssetId
    -> SelectionLimit
    -> utxoSelection u
    -> m (Maybe (UTxOSelectionNonEmpty u))
selectAssetQuantity asset =
    selectMatchingQuantity (WithAssetOnly asset :| [WithAsset asset])

-- | Specializes 'selectMatchingQuantity' to ada.
--
selectCoinQuantity
    :: (MonadRandom m, Ord u)
    => IsUTxOSelection utxoSelection u
    => SelectionLimit
    -> utxoSelection u
    -> m (Maybe (UTxOSelectionNonEmpty u))
selectCoinQuantity =
    selectMatchingQuantity (WithAdaOnly :| [Any])

-- | Selects a UTxO entry that matches one of the specified filters.
--
-- This function traverses the specified list of filters from left to right, in
-- descending order of priority.
--
-- When considering a particular filter:
--
--    - if the function is able to select a UTxO entry that matches, it
--      terminates with an updated selection state that includes the entry.
--
--    - if the function is not able to select a UTxO entry that matches, it
--      traverses to the next filter available.
--
-- This function returns 'Nothing' if (and only if) it traverses the entire
-- list of filters without successfully selecting a UTxO entry.
--
selectMatchingQuantity
    :: forall m utxoSelection u. (MonadRandom m, Ord u)
    => IsUTxOSelection utxoSelection u
    => NonEmpty SelectionFilter
        -- ^ A list of selection filters to be traversed from left-to-right,
        -- in descending order of priority.
    -> SelectionLimit
        -- ^ A limit to adhere to when selecting entries.
    -> utxoSelection u
        -- ^ The current selection state.
    -> m (Maybe (UTxOSelectionNonEmpty u))
        -- ^ An updated selection state that includes a matching UTxO entry,
        -- or 'Nothing' if no such entry could be found.
selectMatchingQuantity filters limit s
    | limitReached =
        pure Nothing
    | otherwise =
        (updateState =<<) <$> UTxOIndex.selectRandomWithPriority
            (UTxOSelection.leftoverIndex s) filters
  where
    limitReached = case limit of
        MaximumInputLimit m -> UTxOSelection.selectedSize s >= m
        NoLimit -> False

    updateState
        :: ((u, TokenBundle), UTxOIndex u) -> Maybe (UTxOSelectionNonEmpty u)
    updateState ((i, _b), _remaining) = UTxOSelection.select i s

--------------------------------------------------------------------------------
-- Running a selection step
--------------------------------------------------------------------------------

-- | Provides a lens on the current selection state.
--
-- A 'SelectionLens' gives 'runSelectionStep' just the information it needs to
-- make a decision, and no more.
--
data SelectionLens m state state' = SelectionLens
    { currentQuantity
        :: state -> Natural
    , updatedQuantity
        :: state' -> Natural
    , selectQuantity
        :: state -> m (Maybe state')
    , minimumQuantity
        :: Natural
    , selectionStrategy
        :: SelectionStrategy
    }

-- | Runs just a single step of a coin selection.
--
-- It returns an updated state if (and only if) the updated selection
-- represents an improvement over the selection in the previous state.
--
-- An improvement, for a given token quantity, is defined in the following way:
--
--    - If the total selected token quantity of the previous selection had
--      not yet reached 100% of the output token quantity, any additional
--      selection is considered to be an improvement.
--
--    - If the total selected token quantity of the previous selection had
--      already reached or surpassed 100% of the output token quantity, any
--      additional selection is considered to be an improvement if and only
--      if it takens the total selected token quantity closer to the target
--      token quantity, but not further away.
--
runSelectionStep
    :: forall m state state'. Monad m
    => SelectionLens m state state'
    -> state
    -> m (Maybe state')
runSelectionStep lens s
    | currentQuantity s < minimumQuantity =
        selectQuantity s
    | otherwise =
        (requireImprovement =<<) <$> selectQuantity s
  where
    SelectionLens
        { currentQuantity
        , updatedQuantity
        , minimumQuantity
        , selectQuantity
        , selectionStrategy
        } = lens

    requireImprovement :: state' -> Maybe state'
    requireImprovement s'
        | updatedDistanceFromTarget s' < currentDistanceFromTarget s = Just s'
        | otherwise = Nothing

    currentDistanceFromTarget :: state -> Natural
    currentDistanceFromTarget = distance targetQuantity . currentQuantity

    updatedDistanceFromTarget :: state' -> Natural
    updatedDistanceFromTarget = distance targetQuantity . updatedQuantity

    targetMultiplier :: Natural
    targetMultiplier = case selectionStrategy of
        SelectionStrategyMinimal -> 1
        SelectionStrategyOptimal -> 2

    targetQuantity :: Natural
    targetQuantity = minimumQuantity * targetMultiplier

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

-- | Criteria for the 'makeChange' function.
--
data MakeChangeCriteria minCoinFor bundleSizeAssessor = MakeChangeCriteria
    { minCoinFor :: minCoinFor
      -- ^ A function that computes the minimum required ada quantity for a
      -- particular output.
    , bundleSizeAssessor :: bundleSizeAssessor
        -- ^ A function to assess the size of a token bundle.
    , requiredCost :: Coin
      -- ^ The minimal (and optimal) delta between the total ada balance
      -- of all input bundles and the total ada balance of all output and
      -- change bundles, where:
      --
      --    delta = getCoin (fold inputBundles)
      --          - getCoin (fold outputBundles)
      --          - getCoin (fold changeBundles)
      --
      -- This typically captures fees plus key deposits.
    , extraCoinSource :: Coin
        -- ^ An extra source of ada.
    , extraCoinSink :: Coin
        -- ^ An extra sink for ada.
    , inputBundles :: NonEmpty TokenBundle
        -- ^ Token bundles of selected inputs.
    , outputBundles :: NonEmpty TokenBundle
        -- ^ Token bundles of original outputs.
    , assetsToMint :: TokenMap
        -- ^ Assets to mint: these provide input value to a transaction.
    , assetsToBurn :: TokenMap
        -- ^ Assets to burn: these consume output value from a transaction.
    } deriving (Eq, Generic, Show)

-- | Indicates 'True' if and only if a token bundle exceeds the maximum size
--   that can be included in a transaction output.
--
tokenBundleSizeExceedsLimit :: TokenBundleSizeAssessor -> TokenBundle -> Bool
tokenBundleSizeExceedsLimit (TokenBundleSizeAssessor assess) b =
    case assess b of
        TokenBundleSizeWithinLimit->
            False
        TokenBundleSizeExceedsLimit ->
            True

-- | Constructs change bundles for a set of selected inputs and outputs.
--
-- Returns 'Nothing' if the specified inputs do not provide enough ada to
-- satisfy the minimum delta and minimum ada quantities of the change bundles
-- generated.
--
-- This function will generate runtime errors if:
--
--    1.  The total balance of all outputs is not less than or equal to the
--        total balance of all inputs.
--
--    2.  The total ada balance of all outputs is zero.
--
-- Pre-condition (1) should be satisfied by any result produced by the
-- 'runSelection' function.
--
-- Pre-condition (2) should be satisfied by assigning a minimum ada quantity
-- to every output token bundle.
--
makeChange
    :: MakeChangeCriteria (TokenMap -> Coin) TokenBundleSizeAssessor
        -- ^ Criteria for making change.
    -> Either UnableToConstructChangeError [TokenBundle]
        -- ^ Generated change bundles.
makeChange criteria
    | not (totalOutputValue `leq` totalInputValue) =
        totalInputValueInsufficient
    | TokenBundle.getCoin totalOutputValue == Coin 0 =
        totalOutputCoinValueIsZero
    | otherwise =
        first mkUnableToConstructChangeError $ do
            adaAvailable <- maybeToEither
                (requiredCost `Coin.difference` excessCoin)
                (excessCoin `Coin.subtract` requiredCost)
            assignCoinsToChangeMaps
                adaAvailable minCoinFor changeMapOutputCoinPairs
  where
    MakeChangeCriteria
        { minCoinFor
        , bundleSizeAssessor
        , requiredCost
        , extraCoinSource
        , extraCoinSink
        , inputBundles
        , outputBundles
        , assetsToMint
        , assetsToBurn
        } = criteria

    -- The following subtraction is safe, as we have already checked
    -- that the total input value is greater than the total output
    -- value:
    excess :: TokenBundle
    excess = totalInputValue `TokenBundle.unsafeSubtract` totalOutputValue

    (excessCoin, excessAssets) = TokenBundle.toFlatList excess

    -- Change maps for all assets, where each change map is paired with a
    -- corresponding coin from the original outputs.
    --
    -- When combining change maps from user-specified assets and non-user-
    -- specified assets, we arrange that any empty maps in either list are
    -- combined together if possible, so as to give 'assignCoinsToChangeMaps'
    -- the greatest chance of success.
    --
    -- This list is sorted into ascending order of asset count, where empty
    -- change maps are all located at the start of the list.
    --
    changeMapOutputCoinPairs :: NonEmpty (TokenMap, Coin)
    changeMapOutputCoinPairs = outputCoins
        -- First, combine the original output coins with the change maps for
        -- user-specified assets. We must pair these together right at the
        -- start in order to retain proportionality with the original outputs.
        & NE.zip changeForUserSpecifiedAssets
        -- Next, sort the list into ascending order of asset count, which moves
        -- any empty maps to the start of the list:
        & NE.sortWith (AssetCount . fst)
        -- Next, combine the existing list with the change maps for non-user
        -- specified assets, which are already sorted into ascending order of
        -- asset count:
        & NE.zipWith (\m1 (m2, c) -> (m1 <> m2, c))
            changeForNonUserSpecifiedAssets
        -- Finally, if there are any maps that are oversized (in any way), then
        -- split these maps up along with their corresponding output coins:
        & splitOversizedMaps
      where
        splitOversizedMaps
            :: NonEmpty (TokenMap, Coin) -> NonEmpty (TokenMap, Coin)
        splitOversizedMaps =
            -- For the sake of convenience when splitting up change maps and
            -- output coins (which are treated as weights), treat each change
            -- map and its corresponding output coin as a token bundle.
            fmap unbundle . split . fmap bundle
          where
            bundle (m, c) = TokenBundle c m
            unbundle (TokenBundle c m) = (m, c)
            split b = b
                & flip splitBundlesWithExcessiveAssetCounts
                    (tokenBundleSizeExceedsLimit assessBundleSizeWithMaxCoin)
                & flip splitBundlesWithExcessiveTokenQuantities
                    txOutMaxTokenQuantity

            -- When assessing the size of a change map to determine if it is
            -- excessively large, we don't yet know how large the associated
            -- ada quantity will be, since ada quantities are assigned at a
            -- later stage (in 'assignCoinsToChangeMaps').
            --
            -- Therefore, we err on the side of caution, and assess the size
            -- of a change map combined with the maximum possible ada quantity.
            --
            -- This means that when presented with a very large change map, we
            -- have a small chance of splitting the map even if that map would
            -- be within the limit when combined with its final ada quantity.
            --
            -- However, oversplitting a change map is preferable to creating
            -- a bundle that is marginally over the limit, which would cause
            -- the resultant transaction to be rejected.
            --
            assessBundleSizeWithMaxCoin :: TokenBundleSizeAssessor
            assessBundleSizeWithMaxCoin = TokenBundleSizeAssessor
                $ view #assessTokenBundleSize bundleSizeAssessor
                . flip TokenBundle.setCoin txOutMaxCoin

    -- Change for user-specified assets: assets that were present in the
    -- original set of user-specified outputs ('outputsToCover').
    changeForUserSpecifiedAssets :: NonEmpty TokenMap
    changeForUserSpecifiedAssets = F.foldr
        (NE.zipWith (<>)
            . makeChangeForUserSpecifiedAsset outputMaps)
        (TokenMap.empty <$ outputMaps)
        excessAssets

    -- Change for non-user-specified assets: assets that were not present
    -- in the original set of user-specified outputs ('outputsToCover').
    changeForNonUserSpecifiedAssets :: NonEmpty TokenMap
    changeForNonUserSpecifiedAssets =
        makeChangeForNonUserSpecifiedAssets
            outputMaps
            nonUserSpecifiedAssetQuantities
        & addMintValuesToChangeMaps
            (removeUserSpecifiedAssetIds assetsToMint)
        & removeBurnValuesFromChangeMaps
            (removeUserSpecifiedAssetIds assetsToBurn)
      where
        removeUserSpecifiedAssetIds :: TokenMap -> TokenMap
        removeUserSpecifiedAssetIds =
            TokenMap.filter (`Set.notMember` userSpecifiedAssetIds)

    totalInputValueInsufficient = error
        "makeChange: not (totalOutputValue <= totalInputValue)"
    totalOutputCoinValueIsZero = error
        "makeChange: not (totalOutputCoinValue > 0)"

    mkUnableToConstructChangeError :: Coin -> UnableToConstructChangeError
    mkUnableToConstructChangeError shortfall = UnableToConstructChangeError
        { requiredCost
        , shortfall
        }

    outputMaps :: NonEmpty TokenMap
    outputMaps = view #tokens <$> outputBundles

    outputCoins :: NonEmpty Coin
    outputCoins = view #coin <$> outputBundles

    totalInputValue :: TokenBundle
    totalInputValue =
        F.fold inputBundles
            <> TokenBundle.fromCoin extraCoinSource
            -- Mints represent extra inputs from "the void"
            <> TokenBundle.fromTokenMap assetsToMint

    totalOutputValue :: TokenBundle
    totalOutputValue =
        F.fold outputBundles
            <> TokenBundle.fromCoin extraCoinSink
            -- Burns represent extra outputs to "the void"
            <> TokenBundle.fromTokenMap assetsToBurn

    -- Identifiers of all user-specified assets: assets that were included in
    -- the original set of outputs.
    userSpecifiedAssetIds :: Set AssetId
    userSpecifiedAssetIds = TokenBundle.getAssets (F.fold outputBundles)

    -- Identifiers and quantities of all non-user-specified assets: assets that
    -- were not included in the original set of outputs, but that were
    -- nevertheless selected during the selection process.
    --
    -- Each asset is paired with the complete list of quantities of that asset
    -- present in the selected inputs.
    nonUserSpecifiedAssetQuantities :: Map AssetId (NonEmpty TokenQuantity)
    nonUserSpecifiedAssetQuantities =
        collateNonUserSpecifiedAssetQuantities
            (view #tokens <$> inputBundles) userSpecifiedAssetIds

-- | Generates a map of all non-user-specified assets and their quantities.
--
-- Each key in the resulting map corresponds to an asset that was NOT included
-- in the original set of user-specified outputs, but that was nevertheless
-- selected during the selection process.
--
-- The value associated with each key corresponds to the complete list of all
-- discrete non-zero quantities of that asset present in the selected inputs.
--
collateNonUserSpecifiedAssetQuantities
    :: NonEmpty TokenMap
      -- ^ Token maps of all selected inputs.
    -> Set AssetId
      -- ^ Set of all assets in user-specified outputs.
    -> Map AssetId (NonEmpty TokenQuantity)
collateNonUserSpecifiedAssetQuantities inputMaps userSpecifiedAssetIds =
    F.foldr discardUserSpecifiedAssets mempty inputMaps
  where
    discardUserSpecifiedAssets
        :: TokenMap
        -> Map AssetId (NonEmpty TokenQuantity)
        -> Map AssetId (NonEmpty TokenQuantity)
    discardUserSpecifiedAssets tokens m =
        foldr (\(k, v) -> Map.insertWith (<>) k (v :| [])) m filtered
      where
        filtered = filter
            ((`Set.notMember` userSpecifiedAssetIds) . fst)
            (TokenMap.toFlatList tokens)

-- | Assigns coin quantities to a list of pre-computed asset change maps.
--
-- Each pre-computed asset change map must be paired with the original coin
-- value of its corresponding output.
--
-- This function:
--
--    - expects the list of pre-computed asset change maps to be sorted in an
--      order that ensures all empty token maps are at the start of the list.
--
--    - attempts to assign a minimum ada quantity to every change map, but
--      iteratively drops empty change maps from the start of the list if the
--      amount of ada is insufficient to cover them all.
--
--    - continues dropping empty change maps from the start of the list until
--      it is possible to assign a minimum ada value to all remaining entries.
--
--    - returns a list that is identical in length to the input list if (and
--      only if) it was possible to assign a minimum ada quantity to all change
--      maps.
--
--    - returns a list that is shorter than the input list if it was only
--      possible to assign a minimum ada quantity to a suffix of the given
--      list.
--
--    - fails if (and only if) there was not enough ada available to assign the
--      minimum ada quantity to all non-empty change maps.
--
assignCoinsToChangeMaps
    :: HasCallStack
    => Coin
    -- ^ The total quantity of ada available, including any extra source of ada.
    -> (TokenMap -> Coin)
    -- ^ A function to calculate the minimum required ada quantity for any
    -- token map.
    -> NonEmpty (TokenMap, Coin)
    -- ^ A list of pre-computed asset change maps paired with original output
    -- coins, sorted into an order that ensures all empty token maps are at the
    -- start of the list.
    -> Either Coin [TokenBundle]
    -- ^ Resulting change bundles, or the shortfall quantity if there was not
    -- enough ada available to assign a minimum ada quantity to all non-empty
    -- token maps.
assignCoinsToChangeMaps adaAvailable minCoinFor pairsAtStart
    | not changeMapsCorrectlyOrdered =
        changeMapsNotCorrectlyOrderedError
    | otherwise =
        loop adaRequiredAtStart pairsAtStart
  where
    loop !adaRequired !pairsNonEmpty = case pairsNonEmpty of

        pair :| pairs | adaAvailable >= adaRequired ->
            -- We have enough ada available to pay for the minimum required
            -- amount of every asset map that remains in our list:
            let assetMapsRemaining = fst <$> (pair :| pairs) in
            let bundlesForAssetsWithMinimumCoins =
                    assignMinimumCoin minCoinFor <$> assetMapsRemaining in
            -- Calculate the amount of ada that remains after assigning the
            -- minimum amount to each map. This should be safe, as we have
            -- already determined that we have enough ada available:
            let adaRemaining = adaAvailable `Coin.distance` adaRequired in
            -- Partition any remaining ada according to the weighted
            -- distribution of output coins that remain in our list:
            let outputCoinsRemaining = snd <$> (pair :| pairs) in
            let bundlesForOutputCoins = TokenBundle.fromCoin <$>
                    makeChangeForCoin outputCoinsRemaining adaRemaining in
            -- Finally, combine the minimal coin asset bundles with the
            -- bundles obtained by partitioning the remaining ada amount:
            Right $ NE.toList $ NE.zipWith (<>)
                bundlesForAssetsWithMinimumCoins
                bundlesForOutputCoins

        (m, _) :| (p : ps) | TokenMap.isEmpty m && adaAvailable < adaRequired ->
            -- We don't have enough ada available to pay for the minimum
            -- required amount of every asset map, but we do have an empty
            -- asset map that is safe to drop. This will reduce the amount of
            -- ada required by a small amount:
            let adaRequired' = adaRequired `Coin.distance` minCoinFor m in
            loop adaRequired' (p :| ps)

        (m, _) :| [] | TokenMap.isEmpty m && adaAvailable < adaRequired ->
            -- We didn't have any non-ada assets at all in our change, and we
            -- also don't have enough ada available to pay even for a single
            -- change output. We just burn the available ada amount (which
            -- will be small), returning no change.
            Right []

        _ ->
            -- We don't have enough ada available, and there are no empty token
            -- maps available to drop. We have to give up at this point.
            Left (adaRequired `Coin.difference` adaAvailable)

    adaRequiredAtStart = F.fold $ minCoinFor . fst <$> pairsAtStart

    changeMaps = fst <$> pairsAtStart

    -- Indicates whether or not the given change maps are correctly ordered,
    -- so that all empty maps are located at the start of the list.
    changeMapsCorrectlyOrdered = (==)
        (NE.takeWhile TokenMap.isEmpty changeMaps)
        (NE.filter TokenMap.isEmpty changeMaps)

    changeMapsNotCorrectlyOrderedError =
        error $ unwords
            [ "assignCoinsToChangeMaps: pre-computed asset change maps must be"
            , "arranged in an order where all empty maps are at the start of"
            , "the list."
            ]

-- | Assigns the minimum required ada quantity to a token map.
--
assignMinimumCoin :: (TokenMap -> Coin) -> TokenMap -> TokenBundle
assignMinimumCoin minCoinFor m = TokenBundle (minCoinFor m) m

-- | Constructs change outputs for a user-specified asset: an asset that was
--   present in the original set of outputs.
--
-- If the given asset does not appear in the given distribution, this function
-- returns a list of empty token maps. Otherwise, the given token quantity is
-- partitioned into a list of quantities that are proportional to the weights
-- within the given input distribution, modulo rounding.
--
-- The length of the output list is always the same as the the length of the
-- input list, and the sum of its quantities is either zero, or exactly equal
-- to the token quantity in the second argument.
--
makeChangeForUserSpecifiedAsset
    :: NonEmpty TokenMap
        -- ^ A list of weights for the distribution. Conveniently captures both
        -- the weights, and the number of elements amongst which the quantity
        -- should be distributed.
    -> (AssetId, TokenQuantity)
        -- ^ A surplus token quantity to distribute.
    -> NonEmpty TokenMap
makeChangeForUserSpecifiedAsset targets (asset, excess) =
    TokenMap.singleton asset <$>
        fromMaybe zeros (TokenQuantity.partition excess weights)
  where
    weights :: NonEmpty TokenQuantity
    weights = flip TokenMap.getQuantity asset <$> targets

    zeros :: NonEmpty TokenQuantity
    zeros = TokenQuantity 0 <$ targets

-- | Constructs change outputs for a non-user-specified asset: an asset that
--   was not present in the original set of outputs.
--
-- This function constructs a list of change outputs by preserving the input
-- distribution as much as possible. Note that only the length of the first
-- argument is used.
--
-- The length of the output list is always the same as the length of the input
-- list, and the sum of its quantities is always exactly equal to the sum of
-- all token quantities given in the second argument.
--
-- The resultant list is sorted into ascending order when maps are compared
-- with the `leq` function.
--
makeChangeForNonUserSpecifiedAsset
    :: NonEmpty a
        -- ^ Determines the number of change maps to create.
    -> (AssetId, NonEmpty TokenQuantity)
        -- ^ An asset quantity to distribute.
    -> NonEmpty TokenMap
        -- ^ The resultant change maps.
makeChangeForNonUserSpecifiedAsset n (asset, quantities) =
    TokenMap.singleton asset <$> padCoalesce quantities n

-- | Constructs change outputs for all non-user-specified assets: assets that
--   were not present in the original set of outputs.
--
-- The resultant list is sorted into ascending order when maps are compared
-- with the `leq` function.
--
makeChangeForNonUserSpecifiedAssets
    :: NonEmpty a
        -- ^ Determines the number of change maps to create.
    -> Map AssetId (NonEmpty TokenQuantity)
        -- ^ A map of asset quantities to distribute.
    -> NonEmpty TokenMap
        -- ^ The resultant change maps.
makeChangeForNonUserSpecifiedAssets n nonUserSpecifiedAssetQuantities =
    F.foldr
        (NE.zipWith (<>) . makeChangeForNonUserSpecifiedAsset n)
        (TokenMap.empty <$ n)
        (Map.toList nonUserSpecifiedAssetQuantities)

-- | Constructs a list of ada change outputs based on the given distribution.
--
-- If the sum of weights in given distribution is equal to zero, this function
-- throws a runtime error.
--
-- The length of the output list is always the same as the length of the input
-- list, and the sum of its quantities is always exactly equal to the 'Coin'
-- value given as the second argument.
--
makeChangeForCoin
    :: HasCallStack
    => NonEmpty Coin
        -- ^ A list of weights for the distribution. Conveniently captures both
        -- the weights, and the number of elements amongst which the surplus
        -- ada quantity should be distributed.
    -> Coin
        -- ^ A surplus ada quantity to be distributed.
    -> NonEmpty Coin
makeChangeForCoin = flip Coin.unsafePartition

--------------------------------------------------------------------------------
-- Minting and burning
--------------------------------------------------------------------------------

-- Once we know how much change to give, grouping the change into bundles is a
-- somewhat complicated topic.
--
-- We want to create change outputs with, as far as possible, values that are
-- likely to be useful to the user in future, where values that more closely
-- approximate the user-specified outputs are considered more "useful".
--
-- A key property is that the number of change outputs should reflect the
-- number of outputs specified by the user. For example, if the user sends
-- value to five distinct outputs, we should create five distinct change
-- outputs.
--
-- However, we also want to mint and burn tokens. In general, minting tokens
-- requires us to add value to the change outputs and burning tokens requires
-- us to remove value from the change outputs.
--
-- It's also important to note that the change bundle calculation requires
-- that the change for user-specified and non-user-specified assets have the
-- following properties:
--
--    1. The lists share the same length;
--    2. The lists are in ascending partial order.
--
-- For example, given the following non-user-specified asset quantities:
--
--    [ ("A", [4, 1, 3, 2])
--    , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
--    ]
--
-- If the user requests 5 outputs in their transaction,
-- 'makeChangeForNonUserSpecifiedAssets' will generate:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ]
--
-- That is to say, it generates change bundles that satisfy the following
-- properties:
--
--    1.  The number of change bundles matches the number of outputs
--        the user originally requested;
--    2.  The change bundles are split in such a way to maximize the
--        number of large change bundles.
--
-- The change function maintains the property that the change bundles are in
-- ascending partial order, such that each change bundle is a subset of the
-- next. This property is required by 'changeMapOutputCoinPairs', so it's
-- important it's maintained.
--
-- The following two functions work by modifying the change bundles for
-- non-user-specified assets.
--
-- We add minted tokens to the largest change bundle:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ] <-- add minted tokens here
--
-- We remove burned tokens from the smallest change bundles, until all burned
-- tokens are removed:
--
--    [ [          ("B", 7)  ] <-- start removing burned tokens from here
--    [ [("A", 1), ("B", 8)  ] <-- if we must burn more, remove from here
--    [ [("A", 2), ("B", 9)  ] <-- if we must burn more, remove from here
--    [ [("A", 3), ("B", 9)  ] <-- and so on, until we've removed everything.
--    [ [("A", 4), ("B", 12) ]
--
-- The solution for minting maintains the properties we desire, namely:
--
--    1.  The number of change bundles matches the number of
--        "outputs to cover" (we are not changing the number of bundles).
--    2.  The change bundles are in ascending partial order (by adding to the
--        largest bundle we trivially maintain ordering).
--    3.  The change bundles are split in such a way to maximize the
--        number of large change bundles.
--
-- The solution for burning maintains the same properties:
--
--    1.  The number of change bundles is not changed, in the case we burn a
--        change bundle completely, we just leave it as an empty entry
--        (effectively "pad with zeros").
--    2.  By removing from the smallest bundle, we maintain the ascending
--        partial order of the change bundles.
--    3.  By removing from the smallest bundles, we remove the "least useful"
--        bundles, maximizing the overall usefulness of our bundles.

-- | Adds a minted asset quantity to a list of change maps.
--
-- This function always adds the given quantity to the final change map in the
-- given list.
--
-- Example:
--
-- Suppose we have the following list of change maps:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ]
--
-- If we add 4 tokens of asset "A", we obtain the following result:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 8), ("B", 12) ] -- Increased by 4
--
-- Provided that the specified change maps are in ascending partial order, this
-- function guarantees that the resulting change maps will also be in ascending
-- partial order.
--
-- The length of the given list is preserved in the output list.
--
addMintValueToChangeMaps
    :: (AssetId, TokenQuantity)
    -> NonEmpty TokenMap
    -> NonEmpty TokenMap
addMintValueToChangeMaps (assetId, assetQty) =
    -- The largest element is the last element in an ascending order list
    modifyLast $ \m -> TokenMap.adjustQuantity m assetId (<> assetQty)
  where
    modifyLast f xs = case NE.reverse xs of
        (y :| ys) -> NE.reverse (f y :| ys)

-- | Adds minted values for multiple assets to a list of change maps.
--
-- Plural of @addMintValueToChangeMaps@.
--
addMintValuesToChangeMaps
    :: TokenMap
    -- ^ Map of minted values
    -> NonEmpty TokenMap
    -- ^ Change maps
    -> NonEmpty TokenMap
    -- ^ Change maps with minted values
addMintValuesToChangeMaps =
    flip (F.foldr addMintValueToChangeMaps) . TokenMap.toFlatList

-- | Removes a burned asset quantity from a list of change maps.
--
-- For a given asset 'a' and reduction target 't', this function traverses the
-- given list from left to right, reducing the quantity of asset 'a' in each
-- change map until the reduction target 't' has been met, or until the list
-- is exhausted.
--
-- For each change map 'm' under consideration:
--
--    - if the quantity 'q' of asset 'a' in map 'm' is less than or equal to
--      the remaining required reduction 'r', it will be replaced with a zero
--      (effectively eliminating asset 'a' from the map).
--
--    - if the quantity 'q' of asset 'a' in map 'm' is greater than the
--      remaining required reduction 'r', it will be replaced with the
--      absolute difference between 'q' and 'r'.
--
-- If the total quantity of the given asset in the given change maps is greater
-- than the specified reduction target, the total reduction will be equal to
-- the specified reduction target. Otherwise, the given asset will be
-- completely eliminated from all change maps.
--
-- Example:
--
-- Suppose we have the following list of change maps:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ]
--
-- If our target is to reduce the quantity of asset "A" by 4, then we should
-- obtain the following result:
--
--    [ [          ("B",  7) ] -- Unable to reduce (already 0)
--    [ [          ("B",  8) ] -- Reduced by 1 (and eliminated from map)
--    [ [          ("B",  9) ] -- Reduced by 2 (and eliminated from map)
--    [ [("A", 2), ("B",  9) ] -- Reduced by 1
--    [ [("A", 4), ("B", 12) ]
--
-- Provided that the specified change maps are in ascending partial order, this
-- function guarantees that the resulting change maps will also be in ascending
-- partial order.
--
-- The length of the given list is preserved in the output list.
--
removeBurnValueFromChangeMaps
    :: (AssetId, TokenQuantity)
    -- ^ Asset quantity reduction target
    -> NonEmpty TokenMap
    -- ^ Change maps with quantities of the given asset to be reduced
    -> NonEmpty TokenMap
    -- ^ Change maps with reduced quantities of the given asset
removeBurnValueFromChangeMaps (assetId, assetQty) maps = maps
    & fmap (`TokenMap.getQuantity` assetId)
    & reduceTokenQuantities assetQty
    & NE.zipWith (`TokenMap.setQuantity` assetId) maps

-- | Reduces the total value of the given list of token quantities by the given
--   reduction target.
--
-- This function traverses the given list of quantities from left to right,
-- reducing each quantity in turn until the total reduction is equal to the
-- given reduction target, or until the list is exhausted.
--
-- For each quantity 'q' under consideration:
--
--    - if 'q' is less than or equal to the remaining required reduction 'r',
--      it will be replaced with a zero.
--
--    - if 'q' is greater than the remaining required reduction 'r', it will
--      be replaced with the absolute difference between 'q' and 'r'.
--
-- If the total value in the list is less than the reduction target, the
-- result will be a list of zeros.
--
-- Provided the given list is in ascending order, the resulting list is also
-- guaranteed to be in ascending order.
--
-- The length of the given list is preserved in the output.
--
reduceTokenQuantities
    :: TokenQuantity
    -- ^ Reduction target
    -> NonEmpty TokenQuantity
    -- ^ List of quantities to reduce
    -> NonEmpty TokenQuantity
    -- ^ The list of reduced quantities
reduceTokenQuantities reductionTarget quantities =
    NE.fromList $ burn reductionTarget (NE.toList quantities) []
  where
    burn _ [      ] ys = reverse ys
    burn b (x : xs) ys
        | x >= b = reverse ys <> (x' : xs)
        | otherwise = burn b' xs (x' : ys)
      where
        b' = b `TokenQuantity.difference` x
        x' = x `TokenQuantity.difference` b

-- | Removes burned values for multiple assets from a list of change maps.
--
-- Plural of @removeBurnValueFromChangeMaps@.
--
removeBurnValuesFromChangeMaps
    :: TokenMap
    -- ^ Map of burned values
    -> NonEmpty TokenMap
    -- ^ Change maps
    -> NonEmpty TokenMap
    -- ^ Change maps with burned values removed
removeBurnValuesFromChangeMaps =
    flip (F.foldr removeBurnValueFromChangeMaps) . TokenMap.toFlatList

--------------------------------------------------------------------------------
-- Splitting bundles
--------------------------------------------------------------------------------

-- | Splits a bundle into smaller bundles if its asset count is excessive when
--   measured with the given 'isExcessive' indicator function.
--
-- Returns a list of smaller bundles for which 'isExcessive' returns 'False'.
--
splitBundleIfAssetCountExcessive
    :: TokenBundle
    -- ^ The token bundle suspected to have an excessive number of assets.
    -> (TokenBundle -> Bool)
    -- ^ A function that returns 'True' if (and only if) the asset count of
    -- the given bundle is excessive.
    -> NonEmpty TokenBundle
splitBundleIfAssetCountExcessive b isExcessive
    | isExcessive b =
        splitInHalf b >>= flip splitBundleIfAssetCountExcessive isExcessive
    | otherwise =
        pure b
  where
    splitInHalf = flip TokenBundle.equipartitionAssets (() :| [()])

-- | Splits bundles with excessive asset counts into smaller bundles.
--
-- Only token bundles where the 'isExcessive' indicator function returns 'True'
-- will be split.
--
-- Returns a list of smaller bundles for which 'isExcessive' returns 'False'.
--
-- If none of the bundles in the given list has an excessive asset count,
-- this function will return the original list.
--
splitBundlesWithExcessiveAssetCounts
    :: NonEmpty TokenBundle
    -- ^ Token bundles.
    -> (TokenBundle -> Bool)
    -- ^ A function that returns 'True' if (and only if) the asset count of
    -- the given bundle is excessive.
    -> NonEmpty TokenBundle
splitBundlesWithExcessiveAssetCounts bs isExcessive =
    (`splitBundleIfAssetCountExcessive` isExcessive) =<< bs

-- | Splits bundles with excessive token quantities into smaller bundles.
--
-- Only token bundles containing quantities that exceed the maximum token
-- quantity will be split.
--
-- If none of the bundles in the given list contain a quantity that exceeds
-- the maximum token quantity, this function will return the original list.
--
splitBundlesWithExcessiveTokenQuantities
    :: NonEmpty TokenBundle
    -- ^ Token bundles.
    -> TokenQuantity
    -- ^ Maximum allowable token quantity.
    -> NonEmpty TokenBundle
    -- ^ The partitioned bundles.
splitBundlesWithExcessiveTokenQuantities bs maxQuantity =
    (`TokenBundle.equipartitionQuantitiesWithUpperBound` maxQuantity) =<< bs

--------------------------------------------------------------------------------
-- Grouping and ungrouping
--------------------------------------------------------------------------------

groupByKey :: forall k v. Ord k => [(k, v)] -> Map k (NonEmpty v)
groupByKey = F.foldl' acc mempty
  where
    acc :: Map k (NonEmpty v) -> (k, v) -> Map k (NonEmpty v)
    acc m (k, v) = Map.alter (Just . maybe (v :| []) (NE.cons v)) k m

ungroupByKey :: forall k v. Map k (NonEmpty v) -> [(k, v)]
ungroupByKey m = [(k, v) | (k, vs) <- Map.toList m, v <- NE.toList vs]

--------------------------------------------------------------------------------
-- Round-robin processing
--------------------------------------------------------------------------------

runRoundRobin :: s -> (s' -> s) -> [(s -> Maybe s')] -> s
runRoundRobin state demote processors =
    runIdentity $ runRoundRobinM state demote $ fmap Identity <$> processors

runRoundRobinM :: Monad m => s -> (s' -> s) -> [(s -> m (Maybe s'))] -> m s
runRoundRobinM state demote processors = go state processors []
  where
    go !s []        [] = pure s
    go !s []       !qs = go s (L.reverse qs) []
    go !s (p : ps) !qs = p s >>=
        \case
            Nothing -> go         s   ps      qs
            Just s' -> go (demote s') ps (p : qs)

--------------------------------------------------------------------------------
-- Accessor functions
--------------------------------------------------------------------------------

selectedAssetQuantity :: IsUTxOSelection s u => AssetId -> s u -> Natural
selectedAssetQuantity asset
    = unTokenQuantity
    . flip TokenBundle.getQuantity asset
    . UTxOSelection.selectedBalance

selectedCoinQuantity :: IsUTxOSelection s u => s u -> Natural
selectedCoinQuantity
    = intCast
    . unCoin
    . TokenBundle.getCoin
    . UTxOSelection.selectedBalance

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

-- | A total ordering on token maps based on the number of assets in each map.
--
-- If two maps have the same number of assets, then we fall back to ordinary
-- lexicographic ordering as a tie-breaker.
--
instance Ord (AssetCount TokenMap) where
    compare (AssetCount m1) (AssetCount m2) =
        case compare (assetCount m1) (assetCount m2) of
            LT -> LT
            GT -> GT
            EQ -> comparing TokenMap.toNestedList m1 m2
      where
        assetCount = TokenMap.size

newtype AssetCount a = AssetCount
    { unAssetCount :: a }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

distance :: Natural -> Natural -> Natural
distance a b
    | a > b = a - b
    | a < b = b - a
    | otherwise = 0

mapMaybe :: (a -> Maybe b) -> NonEmpty a -> [b]
mapMaybe predicate (x :| xs) = go (x:xs)
  where
    go   []   = []
    go (a:as) =
        case predicate a of
            Just b  -> b : go as
            Nothing -> go as
