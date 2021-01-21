{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Provides an implementation of the Random-Round-Robin coin selection
-- algorithm for multi-asset UTxO sets.
--
-- See documentation for the 'performSelection' function for more details on
-- how to perform a selection.
--
module Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    (
    -- * Performing a selection
      performSelection
    , prepareOutputsWith
    , SelectionCriteria (..)
    , SelectionLimit (..)
    , SelectionSkeleton (..)
    , SelectionResult (..)
    , SelectionError (..)
    , BalanceInsufficientError (..)
    , SelectionInsufficientError (..)
    , InsufficientMinCoinValueError (..)
    , UnableToConstructChangeError (..)

    -- * Running a selection (without making change)
    , runSelection
    , SelectionState (..)

    -- * Running a selection step
    , runSelectionStep
    , SelectionLens (..)

    -- * Making change
    , makeChange
    , makeChangeForCoin
    , makeChangeForKnownAsset
    , makeChangeForUnknownAsset

    -- * Grouping and ungrouping
    , groupByKey
    , ungroupByKey

    -- * Round-robin processing
    , runRoundRobin
    , runRoundRobinM

    -- * Accessors
    , fullBalance

    -- * Utility functions
    , distance
    , mapMaybe
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( padCoalesce, partitionNatural )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( SelectionFilter (..), UTxOIndex (..) )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Trans.State
    ( StateT (..) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

-- | Criteria for performing a selection.
--
data SelectionCriteria = SelectionCriteria
    { outputsToCover
        :: !(NonEmpty TxOut)
        -- ^ The complete set of outputs to be covered.
    , utxoAvailable
        :: !UTxOIndex
        -- ^ A UTxO set from which inputs can be selected.
    , selectionLimit
        :: !SelectionLimit
        -- ^ Specifies a limit to be adhered to when performing selection.
    , extraCoinSource
        :: !(Maybe Coin)
        -- ^ An optional extra source of ada.
    }
    deriving (Eq, Show)

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
data SelectionSkeleton = SelectionSkeleton
    { inputsSkeleton
        :: !UTxOIndex
    , outputsSkeleton
        :: !(NonEmpty TxOut)
    , changeSkeleton
        :: !(NonEmpty (Set AssetId))
    }
    deriving (Eq, Show)

-- | Specifies a limit to adhere to when performing a selection.
--
data SelectionLimit
    = NoLimit
      -- ^ Indicates that there is no limit.
    | MaximumInputLimit Int
      -- ^ Indicates a maximum limit on the number of inputs to select.
    deriving (Eq, Show)

-- | The result of performing a successful selection.
--
data SelectionResult = SelectionResult
    { inputsSelected
        :: !(NonEmpty (TxIn, TxOut))
        -- ^ A (non-empty) list of inputs selected from 'utxoAvailable'.
    , changeGenerated
        :: !(NonEmpty TokenBundle)
        -- ^ A (non-empty) list of generated change outputs.
    , utxoRemaining
        :: !UTxOIndex
        -- ^ The subset of 'utxoAvailable' that remains after performing
        -- the selection.
    }
    deriving (Eq, Show)

-- | Represents the set of errors that may occur while performing a selection.
--
data SelectionError
    = BalanceInsufficient
        BalanceInsufficientError
    | SelectionInsufficient
        SelectionInsufficientError
    | InsufficientMinCoinValues
        (NonEmpty InsufficientMinCoinValueError)
    | UnableToConstructChange
        UnableToConstructChangeError
    deriving (Generic, Eq, Show)

-- | Indicates that the balance of inputs actually selected was insufficient to
--   cover the balance of 'outputsToCover'.
--
data SelectionInsufficientError = SelectionInsufficientError
    { balanceRequired
        :: !TokenBundle
      -- ^ The balance of 'outputsToCover'.
    , inputsSelected
        :: ![(TxIn, TxOut)]
      -- ^ The inputs that could be selected while satisfying the
      -- 'selectionLimit'.
    } deriving (Generic, Eq, Show)

-- | Indicates that the balance of 'utxoAvailable' is insufficient to cover the
--   balance of 'outputsToCover'.
--
data BalanceInsufficientError = BalanceInsufficientError
    { balanceAvailable
        :: !TokenBundle
      -- ^ The balance of 'utxoAvailable'.
    , balanceRequired
        :: !TokenBundle
      -- ^ The balance of 'outputsToCover'.
    } deriving (Generic, Eq, Show)

-- | Indicates that a particular output does not have the minimum coin quantity
--   expected by the protocol.
--
-- See also: 'prepareOutputs'.
--
data InsufficientMinCoinValueError = InsufficientMinCoinValueError
    { outputWithInsufficientAda
        :: !TxOut
        -- ^ The particular output that does not have the minimum coin quantity
        -- expected by the protocol.
    , expectedMinCoinValue
        :: !Coin
        -- ^ The minimum coin quantity expected for this output.
    } deriving (Generic, Eq, Show)

newtype UnableToConstructChangeError = UnableToConstructChangeError
    { missingCoins
        :: Coin
        -- ^ The additional coin quantity that would be required to cover the
        -- selection cost and minimum coin quantity of each change output.
    } deriving (Generic, Eq, Show)

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
    :: (TokenMap -> Coin)
    -> NonEmpty TxOut
    -> NonEmpty TxOut
prepareOutputsWith minCoinValueFor = fmap $ \out ->
    out { Tx.tokens = augmentBundle (Tx.tokens out) }
  where
    augmentBundle bundle =
        if TokenBundle.getCoin bundle == Coin 0
        then bundle { coin = minCoinValueFor (view #tokens bundle) }
        else bundle
--
-- | Performs a coin selection and generates change bundles in one step.
--
-- Returns 'BalanceInsufficient' if the total balance of 'utxoAvailable' is not
-- strictly greater than or equal to the total balance of 'outputsToCover'.
--
-- Provided that the total balance of 'utxoAvailable' is sufficient to cover
-- the total balance of 'outputsToCover', this function guarantees to return
-- an 'inputsSelected' value that satisfies:
--
--    balance inputsSelected >= balance outputsToCover
--    balance inputsSelected == balance outputsToCover + balance changeGenerated
--
-- Finally, this function guarantees that:
--
--    inputsSelected ∪ utxoRemaining == utxoAvailable
--    inputsSelected ∩ utxoRemaining == ∅
--
performSelection
    :: forall m. (HasCallStack, MonadRandom m)
    => (TokenMap -> Coin)
        -- ^ A function that computes the minimum ada quantity required by the
        -- protocol for a particular output.
    -> (SelectionSkeleton -> Coin)
        -- ^ A function that computes the extra cost corresponding to a given
        -- selection. This function must not depend on the magnitudes of
        -- individual asset quantities held within each change output.
    -> SelectionCriteria
        -- ^ The selection goal to satisfy.
    -> m (Either SelectionError SelectionResult)
performSelection minCoinValueFor costFor criteria
    | not (balanceRequired `leq` balanceAvailable) =
        pure $ Left $ BalanceInsufficient $ BalanceInsufficientError
            { balanceAvailable, balanceRequired }

    | not (null insufficientMinCoinValues) =
        pure $ Left $ InsufficientMinCoinValues $
            NE.fromList insufficientMinCoinValues

    | otherwise = do
        state <- runSelection selectionLimit extraCoinSource utxoAvailable balanceRequired
        let balanceSelected = fullBalance (selected state) extraCoinSource
        if balanceRequired `leq` balanceSelected then do
            let predictedChange = predictChange (selected state)
            makeChangeRepeatedly predictedChange state

        else
            pure $ Left $ SelectionInsufficient $ SelectionInsufficientError
                { inputsSelected = UTxOIndex.toList (selected state)
                , balanceRequired
                }
  where
    SelectionCriteria
        { outputsToCover
        , utxoAvailable
        , selectionLimit
        , extraCoinSource
        } = criteria

    mkInputsSelected :: UTxOIndex -> NonEmpty (TxIn, TxOut)
    mkInputsSelected =
        fromMaybe invariantSelectAnyInputs . NE.nonEmpty . UTxOIndex.toList

    balanceAvailable :: TokenBundle
    balanceAvailable = fullBalance utxoAvailable extraCoinSource

    balanceRequired :: TokenBundle
    balanceRequired = F.foldMap (view #tokens) outputsToCover

    insufficientMinCoinValues :: [InsufficientMinCoinValueError]
    insufficientMinCoinValues =
        mapMaybe mkInsufficientMinCoinValueError outputsToCover
      where
        mkInsufficientMinCoinValueError
            :: TxOut
            -> Maybe InsufficientMinCoinValueError
        mkInsufficientMinCoinValueError o
            | view (#tokens . #coin) o >= expectedMinCoinValue =
                Nothing
            | otherwise =
                Just $ InsufficientMinCoinValueError
                    { expectedMinCoinValue, outputWithInsufficientAda = o }
          where
            expectedMinCoinValue = minCoinValueFor (view (#tokens . #tokens) o)

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
    --     ∃ criteria. / isRight (performSelection criteria) =>
    --         Right predictedChange === assets <$> performSelection criteria
    --
    --     (That is, the predicted change is necessarily equal to the change
    --     assets of the final resulting selection).
    --
    predictChange
        :: UTxOIndex
        -> NonEmpty (Set AssetId)
    predictChange inputsPreSelected = either
        (const $ invariantResultWithNoCost inputsPreSelected)
        (fmap (TokenMap.getAssets . view #tokens))
        (makeChange noMinimumCoin noCost
            extraCoinSource
            (view #tokens . snd <$> mkInputsSelected inputsPreSelected)
            (view #tokens <$> outputsToCover)
        )
      where
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
    makeChangeRepeatedly
        :: NonEmpty (Set AssetId)
        -> SelectionState
        -> m (Either SelectionError SelectionResult)
    makeChangeRepeatedly changeSkeleton s@SelectionState{selected,leftover} = do
        let inputsSelected = mkInputsSelected selected

        let cost = costFor SelectionSkeleton
                { inputsSkeleton  = selected
                , outputsSkeleton = outputsToCover
                , changeSkeleton
                }

        let mChangeGenerated
                :: Either UnableToConstructChangeError (NonEmpty TokenBundle)
            mChangeGenerated = makeChange minCoinValueFor cost
                extraCoinSource
                (view #tokens . snd <$> inputsSelected)
                (view #tokens <$> outputsToCover)

        case mChangeGenerated of
            Right changeGenerated -> pure . Right $
                SelectionResult
                    { inputsSelected
                    , changeGenerated
                    , utxoRemaining = leftover
                    }

            Left changeErr ->
                let
                    selectionErr = Left $ UnableToConstructChange changeErr
                in
                    selectMatchingQuantity selectionLimit [WithAdaOnly] s
                    >>=
                    maybe (pure selectionErr) (makeChangeRepeatedly changeSkeleton)

    invariantSelectAnyInputs =
        -- This should be impossible, as we have already determined
        -- that the UTxO balance is sufficient to cover the outputs.
        error $ unlines
            [ "performSelection: unable to select any inputs!"
            , "balance required:"
            , show balanceRequired
            , "balance available:"
            , show balanceAvailable
            ]

    invariantResultWithNoCost inputs_ = error $ unlines
        -- This should be impossible, as the 'makeChange' function should
        -- always succeed if there's no extra cost or minimum value to assign.
        -- This is because it is called with the result of 'runSelection',
        -- which only terminates successfully if the target was satisfied.
        [ "performSelection: couldn't construct change for a selection with no "
        , "minimum coin value and no cost!"
        , "inputs: " <> show inputs_
        , "extra input source: " <> show extraCoinSource
        , "outputs: " <> show outputsToCover
        ]

--------------------------------------------------------------------------------
-- Running a selection (without making change)
--------------------------------------------------------------------------------

data SelectionState = SelectionState
    { selected
        :: !UTxOIndex
    , leftover
        :: !UTxOIndex
    }
    deriving (Eq, Show)

runSelection
    :: forall m. MonadRandom m
    => SelectionLimit
        -- ^ A limit to adhere to when performing a selection.
    -> Maybe Coin
        -- ^ An extra source of ada, which can only be used after at least one
        -- input has been selected.
    -> UTxOIndex
        -- ^ UTxO entries available for selection.
    -> TokenBundle
        -- ^ Minimum balance to cover.
    -> m SelectionState
        -- ^ Final selection state.
runSelection limit mExtraCoinSource available minimumBalance =
    runRoundRobinM initialState selectors
  where
    initialState :: SelectionState
    initialState = SelectionState
        { selected = UTxOIndex.empty
        , leftover = available
        }

    selectors :: [SelectionState -> m (Maybe SelectionState)]
    selectors = coinSelector : fmap assetSelector minimumAssetQuantities
      where
        assetSelector = runSelectionStep . assetSelectionLens
        coinSelector = runSelectionStep coinSelectionLens

    (minimumCoinQuantity, minimumAssetQuantities) =
        TokenBundle.toFlatList minimumBalance

    assetSelectionLens
        :: (AssetId, TokenQuantity) -> SelectionLens m SelectionState
    assetSelectionLens (asset, minimumAssetQuantity) = SelectionLens
        { currentQuantity = assetQuantity asset . selected
        , minimumQuantity = unTokenQuantity minimumAssetQuantity
        , selectQuantity = selectMatchingQuantity limit
            [ WithAsset asset
            ]
        }

    coinSelectionLens :: SelectionLens m SelectionState
    coinSelectionLens = SelectionLens
        { currentQuantity = \s -> coinQuantity (selected s) mExtraCoinSource
        , minimumQuantity = fromIntegral $ unCoin minimumCoinQuantity
        , selectQuantity  = selectMatchingQuantity limit
            [ WithAdaOnly
            , Any
            ]
        }

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
    :: MonadRandom m
    => SelectionLimit
        -- ^ A limit to adhere to when selecting entries.
    -> [SelectionFilter]
        -- ^ A list of selection filters to be traversed from left-to-right,
        -- in descending order of priority.
    -> SelectionState
        -- ^ The current selection state.
    -> m (Maybe SelectionState)
        -- ^ An updated selection state that includes a matching UTxO entry,
        -- or 'Nothing' if no such entry could be found.
selectMatchingQuantity _       []  _ = pure Nothing
selectMatchingQuantity limit (h:q) s
    | limitReached =
        pure Nothing
    | otherwise = do
        UTxOIndex.selectRandom (leftover s) h >>= \case
            Just s' -> pure $ Just $ updateState s'
            Nothing -> selectMatchingQuantity limit q s
  where
    limitReached = case limit of
        MaximumInputLimit m -> UTxOIndex.size (selected s) >= m
        NoLimit -> False

    updateState ((i, o), remaining) = SelectionState
        { leftover = remaining
        , selected = UTxOIndex.insert i o (selected s)
        }

--------------------------------------------------------------------------------
-- Running a selection step
--------------------------------------------------------------------------------

-- | Provides a lens on the current selection state.
--
-- A 'SelectionLens' gives 'runSelectionStep' just the information it needs to
-- make a decision, and no more.
--
data SelectionLens m state = SelectionLens
    { currentQuantity
        :: state -> Natural
    , selectQuantity
        :: state -> m (Maybe state)
    , minimumQuantity
        :: Natural
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
--      if it takens the total selected token quantity closer to 200% of the
--      output token quantity, but not further away.
--
runSelectionStep
    :: forall m state. Monad m
    => SelectionLens m state
    -> state
    -> m (Maybe state)
runSelectionStep lens s
    | currentQuantity s < minimumQuantity =
        selectQuantity s
    | otherwise =
        (requireImprovement =<<) <$> selectQuantity s
  where
    SelectionLens {currentQuantity, selectQuantity, minimumQuantity} = lens

    requireImprovement :: state -> Maybe state
    requireImprovement s'
        | distanceFromTarget s' < distanceFromTarget s = Just s'
        | otherwise = Nothing

    distanceFromTarget :: state -> Natural
    distanceFromTarget = distance targetQuantity . currentQuantity

    targetQuantity :: Natural
    targetQuantity = minimumQuantity * 2

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

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
    :: (TokenMap -> Coin)
        -- A function that computes the minimum required ada quantity for a
        -- particular output.
    -> Coin
        -- ^ The minimal (and optimal) delta between the total ada balance
        -- of all input bundles and the total ada balance of all output and
        -- change bundles, where:
        --
        --    delta = getCoin (fold inputBundles)
        --          - getCoin (fold outputBundles)
        --          - getCoin (fold changeBundles)
        --
        -- This typically captures fees plus key deposits.
        --
    -> Maybe Coin
        -- ^ An optional extra source of ada.
    -> NonEmpty TokenBundle
        -- ^ Token bundles of selected inputs.
    -> NonEmpty TokenBundle
        -- ^ Token bundles of original outputs.
    -> Either UnableToConstructChangeError (NonEmpty TokenBundle)
        -- ^ Generated change bundles.
makeChange minCoinValueFor requiredCost mExtraCoinSource inputBundles outputBundles
    | not (totalOutputValue `leq` totalInputValue) =
        totalInputValueInsufficient
    | TokenBundle.getCoin totalOutputValue == Coin 0 =
        totalOutputCoinValueIsZero
    | otherwise = do
            -- The following subtraction is safe, as we have already checked
            -- that the total input value is greater than the total output
            -- value:
        let excess :: TokenBundle
            excess = totalInputValue `TokenBundle.unsafeSubtract` totalOutputValue

        let (excessCoin, excessAssets) = TokenBundle.toFlatList excess

        let unknownAssets =
                Map.toList $ F.foldr discardKnownAssets mempty inputBundles

        let changeForKnownAssets :: NonEmpty TokenMap
            changeForKnownAssets = F.foldr
                (NE.zipWith (<>) . makeChangeForKnownAsset outputTokens)
                (TokenMap.empty <$ outputTokens)
                excessAssets

        let changeForUnknownAssets :: NonEmpty TokenMap
            changeForUnknownAssets = F.foldr
                (NE.zipWith (<>) . makeChangeForUnknownAsset outputTokens)
                (TokenMap.empty <$ outputTokens)
                unknownAssets

        let change :: NonEmpty TokenMap
            change = NE.zipWith (<>) changeForKnownAssets changeForUnknownAssets

        (bundles, remainder) <- maybe (Left $ changeError excessCoin change) Right $
            excessCoin `subtractCoin` requiredCost
            >>=
            runStateT (sequence (StateT . assignCoin minCoinValueFor <$> change))

        let changeForCoins :: NonEmpty TokenBundle
            changeForCoins = TokenBundle.fromCoin
                <$> makeChangeForCoin outputCoins remainder

        pure (NE.zipWith (<>) bundles changeForCoins)
  where
    totalInputValueInsufficient = error
        "makeChange: not (totalOutputValue <= totalInputValue)"
    totalOutputCoinValueIsZero = error
        "makeChange: not (totalOutputCoinValue > 0)"

    changeError
        :: Coin
        -> NonEmpty TokenMap
        -> UnableToConstructChangeError
    changeError excessCoin change =
        UnableToConstructChangeError
            { missingCoins =
                -- This conversion is safe because we know that the distance is
                -- small-ish. If it wasn't, we would have have enough coins to
                -- construct the change.
                unsafeNaturalToCoin $ distance
                    (coinToNatural excessCoin)
                    (coinToNatural requiredCost + totalMinCoinValue)
            }
      where
        totalMinCoinValue =
            F.sum $ (coinToNatural . minCoinValueFor) <$> change

    outputTokens :: NonEmpty TokenMap
    outputTokens = view #tokens <$> outputBundles

    outputCoins :: NonEmpty Coin
    outputCoins = view #coin <$> outputBundles

    totalInputValue :: TokenBundle
    totalInputValue = TokenBundle.add
        (F.fold inputBundles)
        (maybe TokenBundle.empty TokenBundle.fromCoin mExtraCoinSource)

    totalOutputValue :: TokenBundle
    totalOutputValue = F.fold outputBundles

    -- Identifiers of assets included in outputs.
    knownAssetIds :: Set AssetId
    knownAssetIds = TokenBundle.getAssets totalOutputValue

    discardKnownAssets
        :: TokenBundle
        -> Map AssetId (NonEmpty TokenQuantity)
        -> Map AssetId (NonEmpty TokenQuantity)
    discardKnownAssets (TokenBundle _ tokens) m =
        foldr (\(k, v) -> Map.insertWith (<>) k (v :| [])) m filtered
      where
        filtered = filter
            ((`Set.notMember` knownAssetIds) . fst)
            (TokenMap.toFlatList tokens)

-- | Constructs change outputs for an asset that was present in the original
--   set of outputs, based on the given weight distribution.
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
makeChangeForKnownAsset
    :: NonEmpty TokenMap
        -- ^ A list of weights for the distribution. Conveniently captures both
        -- the weights, and the number of elements amongst which the quantity
        -- should be distributed.
    -> (AssetId, TokenQuantity)
        -- ^ A surplus token quantity to distribute.
    -> NonEmpty TokenMap
makeChangeForKnownAsset targets (asset, TokenQuantity excess) =
    let
        partition = fromMaybe zeros (partitionNatural excess weights)
    in
        TokenMap.singleton asset . TokenQuantity <$> partition
  where
    weights :: NonEmpty Natural
    weights = byAsset asset <$> targets
      where
        byAsset :: AssetId -> TokenMap -> Natural
        byAsset x = unTokenQuantity . flip TokenMap.getQuantity x

    zeros :: NonEmpty Natural
    zeros = 0 :| replicate (length targets - 1) 0

-- | Constructs change outputs for an asset that was not present in the original
--   set of outputs.
--
-- This function constructs a list of change outputs by preserving the input
-- distribution as much as possible. Note that only the length of the first
-- argument is used.
--
-- The length of the output list is always the same as the length of the input
-- list, and the sum of its quantities is always exactly equal to the sum of
-- all token quantities given in the second argument.
--
makeChangeForUnknownAsset
    :: NonEmpty TokenMap
        -- ^ A list of weights for the distribution. The list is only used for
        -- its number of elements.
    -> (AssetId, NonEmpty TokenQuantity)
        -- ^ An asset quantity to distribute.
    -> NonEmpty TokenMap
makeChangeForUnknownAsset n (asset, quantities) =
    TokenMap.singleton asset <$> padCoalesce quantities n

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
makeChangeForCoin targets excess =
    -- The 'Natural -> Coin' conversion is safe, because 'partitionNatural'
    -- guarantees to produce a list where every entry is less than or equal to
    -- the target value.
    maybe zeroWeightSum (fmap unsafeNaturalToCoin)
        (partitionNatural (coinToNatural excess) weights)
  where
    zeroWeightSum :: HasCallStack => a
    zeroWeightSum = error
        "partitionValue: The specified weights must have a non-zero sum."

    weights :: NonEmpty Natural
    weights = coinToNatural <$> targets

-- | Creates a 'TokenBundle' from a 'TokenMap' by assigning it the minimum
--   ada quantity required by the protocol, using the given 'Coin' as a source
--   of ada.
--
-- Returns both the constructed 'TokenBundle' and the leftover 'Coin' quantity.
--
-- Returns 'Nothing' if the given source of ada is not enough to safisfy the
-- minimum quantity required by the protocol.
--
assignCoin
    :: (TokenMap -> Coin)
    -> TokenMap
    -> Coin
    -> Maybe (TokenBundle, Coin)
assignCoin minCoinValueFor tokens availableCoins@(Coin c)
    | availableCoins < minCoin =
        Nothing
    | otherwise =
        Just (TokenBundle minCoin tokens, Coin (c - m))
  where
    minCoin@(Coin m) = minCoinValueFor tokens

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

runRoundRobin :: s -> [(s -> Maybe s)] -> s
runRoundRobin state processors =
    runIdentity $ runRoundRobinM state $ fmap Identity <$> processors

runRoundRobinM :: Monad m => s -> [(s -> m (Maybe s))] -> m s
runRoundRobinM state processors = go state processors []
  where
    go !s []        [] = pure s
    go !s []       !qs = go s (L.reverse qs) []
    go !s (p : ps) !qs = p s >>=
        \case
            Nothing -> go s  ps      qs
            Just s' -> go s' ps (p : qs)

--------------------------------------------------------------------------------
-- Accessor functions
--------------------------------------------------------------------------------

assetQuantity :: AssetId -> UTxOIndex -> Natural
assetQuantity asset =
    unTokenQuantity . flip TokenBundle.getQuantity asset . view #balance

coinQuantity :: UTxOIndex -> Maybe Coin -> Natural
coinQuantity index =
    fromIntegral . unCoin . TokenBundle.getCoin . fullBalance index

fullBalance :: UTxOIndex -> Maybe Coin -> TokenBundle
fullBalance index extraSource
    | UTxOIndex.null index =
        TokenBundle.empty
    | otherwise =
        TokenBundle.add
            (view #balance index)
            (maybe TokenBundle.empty TokenBundle.fromCoin extraSource)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

coinToNatural :: Coin -> Natural
coinToNatural = fromIntegral . unCoin

unsafeNaturalToCoin :: Natural -> Coin
unsafeNaturalToCoin = Coin . fromIntegral

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
