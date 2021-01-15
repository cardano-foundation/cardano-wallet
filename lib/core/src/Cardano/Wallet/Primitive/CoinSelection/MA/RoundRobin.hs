{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , SelectionCriteria (..)
    , SelectionLimit (..)
    , SelectionResult (..)
    , SelectionError (..)
    , BalanceInsufficientError (..)
    , SelectionInsufficientError (..)

    -- * Running a selection (without making change)
    , runSelection
    , SelectionState (..)

    -- * Running a selection step
    , runSelectionStep
    , SelectionLens (..)

    -- * Making change
    , makeChange
    , makeChangeForCoins
    , makeChangeForKnownAsset
    , makeChangeForUnknownAsset

    -- * Grouping and ungrouping
    , groupByKey
    , ungroupByKey

    -- * Round-robin processing
    , runRoundRobin
    , runRoundRobinM

    -- * Utility functions
    , distance
    , subtractCoin
    , addCoin
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( padCoalesce, partitionNatural )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
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
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
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
        :: NonEmpty TxOut
    , utxoAvailable
        :: !UTxOIndex
    , selectionLimit
        :: !SelectionLimit
    }
    deriving (Eq, Show)

-- | Specifies a limit to adhere to when performing a selection.
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
    , changeGenerated
        :: !(NonEmpty TokenBundle)
    , utxoRemaining
        :: !UTxOIndex
    }
    deriving (Eq, Show)

-- | Represents the set of errors that may occur while performing a selection.
--
data SelectionError
    = BalanceInsufficient
        BalanceInsufficientError
    | SelectionInsufficient
        SelectionInsufficientError

-- | Indicates that the balance of 'utxoAvailable' is insufficient to cover the
--   balance of 'outputsToCover'.
--
data BalanceInsufficientError = BalanceInsufficientError
    { balanceAvailable
        :: TokenBundle
      -- ^ The balance of 'utxoAvailable'.
    , balanceRequired
        :: TokenBundle
      -- ^ The balance of 'outputsToCover'.
    }

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
    }

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
    => SelectionCriteria
    -> m (Either SelectionError SelectionResult)
performSelection criteria
    | not (balanceRequired `leq` balanceAvailable) =
        pure $ Left $ BalanceInsufficient $ BalanceInsufficientError
            {balanceAvailable, balanceRequired}
    | otherwise =
        mkResult <$>
            runSelection selectionLimit utxoAvailable balanceRequired
  where
    SelectionCriteria
        {outputsToCover, utxoAvailable, selectionLimit} = criteria

    balanceAvailable :: TokenBundle
    balanceAvailable = view #balance utxoAvailable

    balanceRequired :: TokenBundle
    balanceRequired = F.foldMap (view #tokens) outputsToCover

    mkResult :: SelectionState -> Either SelectionError SelectionResult
    mkResult SelectionState {selected, leftover}
        | not (balanceRequired `leq` balanceSelected) =
            Left $ SelectionInsufficient $ SelectionInsufficientError
                {inputsSelected = UTxOIndex.toList selected, balanceRequired}
        | otherwise =
            case NE.nonEmpty (UTxOIndex.toList selected) of
                Nothing ->
                    unableToSelectAnyInputsError
                Just inputsSelected -> Right SelectionResult
                    { inputsSelected
                    , utxoRemaining = leftover
                    , changeGenerated = makeChange
                        (view #tokens . snd <$> inputsSelected)
                        (view #tokens <$> outputsToCover)
                    }
      where
        balanceSelected = UTxOIndex.balance selected

    unableToSelectAnyInputsError =
        -- This should be impossible, as we have already determined
        -- that the UTxO balance is sufficient to cover the outputs.
        error $ unlines
            [ "performSelection: unable to select any inputs!"
            , "balance required:"
            , show balanceRequired
            , "balance available:"
            , show balanceAvailable
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
        -- ^ An extra source of Ada, which can only be used after at least one
        -- input has been selected.
    -> UTxOIndex
        -- ^ UTxO entries available for selection
    -> TokenBundle
        -- ^ Minimum balance to cover
    -> m SelectionState
        -- ^ Final selection state
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
        { currentQuantity = \s ->
            coinQuantity (selected s)
            +
            if UTxOIndex.null (selected s) then 0 else extraCoinSource
        , minimumQuantity = fromIntegral $ unCoin minimumCoinQuantity
        , selectQuantity = selectMatchingQuantity limit
            [ WithAdaOnly
            , Any
            ]
        }
      where
        extraCoinSource :: Natural
        extraCoinSource = maybe 0 (fromIntegral . unCoin) mExtraCoinSource

selectMatchingQuantity
    :: MonadRandom m
    => SelectionLimit
    -> [SelectionFilter]
        -- A list of selection filters, traversed from left to right if previous
        -- filter failed. This allows for giving some filters priorities over
        -- others.
    -> SelectionState
    -> m (Maybe SelectionState)
selectMatchingQuantity _       []  _ = pure Nothing
selectMatchingQuantity limit (h:q) s =
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

data SelectionLens m state = SelectionLens
    { currentQuantity
        :: state -> Natural
    , selectQuantity
        :: state -> m (Maybe state)
    , minimumQuantity
        :: Natural
    }

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

-- | Calculate change bundles from a set of selected inputs and outputs. Returns
-- 'Nothing' if there are not enough 'Ada' inputs to satisfy minimum delta and
-- minimum values in each token bundle. However, generate runtime errors if:
--
-- 1. The total input value is lesser than the total output value
-- 2. The total output value is null
--
-- The pre-condition (1) should be satisfied by any result coming from
-- `runSelection`. The pre-condition (2) is a undirect consequence of assigning
-- a minimum UTxO value to every output token bundle.
makeChange
    :: (TokenMap -> Coin)
        -- A function which computes the minimum required Ada coins for a
        -- particular output.
    -> Coin
        -- ^ The minimal (and optimal) delta between the total Ada inputs and
        -- total Ada outputs. This typically captures fees plus key deposits.
    -> Maybe Coin
        -- ^ An extra source of Ada, if any.
    -> NonEmpty TokenBundle
        -- ^ Token bundles of selected inputs
    -> NonEmpty TokenBundle
        -- ^ Token bundles of original outputs
    -> Maybe (NonEmpty TokenBundle)
        -- ^ Change bundles.
makeChange minCoinValueFor requiredCost mExtraCoinSource inputBundles outputBundles
    | not (totalOutputValue `leq` totalInputValue) =
        totalInputValueInsufficient
    | TokenBundle.getCoin totalOutputValue == Coin 0 =
        totalOutputCoinValueIsZero
    | otherwise = do
            -- The following subtraction is safe, as we have already checked
            -- that the total input value is greater than the total output value
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

        (bundles, remainder) <-
            excessCoin `subtractCoin` requiredCost
            >>=
            runStateT (sequence (StateT . assignCoin minCoinValueFor <$> change))

        let changeForCoins :: NonEmpty TokenBundle
            changeForCoins = TokenBundle.fromCoin
                <$> makeChangeForCoins outputCoins remainder

        pure (NE.zipWith (<>) bundles changeForCoins)
  where
    totalInputValueInsufficient = error
        "makeChange: not (totalOutputValue <= totalInputValue)"
    totalOutputCoinValueIsZero = error
        "makeChange: not (totalOutputCoinValue > 0)"

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

-- | Construct change outputs for known assets based on a distribution given as
-- input. If the provided 'AssetId' figures nowhere in the given distribution,
-- then a list of empty token maps is returned. Otherwise, the given
-- 'TokenQuantity' is distributed in a list proportionally to the input
-- distribution.
--
-- The output list has always the same size as the input list, and the sum of
-- its values is either zero, or exactly the 'TokenQuantity' given as 2nd
-- argument.
makeChangeForKnownAsset
    :: NonEmpty TokenMap
        -- ^ A list of weights for the distribution. Conveniently captures both
        -- the weights, and the number of elements amongst which the surplus
        -- should be distributed.
    -> (AssetId, TokenQuantity)
        -- ^ A surplus token to distribute
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

-- | Construct a list of change outputs based by preserving as much as
-- possible the input distribution. Note that only the length of the first
-- argument is used.
--
-- The output list has always the same size as the input list, and the sum of
-- its values is always exactly the sum of all 'TokenQuantity' given as 2nd
-- argument.
makeChangeForUnknownAsset
    :: NonEmpty TokenMap
        -- ^ A list of weights for the distribution. The list is only used for
        -- its number of elements.
    -> (AssetId, NonEmpty TokenQuantity)
        -- ^ An asset to distribute
    -> NonEmpty TokenMap
makeChangeForUnknownAsset n (asset, quantities) =
    TokenMap.singleton asset <$> padCoalesce quantities n

-- | Construct a list of coin change outputs based on a distribution given as
-- first input. If the input distribution is filled with 0, this function throws
-- a runtime error.
--
-- The output list has always the same size as the input list, and the sum of
-- its values is always exactly equal to the 'Coin' value given as 2nd argument.
makeChangeForCoins
    :: HasCallStack
    => NonEmpty Coin
        -- ^ A list of weights for the distribution. Conveniently captures both
        -- the weights, and the number of elements amongst which the surplus
        -- should be distributed.
    -> Coin
        -- ^ A surplus Ada value which needs to be distributed
    -> NonEmpty Coin
makeChangeForCoins targets excess =
    maybe zeroWeightSum (fmap naturalToCoin)
        (partitionNatural (coinToNatural excess) weights)
  where
    zeroWeightSum :: HasCallStack => a
    zeroWeightSum = error
        "partitionValue: The specified weights must have a non-zero sum."

    weights :: NonEmpty Natural
    weights = coinToNatural <$> targets

    coinToNatural :: Coin -> Natural
    coinToNatural = fromIntegral . unCoin

    -- This conversion is safe, because 'partitionValue' guarantees to produce
    -- a list where every entry is less than or equal to the target value.
    naturalToCoin :: Natural -> Coin
    naturalToCoin = Coin . fromIntegral

-- Create a 'TokenBundle' from a 'TokenMap' by assigning it a minimum required
-- Ada value from a coin source. Returns 'Nothing' if there's not enough 'Coin'
-- to cover the minimum amount, and return a 'TokenBundle' and the remainder
-- coins otherwise.
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

coinQuantity :: UTxOIndex -> Natural
coinQuantity =
    fromIntegral . unCoin . TokenBundle.getCoin . view #balance

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

distance :: Natural -> Natural -> Natural
distance a b
    | a > b = a - b
    | a < b = b - a
    | otherwise = 0

subtractCoin :: Coin -> Coin -> Maybe Coin
subtractCoin (Coin a) (Coin b)
    | a >= b    = Just $ Coin (a - b)
    | otherwise = Nothing

addCoin :: Coin -> Coin -> Coin
addCoin (Coin a) (Coin b) = Coin (a + b)
