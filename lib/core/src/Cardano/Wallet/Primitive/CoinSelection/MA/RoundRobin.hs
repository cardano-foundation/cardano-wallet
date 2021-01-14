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
    , makeChangeForCoin
    , makeChangeForPaymentAssets
    , makeChangeForSurplusAssets

    -- * Partitioning
    , partitionCoin
    , partitionTokenQuantity
    , partitionValue

    -- * Grouping and ungrouping
    , groupByKey
    , ungroupByKey

    -- * Round-robin processing
    , runRoundRobin
    , runRoundRobinM

    -- * Utility functions
    , distance

    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( padCoalesce, partitionNatural )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
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
import Data.Function
    ( (&) )
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
    -> UTxOIndex
        -- ^ UTxO entries available for selection
    -> TokenBundle
        -- ^ Minimum balance to cover
    -> m SelectionState
        -- ^ Final selection state
runSelection limit available minimumBalance =
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
        { currentQuantity = coinQuantity . selected
        , minimumQuantity = fromIntegral $ unCoin minimumCoinQuantity
        , selectQuantity = selectMatchingQuantity limit
            [ WithAdaOnly
            , Any
            ]
        }

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

makeChange
    :: NonEmpty TokenBundle
        -- ^ Token bundles of selected inputs
    -> NonEmpty TokenBundle
        -- ^ Token bundles of original outputs
    -> NonEmpty TokenBundle
        -- ^ Change bundles
makeChange inputBundles outputBundles
    | not (totalOutputValue `leq` totalInputValue) =
        totalInputValueInsufficient
    | totalOutputCoinValue == Coin 0 =
        totalOutputCoinValueIsZero
    | otherwise =
        change
  where
    totalInputValueInsufficient = error
        "makeChange: not (totalOutputValue <= totalInputValue)"
    totalOutputCoinValueIsZero = error
        "makeChange: not (totalOutputCoinValue > 0)"

    totalInputValue :: TokenBundle
    totalInputValue = F.fold inputBundles

    totalOutputValue :: TokenBundle
    totalOutputValue = F.fold outputBundles

    totalOutputCoinValue :: Coin
    totalOutputCoinValue = TokenBundle.getCoin totalOutputValue

    excess :: TokenBundle
    excess =
        -- The following subtraction is safe, as we have already checked that
        -- the total input value is greater than the total output value:
        totalInputValue `TokenBundle.unsafeSubtract` totalOutputValue

    change :: NonEmpty TokenBundle
    change
        = TokenBundle.fromCoin <$> changeForCoin
        & NE.zipWith (<>) (TokenBundle.fromTokenMap <$> changeForPaymentAssets)
        -- Here we sort in ascending order of coin value so that surplus assets
        -- and coin values are in increasing order in the resulting bundles:
        & NE.sortWith TokenBundle.getCoin
        & NE.zipWith (<>) (TokenBundle.fromTokenMap <$> changeForSurplusAssets)

    -- Change for the excess coin quantity.
    changeForCoin :: NonEmpty Coin
    changeForCoin = makeChangeForCoin
        (TokenBundle.getCoin excess)
        (TokenBundle.getCoin <$> outputBundles)

    -- Change for excess quantities of assets included in outputs.
    changeForPaymentAssets :: NonEmpty TokenMap
    changeForPaymentAssets = makeChangeForPaymentAssets
        (TokenMap.filter (`Set.member` paymentAssetIds) (view #tokens excess))
        (view #tokens <$> outputBundles)

    -- Change for excess quantities of assets NOT included in outputs.
    changeForSurplusAssets :: NonEmpty TokenMap
    changeForSurplusAssets =
        makeChangeForSurplusAssets surplusAssetsGrouped outputBundles
      where
        surplusAssetsGrouped :: Map AssetId (NonEmpty TokenQuantity)
        surplusAssetsGrouped = groupByKey surplusAssets

        surplusAssets :: [(AssetId, TokenQuantity)]
        surplusAssets = filter
            ((`Set.notMember` paymentAssetIds) . fst)
            (TokenMap.toFlatList . view #tokens =<< NE.toList inputBundles)

    -- Identifiers of assets included in outputs.
    paymentAssetIds :: Set AssetId
    paymentAssetIds = TokenBundle.getAssets totalOutputValue

-- | Makes change for the excess quantity of ada.
--
makeChangeForCoin
    :: HasCallStack
    => Coin
    -> NonEmpty Coin
    -> NonEmpty Coin
makeChangeForCoin = partitionCoin

-- | Makes change for excess quantities of assets included in the outputs.
--
makeChangeForPaymentAssets
    :: HasCallStack
    => TokenMap
        -- ^ Excess to be distributed
    -> NonEmpty TokenMap
        -- ^ Maps from original outputs
    -> NonEmpty TokenMap
        -- ^ Change maps
makeChangeForPaymentAssets excess outputMaps =
    F.foldl'
        (NE.zipWith (<>))
        (TokenMap.empty <$ outputMaps)
        (changeForAsset <$> assetsToConsider)
  where
    assetsToConsider :: [AssetId]
    assetsToConsider = F.toList $ TokenMap.getAssets excess

    changeForAsset :: AssetId -> NonEmpty TokenMap
    changeForAsset asset = TokenMap.singleton asset <$>
        partitionTokenQuantity
            (TokenMap.getQuantity excess asset)
            (flip TokenMap.getQuantity asset <$> outputMaps)

-- | Makes change for excess quantities of assets NOT included in the outputs.
--
-- Distributes quantities to a number of token maps, where the number of token
-- maps to create is equal to the length of the target list.
--
-- If a given asset has fewer quantities than the target length, each of these
-- quantities will be distributed without modification to a separate token map.
--
-- If a given asset has more quantities than the target length, the smallest of
-- these quantities will be repeatedly coalesced together before distributing
-- them to token maps.
--
-- This function guarantees that:
--
--    - The total value of each asset is preserved.
--
--    - The resulting list of token maps is sorted in ascending order, so that
--      each token map in the list is less than or equal to its immediate
--      successor, when compared with the 'PartialOrder.leq' function.
--
-- Examples (shown with pseudo-code):
--
-- >>> makeChangeForSurplusAssets [("A", [1, 2])] [1]
-- [ TokenMap [("A", 3)] ]
--
-- >>> makeChangeForSurplusAssets [("A", [1, 2])] [1 .. 3]
-- [ TokenMap [        ]
-- , TokenMap [("A", 1)]
-- , TokenMap [("A", 2)]
-- ]
--
-- >>> makeChangeForSurplusAssets [("A", [1]), ("B", [2, 3, 4])] [1 .. 2]
-- [ TokenMap [          ("B", 4)]
-- , TokenMap [("A", 1), ("B", 5)]
-- ]
--
makeChangeForSurplusAssets
    :: Map AssetId (NonEmpty TokenQuantity)
    -- ^ Asset quantities
    -> NonEmpty a
    -- ^ Target list (denotes the desired length of the result)
    -> NonEmpty TokenMap
makeChangeForSurplusAssets assetQuantities target =
    F.foldl'
        (NE.zipWith (<>))
        (TokenMap.empty <$ target)
        (Map.mapWithKey distribute assetQuantities)
  where
    distribute :: AssetId -> NonEmpty TokenQuantity -> NonEmpty TokenMap
    distribute asset quantities =
        TokenMap.singleton asset <$> padCoalesce quantities target

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

partitionCoin
    :: HasCallStack
    => Coin
        -- ^ Target
    -> NonEmpty Coin
        -- ^ Weights
    -> NonEmpty Coin
partitionCoin = partitionValue coinToNatural naturalToCoin
  where
    -- This conversion is safe, because 'partitionValue' guarantees to produce
    -- a list where every entry is less than or equal to the target value.
    naturalToCoin :: Natural -> Coin
    naturalToCoin = Coin . fromIntegral

    coinToNatural :: Coin -> Natural
    coinToNatural = fromIntegral . unCoin

partitionTokenQuantity
    :: HasCallStack
    => TokenQuantity
        -- ^ Target
    -> NonEmpty TokenQuantity
        -- ^ Weights
    -> NonEmpty TokenQuantity
partitionTokenQuantity = partitionValue unTokenQuantity TokenQuantity

partitionValue
    :: forall a. HasCallStack
    => (a -> Natural) -> (Natural -> a)
    -> a
    -> NonEmpty a
    -> NonEmpty a
partitionValue toNatural fromNatural target weights =
    fromMaybe zeroWeightSum maybeResult
  where
    maybeResult :: Maybe (NonEmpty a)
    maybeResult =
        fmap fromNatural <$> partitionNatural
            (toNatural target)
            (toNatural <$> weights)
    zeroWeightSum = error
        "partitionValue: The specified weights must have a non-zero sum."

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
