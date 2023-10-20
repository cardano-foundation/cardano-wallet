{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module contains functions for incrementally constructing a selection
-- to be included in a migration plan.
--
-- A selection is the basis for a single transaction.
--
-- Use 'create' to create a selection with one or more inputs.
-- Use 'extend' to extend a selection with an additional input.
--
module Cardano.Wallet.Balance.Migration.Selection
    (
    -- * Types
      Selection (..)
    , SelectionError (..)
    , SelectionFullError (..)
    , RewardWithdrawal (..)

    -- * Creating selections
    , create

    -- * Extending selections
    , extend

    -- * Balancing selections
    , balance

    -- * Adding value to outputs
    , addValueToOutputs

    -- * Minimizing fees
    , minimizeFee
    , minimizeFeeStep

    -- * Computing bulk properties of selections
    , computeCurrentFee
    , computeCurrentSize
    , computeMinimumFee

    -- * Verifying selections for correctness
    , verify
    , SelectionCorrectness (..)

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Constants
    ( maxLengthAddress
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , TxSize
    , txOutMaxCoin
    , txOutputCoinCost
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( (>=>)
    )
import Data.Bifunctor
    ( first
    )
import Data.Either.Extra
    ( eitherToMaybe
    , maybeToEither
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( catMaybes
    , listToMaybe
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

-- | A selection is the basis for a single transaction.
--
-- Use 'create' to create a selection with one or more inputs.
-- Use 'extend' to extend a selection with an additional input.
-- Use 'verify' to verify the correctness of a selection.
--
data Selection input = Selection
    { inputIds :: !(NonEmpty input)
      -- ^ The selected inputs.
    , inputBalance :: !TokenBundle
      -- ^ The total balance of value provided by the inputs.
    , outputs :: !(NonEmpty TokenBundle)
      -- ^ The outputs, adjusted to pay for the fee.
    , fee :: !Coin
      -- ^ The actual fee payable for this selection.
    , feeExcess :: !Coin
      -- ^ The excess over the minimum permissible fee for this selection.
    , size :: !TxSize
      -- ^ The size of this selection.
    , rewardWithdrawal :: !Coin
      -- ^ The reward withdrawal amount, if any.
    }
    deriving (Eq, Generic, Show)

instance NFData input => NFData (Selection input)

newtype RewardWithdrawal = RewardWithdrawal
    { unRewardWithdrawal :: Coin }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Selection errors
--------------------------------------------------------------------------------

-- | Indicates a failure to create or extend a selection.
--
data SelectionError
    = SelectionAdaInsufficient
    -- ^ Indicates that the desired selection would not have enough ada to pay
    -- for the minimum permissible fee.
    | SelectionFull
    -- ^ Indicates that the desired selection would exceed the maximum
    -- selection size.
      SelectionFullError
    deriving (Eq, Show)

data SelectionFullError = SelectionFullError
    { selectionSizeMaximum :: TxSize
    , selectionSizeRequired :: TxSize
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Creating selections
--------------------------------------------------------------------------------

-- | Creates a selection with the given inputs.
--
-- Guarantees the following property for a returned selection 's':
--
-- >>> verify s == SelectionCorrect
--
-- Returns 'SelectionAdaInsufficient' if the desired selection would not have
-- enough ada to pay for the fee.
--
-- Returns 'SelectionFull' if the desired selection would exceed the maximum
-- selection size.
--
create
    :: TxConstraints
    -> RewardWithdrawal
    -> NonEmpty (input, TokenBundle)
    -> Either SelectionError (Selection input)
create constraints reward inputs =
    balance constraints $ Selection
        { inputBalance = F.foldMap snd inputs
        , inputIds = fst <$> inputs
        , outputs = assignMinimumAdaQuantity constraints <$>
            F.foldl'
                (addValueToOutputs constraints . NE.toList)
                (addValueToOutputs constraints [] (NE.head inputMaps))
                (NE.tail inputMaps)
        , fee = Coin 0
        , feeExcess = Coin 0
        , size = mempty
        , rewardWithdrawal = unRewardWithdrawal reward
        }
  where
    inputMaps = view #tokens . snd <$> inputs

--------------------------------------------------------------------------------
-- Extending selections
--------------------------------------------------------------------------------

-- | Extends a selection with an additional input.
--
-- Guarantees the following property for a returned selection 's':
--
-- >>> verify s == SelectionCorrect
--
-- Returns 'SelectionAdaInsufficient' if the desired selection would not have
-- enough ada to pay for the fee.
--
-- Returns 'SelectionFull' if the desired selection would exceed the maximum
-- selection size.
--
extend
    :: TxConstraints
    -> Selection input
    -> (input, TokenBundle)
    -> Either SelectionError (Selection input)
extend constraints selection (inputId, inputBundle) =
    balance constraints $ Selection
        { inputBalance = inputBundle <> inputBalance selection
        , inputIds = inputId `NE.cons` inputIds selection
        , outputs = assignMinimumAdaQuantity constraints <$>
            addValueToOutputs constraints
                (view #tokens <$> NE.toList (outputs selection))
                (view #tokens inputBundle)
        , fee = Coin 0
        , feeExcess = Coin 0
        , size = mempty
        , rewardWithdrawal = rewardWithdrawal selection
        }

--------------------------------------------------------------------------------
-- Balancing selections
--------------------------------------------------------------------------------

-- | Balances the fee for a given selection.
--
-- The ada quantities of the outputs are maximized in order to minimize the fee
-- excess.
--
-- Pre-condition: outputs have minimal ada quantities.
--
-- Guarantees the following property for a returned selection 's':
--
-- >>> verify s == SelectionCorrect
--
balance
    :: TxConstraints
    -> Selection input
    -> Either SelectionError (Selection input)
balance constraints unbalancedSelection = do
    let minimizedOutputs = outputs unbalancedSelection
    unbalancedFee <- first (const SelectionAdaInsufficient) $
        computeCurrentFee unbalancedSelection
    let minimumFeeForUnbalancedSelection =
            computeMinimumFee constraints unbalancedSelection
    unbalancedFeeExcess <- maybeToEither SelectionAdaInsufficient $
        Coin.subtract unbalancedFee minimumFeeForUnbalancedSelection
    let (minimizedFeeExcess, maximizedOutputs) = minimizeFee constraints
            (unbalancedFeeExcess, minimizedOutputs)
    let costIncrease = Coin.distance
            (totalCoinCost minimizedOutputs)
            (totalCoinCost maximizedOutputs)
    let balancedSelection = unbalancedSelection
            { fee = mconcat
                [ minimumFeeForUnbalancedSelection
                , minimizedFeeExcess
                , costIncrease
                ]
            , feeExcess = minimizedFeeExcess
            , outputs = maximizedOutputs
            }
    size <- guardSize constraints $
        computeCurrentSize constraints balancedSelection
    pure balancedSelection {size}
  where
    totalCoinCost :: NonEmpty TokenBundle -> Coin
    totalCoinCost = F.foldMap (txOutputCoinCost constraints . view #coin)

assignMinimumAdaQuantity :: TxConstraints -> TokenMap -> TokenBundle
assignMinimumAdaQuantity constraints m =
    TokenBundle c m
  where
    -- Using @maxLengthAddressFor@ via @constraints@ would not help
    -- here, as outputs created by the migration algorithm are assigned with
    -- user-defined addresses.
    --
    -- Something we /could/ do would be to pass in the actual user-defined
    -- addresses here, since they are available in the 'createMigrationPlan'
    -- server handler.
    --
    c = txOutputMinimumAdaQuantity constraints maxLengthAddress m

--------------------------------------------------------------------------------
-- Adding value to outputs
--------------------------------------------------------------------------------

-- | Adds value (obtained from an input) to an existing set of output maps.
--
-- This function attempts to merge the given value into one of the existing
-- output maps. If merging is successful, then the returned output map list
-- will be identical in length and content to the original output map list,
-- except for the merged output.
--
-- If the given value cannot be merged into one of the existing output maps
-- (because it would cause an output to exceed the output size limit), then
-- this function appends the given output map to the given output map list,
-- effectively creating a new output.
--
-- Pre-condition: all output maps in the given list must be within the output
-- size limit.
--
-- Assuming the above pre-condition is met, this function guarantees that all
-- output maps in the returned list will also be within the output size limit.
--
addValueToOutputs
    :: TxConstraints
    -> [TokenMap]
    -- ^ Outputs
    -> TokenMap
    -- ^ Output value to add
    -> NonEmpty TokenMap
    -- ^ Outputs with the additional value added
addValueToOutputs constraints outputsOriginal outputUnchecked =
    -- We need to be a bit careful with the output value to be added, as it may
    -- itself be oversized. We split it up if any of the output size limits are
    -- exceeded:
    NE.fromList
        $ F.foldl' (flip add) outputsOriginal
        $ splitOutputIfLimitsExceeded constraints outputUnchecked
  where
    -- Add an output value (whose size has been checked) to the existing
    -- outputs, merging it into one of the existing outputs if possible.
    add :: TokenMap -> [TokenMap] -> [TokenMap]
    add output outputs = run [] outputsSorted
      where
        -- Attempt to merge the specified output value into one of the existing
        -- outputs, by trying each existing output in turn, and terminating as
        -- soon as a successful candidate for merging is found.
        run :: [TokenMap] -> [TokenMap] -> [TokenMap]
        run considered (candidate : unconsidered) =
            case safeMerge output candidate of
                Just merged -> merged : (considered <> unconsidered)
                Nothing -> run (candidate : considered) unconsidered
        run considered [] =
            -- Merging with an existing output is not possible, so just make
            -- a new output.
            output : considered

        -- To minimize both the number of merge attempts and the size increase
        -- of the merged output compared to the original, we sort the existing
        -- outputs into ascending order according to the number of assets that
        -- would need to be added to each output.
        --
        -- In the absolute ideal case, where an existing output's assets are a
        -- superset of the output value to be added, merging with that output
        -- will not increase its asset count.
        --
        -- As a tie-breaker, we give priority to outputs with smaller numbers
        -- of assets. Merging with a smaller output is more likely to succeed,
        -- because merging with a larger output is more likely to fall foul of
        -- the output size limit.
        outputsSorted :: [TokenMap]
        outputsSorted = L.sortOn sortOrder outputs
          where
            sortOrder targetOutput =
                (targetOutputAssetCountIncrease, targetOutputAssetCount)
              where
                targetOutputAssetCount
                    = Set.size targetOutputAssets
                targetOutputAssetCountIncrease
                    = Set.size
                    $ Set.difference sourceOutputAssets targetOutputAssets
                sourceOutputAssets = TokenMap.getAssets output
                targetOutputAssets = TokenMap.getAssets targetOutput

    safeMerge :: TokenMap -> TokenMap -> Maybe TokenMap
    safeMerge a b
        | isSafe = Just value
        | otherwise = Nothing
      where
        isSafe = (&&)
            (txOutputHasValidSizeIfAdaMaximized constraints value)
            (txOutputHasValidTokenQuantities constraints value)
        value = a <> b

--------------------------------------------------------------------------------
-- Splitting output values
--------------------------------------------------------------------------------

-- | Splits up an output map into smaller maps if it exceeds any of the limits.
--
splitOutputIfLimitsExceeded
    :: TxConstraints
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfLimitsExceeded constraints =
    splitOutputIfTokenQuantityExceedsLimit constraints >=>
    splitOutputIfSizeExceedsLimit constraints

-- | Splits up an output map if it exceeds the serialized size limit.
--
splitOutputIfSizeExceedsLimit
    :: TxConstraints
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfSizeExceedsLimit constraints value
    | txOutputHasValidSizeIfAdaMaximized constraints value =
        pure value
    | otherwise =
        split value >>= splitOutputIfSizeExceedsLimit constraints
  where
    split = flip TokenMap.equipartitionAssets (() :| [()])

-- | Splits up an output map if any individual token quantity exceeds the limit.
--
splitOutputIfTokenQuantityExceedsLimit
    :: TxConstraints
    -> TokenMap
    -> NonEmpty TokenMap
splitOutputIfTokenQuantityExceedsLimit
    = flip TokenMap.equipartitionQuantitiesWithUpperBound
    . txOutputMaximumTokenQuantity

-- | Checks that an output has a valid size even if it is assigned the maximum
--   possible ada quantity.
--
-- Using this function to check all outputs provided to 'balance' will ensure
-- that it has complete freedom to adjust the ada quantities of those outputs,
-- without exceeding the output size limit.
--
txOutputHasValidSizeIfAdaMaximized :: TxConstraints -> TokenMap -> Bool
txOutputHasValidSizeIfAdaMaximized constraints output =
    txOutputHasValidSize constraints (TokenBundle txOutMaxCoin output)

--------------------------------------------------------------------------------
-- Minimizing fees
--------------------------------------------------------------------------------

-- | Minimizes the given fee excess by adding ada to the given output bundles.
--
-- This function:
--
--  - guarantees to leave all non-ada quantities unchanged.
--
--  - guarantees to not change the length of the list.
--
--  - guarantees that each resulting output bundle will have an ada quantity
--    that is greater than or equal to its original ada quantity.
--
--  - guarantees that the resulting fee excess will be less than or equal to
--    the original fee excess.
--
--  - does not check that the given ada quantities are above the minimum
--    required for each output, and therefore only guarantees that the
--    resulting ada quantities will be above the minimum required if the
--    caller makes this guarantee for the original output bundles.
--
-- This function aims to adjust as few output bundles as possible, and in the
-- ideal case, will increase the ada quantity of just one output bundle.
--
-- Increasing the ada quantity of an output may increase the overall cost of
-- that output, as increasing an ada quantity may increase the length of the
-- binary representation used to encode that quantity.
--
-- By maximizing the ada increase of a single output, and minimizing the ada
-- increases of the remaining outputs, we can minimize the cost increase of
-- the overall selection, and therefore maximize the chance of being able to
-- pay for the selection.
--
-- This is a consequence of the following mathematical relationship:
--
-- Consider a non-negative integer constant 'a' defined in terms of a summation
-- of a fixed number 'n' of non-negative integer variables:
--
--    >>> a = a1 + a2 + a3 + ... + an
--
-- Now consider the total space 's' required to encode all of the variables:
--
--    >>> s = length a1 + length a2 + length a3 + ... + length an
--
-- For any given number base, we can get close to the minimal value of 's' by
-- making the following assignments:
--
--    >>> a1 := a
--    >>> a2 := 0
--    >>> a3 := 0
--    >>> ...
--    >>> an := 0
--
-- Consider the following example, working in base 10:
--
--    >>> a = 999
--    >>> n = 9
--
-- If we were to use a flat distribution, where the constant is partitioned
-- into 'n' equal quantities (modulo rounding), our space cost 's' would be:
--
--    >>> s = length  a1 + length  a2 + length  a3 + ... + length  a9
--    >>> s = length 111 + length 111 + length 111 + ... + length 111
--    >>> s =          3 +          3 +          3 + ... +          3
--    >>> s =          3 × 9
--    >>> s = 27
--
-- But by maximizing 'a1' and minimizing the remaining variables, we can obtain
-- the following smaller space cost:
--
--    >>> s = length  a1 + length  a2 + length  a3 + ... + length  a9
--    >>> s = length 999 + length   0 + length   0 + ... + length   0
--    >>> s =          3 +          1 +          1 + ... +          1
--    >>> s =          3 +          8
--    >>> s = 11
--
minimizeFee
    :: TxConstraints
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles.
    -> (Coin, NonEmpty TokenBundle)
    -- ^ Fee excess and output bundles after optimization.
minimizeFee constraints (currentFeeExcess, outputs) =
    NE.fromList <$> run currentFeeExcess (NE.toList outputs) []
  where
    run :: Coin -> [TokenBundle] -> [TokenBundle] -> (Coin, [TokenBundle])
    run (Coin 0) remaining processed =
        (Coin 0, processed <> remaining)
    run feeExcessRemaining [] processed =
        (feeExcessRemaining, processed)
    run feeExcessRemaining (output : remaining) processed =
        run feeExcessRemaining' remaining (output' : processed)
      where
        (feeExcessRemaining', output') =
            minimizeFeeStep constraints (feeExcessRemaining, output)

-- | Minimizes the given fee excess by adding ada to the given output.
--
-- This function:
--
--  - guarantees to leave all non-ada quantities unchanged.
--
--  - increases the ada quantity of the given output until it is no longer
--    economically worthwhile to increase it further (i.e., if the cost of
--    a further increase would be greater than the increase itself).
--
--  - guarantees that the resulting output bundle will have an ada quantity
--    that is greater than or equal to its original ada quantity.
--
--  - guarantees that the resulting fee excess will be less than or equal to
--    the original fee excess.
--
-- Returns the minimized fee excess and the modified output.
--
minimizeFeeStep
    :: TxConstraints
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle.
    -> (Coin, TokenBundle)
    -- ^ Fee excess and output bundle after optimization.
minimizeFeeStep constraints =
    findFixedPoint reduceFee
  where
    reduceFee :: (Coin, TokenBundle) -> (Coin, TokenBundle)
    reduceFee (feeExcess, outputBundle)
        | outputCoinFinal > outputCoin &&
          outputCoinFinalCostIncrease < outputCoinFinalIncrease =
            (feeExcessFinal, outputBundleFinal)
        | otherwise =
            (feeExcess, outputBundle)
      where
        outputCoin = view #coin outputBundle
        outputCoinMaxCostIncrease = Coin.distance
            (txOutputCoinCost constraints outputCoin)
            (txOutputCoinCost constraints $ outputCoin <> feeExcess)
        outputCoinFinal = Coin
            $ unCoin outputCoin
            + unCoin feeExcess
            - unCoin outputCoinMaxCostIncrease
        outputCoinFinalCostIncrease = Coin.distance
            (txOutputCoinCost constraints outputCoin)
            (txOutputCoinCost constraints outputCoinFinal)
        outputCoinFinalIncrease = Coin.distance outputCoin outputCoinFinal
        outputBundleFinal = TokenBundle.setCoin outputBundle outputCoinFinal
        feeExcessFinal = Coin
            $ unCoin feeExcess
            - unCoin outputCoinFinalIncrease
            - unCoin outputCoinFinalCostIncrease

--------------------------------------------------------------------------------
-- Computing bulk properties of selections
--------------------------------------------------------------------------------

-- | Calculates the current fee for a selection.
--
computeCurrentFee :: Selection input -> Either NegativeCoin Coin
computeCurrentFee Selection {inputBalance, outputs, rewardWithdrawal}
    | adaBalanceIn >= adaBalanceOut =
        Right adaDifference
    | otherwise =
        Left (NegativeCoin adaDifference)
  where
    adaBalanceIn =
        rewardWithdrawal <> view #coin inputBalance
    adaBalanceOut =
        F.foldMap (TokenBundle.getCoin) outputs
    adaDifference =
        Coin.distance adaBalanceIn adaBalanceOut

-- | Calculates the current size of a selection.
--
computeCurrentSize
    :: TxConstraints
    -> Selection input
    -> TxSize
computeCurrentSize constraints selection = mconcat
    [ txBaseSize constraints
    , F.foldMap (const $ txInputSize constraints) (inputIds selection)
    , F.foldMap (txOutputSize constraints) (outputs selection)
    , txRewardWithdrawalSize constraints (rewardWithdrawal selection)
    ]

-- | Calculates the minimum permissible fee for a selection.
--
computeMinimumFee :: TxConstraints -> Selection input -> Coin
computeMinimumFee constraints selection = mconcat
    [ txBaseCost constraints
    , F.foldMap (const $ txInputCost constraints) (inputIds selection)
    , F.foldMap (txOutputCost constraints) (outputs selection)
    , txRewardWithdrawalCost constraints (rewardWithdrawal selection)
    ]

--------------------------------------------------------------------------------
-- Verifying selections for correctness
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
    = SelectionAssetBalanceIncorrect
      SelectionAssetBalanceIncorrectError
    | SelectionFeeIncorrect
      SelectionFeeIncorrectError
    | SelectionFeeExcessIncorrect
      SelectionFeeExcessIncorrectError
    | SelectionFeeInsufficient
      SelectionFeeInsufficientError
    | SelectionOutputBelowMinimumAdaQuantity
      SelectionOutputBelowMinimumAdaQuantityError
    | SelectionOutputSizeExceedsLimit
      SelectionOutputSizeExceedsLimitError
    | SelectionSizeExceedsLimit
      SelectionSizeExceedsLimitError
    | SelectionSizeIncorrect
      SelectionSizeIncorrectError
    deriving (Eq, Show)

-- | Verifies a selection for correctness.
--
-- This function is provided primarily as a convenience for testing. As such,
-- it's not usually necessary to call this function from ordinary application
-- code, unless you suspect that a selection value is incorrect in some way.
--
verify
    :: TxConstraints
    -> Selection input
    -> SelectionCorrectness
verify constraints selection =
    either SelectionIncorrect (const SelectionCorrect) verifyAll
  where
    verifyAll :: Either SelectionCorrectnessError ()
    verifyAll = do
        checkAssetBalance selection
            `failWith` SelectionAssetBalanceIncorrect
        checkFee selection
            `failWith` SelectionFeeIncorrect
        checkFeeSufficient constraints selection
            `failWith` SelectionFeeInsufficient
        checkFeeExcess constraints selection
            `failWith` SelectionFeeExcessIncorrect
        checkOutputMinimumAdaQuantities constraints selection
            `failWith` SelectionOutputBelowMinimumAdaQuantity
        checkOutputSizes constraints selection
            `failWith` SelectionOutputSizeExceedsLimit
        checkSizeWithinLimit constraints selection
            `failWith` SelectionSizeExceedsLimit
        checkSizeCorrectness constraints selection
            `failWith` SelectionSizeIncorrect

    failWith :: Maybe e1 -> (e1 -> e2) -> Either e2 ()
    onError `failWith` thisError = maybe (Right ()) (Left . thisError) onError

--------------------------------------------------------------------------------
-- Selection correctness: asset balance correctness
--------------------------------------------------------------------------------

data SelectionAssetBalanceIncorrectError = SelectionAssetBalanceIncorrectError
    { assetBalanceInputs
        :: TokenMap
    , assetBalanceOutputs
        :: TokenMap
    }
    deriving (Eq, Show)

checkAssetBalance
    :: Selection input
    -> Maybe SelectionAssetBalanceIncorrectError
checkAssetBalance Selection {inputBalance, outputs}
    | assetBalanceInputs == assetBalanceOutputs =
        Nothing
    | otherwise =
        Just SelectionAssetBalanceIncorrectError
            { assetBalanceInputs
            , assetBalanceOutputs
            }
  where
    assetBalanceInputs = view #tokens inputBalance
    assetBalanceOutputs = F.foldMap (tokens) outputs

--------------------------------------------------------------------------------
-- Selection correctness: fee correctness
--------------------------------------------------------------------------------

data SelectionFeeIncorrectError = SelectionFeeIncorrectError
    { selectionFeeComputed
        :: Either NegativeCoin Coin
    , selectionFeeStored
        :: Coin
    }
    deriving (Eq, Show)

checkFee :: Selection input -> Maybe SelectionFeeIncorrectError
checkFee selection =
    case computeCurrentFee selection of
      Left negativeFee ->
          pure SelectionFeeIncorrectError
              { selectionFeeComputed = Left negativeFee
              , selectionFeeStored = fee selection
              }
      Right positiveFee | positiveFee /= fee selection ->
          pure SelectionFeeIncorrectError
              { selectionFeeComputed = Right positiveFee
              , selectionFeeStored = fee selection
              }
      Right _ ->
          Nothing

--------------------------------------------------------------------------------
-- Selection correctness: fee excess correctness
--------------------------------------------------------------------------------

data SelectionFeeExcessIncorrectError = SelectionFeeExcessIncorrectError
    { selectionFeeExcessActual
        :: Coin
    , selectionFeeExcessExpected
        :: Coin
    }
    deriving (Eq, Show)

checkFeeExcess
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionFeeExcessIncorrectError
checkFeeExcess constraints selection =
    checkInner =<< eitherToMaybe (computeCurrentFee selection)
  where
    checkInner :: Coin -> Maybe SelectionFeeExcessIncorrectError
    checkInner currentSelectionFee
        | selectionFeeExcessExpected == selectionFeeExcessActual =
            Nothing
        | otherwise =
            Just SelectionFeeExcessIncorrectError
                { selectionFeeExcessActual
                , selectionFeeExcessExpected
                }
      where
        selectionFeeExcessActual = feeExcess selection
        selectionFeeExcessExpected = Coin.distance
            (currentSelectionFee)
            (computeMinimumFee constraints selection)

--------------------------------------------------------------------------------
-- Selection correctness: fee sufficiency
--------------------------------------------------------------------------------

data SelectionFeeInsufficientError = SelectionFeeInsufficientError
    { selectionFeeActual
        :: Either NegativeCoin Coin
    , selectionFeeMinimum
        :: Coin
    }
    deriving (Eq, Show)

checkFeeSufficient
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionFeeInsufficientError
checkFeeSufficient constraints selection =
    case computeCurrentFee selection of
        Left nf ->
            Just SelectionFeeInsufficientError
                { selectionFeeActual = Left nf
                , selectionFeeMinimum
                }
        Right pf | pf < selectionFeeMinimum ->
            Just SelectionFeeInsufficientError
                { selectionFeeActual = Right pf
                , selectionFeeMinimum
                }
        Right _ ->
            Nothing
  where
    selectionFeeMinimum = computeMinimumFee constraints selection

--------------------------------------------------------------------------------
-- Selection correctness: minimum ada quantities
--------------------------------------------------------------------------------

data SelectionOutputBelowMinimumAdaQuantityError =
    SelectionOutputBelowMinimumAdaQuantityError
        { outputBundle :: TokenBundle
          -- ^ The output that is below the expected minimum ada quantity.
        , expectedMinimumAdaQuantity :: Coin
          -- ^ The expected minimum ada quantity.
        }
    deriving (Eq, Show)

checkOutputMinimumAdaQuantities
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionOutputBelowMinimumAdaQuantityError
checkOutputMinimumAdaQuantities constraints selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionOutputBelowMinimumAdaQuantityError
    checkOutput outputBundle
        | TokenBundle.getCoin outputBundle >= expectedMinimumAdaQuantity =
            Nothing
        | otherwise =
            Just SelectionOutputBelowMinimumAdaQuantityError
                { outputBundle
                , expectedMinimumAdaQuantity
                }
      where
        expectedMinimumAdaQuantity = txOutputMinimumAdaQuantity constraints
            maxLengthAddress
            (view #tokens outputBundle)

--------------------------------------------------------------------------------
-- Selection correctness: output sizes
--------------------------------------------------------------------------------

newtype SelectionOutputSizeExceedsLimitError =
    SelectionOutputSizeExceedsLimitError
        { selectionOutput :: TokenBundle }
    deriving (Eq, Show)

checkOutputSizes
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionOutputSizeExceedsLimitError
checkOutputSizes constraints selection =
     maybesToMaybe $ checkOutput <$> outputs selection
  where
    checkOutput
        :: TokenBundle
        -> Maybe SelectionOutputSizeExceedsLimitError
    checkOutput selectionOutput
        | txOutputHasValidSize constraints selectionOutput =
            Nothing
        | otherwise =
            Just SelectionOutputSizeExceedsLimitError
                { selectionOutput }

--------------------------------------------------------------------------------
-- Selection correctness: selection size (in comparison to the stored value)
--------------------------------------------------------------------------------

data SelectionSizeIncorrectError = SelectionSizeIncorrectError
    { selectionSizeComputed :: TxSize
    , selectionSizeStored :: TxSize
    }
    deriving (Eq, Show)

checkSizeCorrectness
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionSizeIncorrectError
checkSizeCorrectness constraints selection
    | selectionSizeComputed == selectionSizeStored =
        Nothing
    | otherwise = pure SelectionSizeIncorrectError
        { selectionSizeComputed
        , selectionSizeStored
        }
  where
    selectionSizeComputed = computeCurrentSize constraints selection
    selectionSizeStored = size selection

--------------------------------------------------------------------------------
-- Selection correctness: selection size (in comparison to the limit)
--------------------------------------------------------------------------------

data SelectionSizeExceedsLimitError = SelectionSizeExceedsLimitError
    { selectionSizeComputed :: TxSize
    , selectionSizeMaximum :: TxSize
    }
    deriving (Eq, Show)

checkSizeWithinLimit
    :: TxConstraints
    -> Selection input
    -> Maybe SelectionSizeExceedsLimitError
checkSizeWithinLimit constraints selection
    | selectionSizeComputed <= selectionSizeMaximum =
        Nothing
    | otherwise = pure SelectionSizeExceedsLimitError
        { selectionSizeComputed
        , selectionSizeMaximum
        }
  where
    selectionSizeComputed = computeCurrentSize constraints selection
    selectionSizeMaximum = txMaximumSize constraints

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

newtype NegativeCoin = NegativeCoin
    { unNegativeCoin :: Coin
    }
    deriving (Eq, Show)

findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f = findInner
  where
    findInner a = let fa = f a in if a == fa then a else findInner fa

guardSize
    :: TxConstraints
    -> TxSize
    -> Either SelectionError TxSize
guardSize constraints selectionSizeRequired
    | selectionSizeRequired <= selectionSizeMaximum =
        pure selectionSizeRequired
    | otherwise =
        Left $ SelectionFull SelectionFullError
            { selectionSizeMaximum
            , selectionSizeRequired
            }
  where
    selectionSizeMaximum = txMaximumSize constraints

maybesToMaybe :: NonEmpty (Maybe a) -> Maybe a
maybesToMaybe = listToMaybe . catMaybes . NE.toList
