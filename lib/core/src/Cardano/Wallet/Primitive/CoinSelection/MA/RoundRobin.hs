{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
    , emptySkeleton
    , selectionDelta
    , SelectionCriteria (..)
    , SelectionLimit (..)
    , SelectionSkeleton (..)
    , SelectionResult (..)
    , SelectionError (..)
    , BalanceInsufficientError (..)
    , OutputsInsufficientError (..)
    , SelectionInsufficientError (..)
    , InsufficientMinCoinValueError (..)
    , UnableToConstructChangeError (..)

    -- * Reporting
    , SelectionReport (..)
    , SelectionReportSummarized (..)
    , SelectionReportDetailed (..)
    , makeSelectionReport
    , makeSelectionReportSummarized
    , makeSelectionReportDetailed

    -- * Running a selection (without making change)
    , runSelection
    , SelectionState (..)

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

    -- * Accessors
    , fullBalance

    -- * Utility classes
    , AssetCount (..)

    -- * Utility functions
    , distance
    , mapMaybe
    , balanceMissing
    , missingOutputAssets
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( padCoalesce )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin, subtractCoin, sumCoins )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxIn
    , TxOut
    , txOutCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( SelectionFilter (..), UTxOIndex (..) )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Ord
    ( comparing )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), genericF, nameF, unlinesF )
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
    }
    deriving (Eq, Generic, Show)

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
    { skeletonInputCount
        :: !Int
    , skeletonOutputs
        :: ![TxOut]
    , skeletonChange
        :: ![Set AssetId]
    }
    deriving (Eq, Generic, Show)

-- | Creates an empty 'SelectionSkeleton' with no inputs, no outputs and no
-- change.
emptySkeleton :: SelectionSkeleton
emptySkeleton = SelectionSkeleton
    { skeletonInputCount = 0
    , skeletonOutputs = mempty
    , skeletonChange = mempty
    }

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
data SelectionResult change = SelectionResult
    { inputsSelected
        :: !(NonEmpty (TxIn, TxOut))
        -- ^ A (non-empty) list of inputs selected from 'utxoAvailable'.
    , extraCoinSource
        :: !(Maybe Coin)
        -- ^ An optional extra source of ada.
    , outputsCovered
        :: ![TxOut]
        -- ^ A list of outputs covered.
        -- FIXME: Left as a list to allow to work-around the limitation of
        -- 'performSelection' which cannot run for no output targets (e.g. in
        -- the context of a delegation transaction). This allows callers to
        -- specify a dummy 'TxOut' as argument, and remove it later in the
        -- result; Ideally, we want to handle this in a better way by allowing
        -- 'performSelection' to work with empty output targets. At the moment
        -- of writing these lines, I've already been yak-shaving for a while and
        -- this is the last remaining obstacle, not worth the effort _yet_.
    , changeGenerated
        :: ![change]
        -- ^ A list of generated change outputs.
    , utxoRemaining
        :: !UTxOIndex
        -- ^ The subset of 'utxoAvailable' that remains after performing
        -- the selection.
    }
    deriving (Generic, Eq, Show)

-- | Calculate the actual difference between the total outputs (incl. change)
-- and total inputs of a particular selection. By construction, this should be
-- greater than total fees and deposits.
selectionDelta
    :: (change -> Coin)
    -> SelectionResult change
    -> Coin
selectionDelta getChangeCoin sel@SelectionResult{inputsSelected,extraCoinSource} =
    let
        totalOut
            = sumCoins (getChangeCoin <$> changeGenerated sel)
            & addCoin  (sumCoins (txOutCoin <$> outputsCovered sel))

        totalIn
            = sumCoins (txOutCoin . snd <$> inputsSelected)
            & addCoin (fromMaybe (Coin 0) extraCoinSource)
    in
        Coin.distance totalIn totalOut

-- | Represents the set of errors that may occur while performing a selection.
--
data SelectionError
    = BalanceInsufficient
        BalanceInsufficientError
    | SelectionInsufficient
        SelectionInsufficientError
    | InsufficientMinCoinValues
        (NonEmpty InsufficientMinCoinValueError)
    | OutputsInsufficient
        OutputsInsufficientError
    | UnableToConstructChange
        UnableToConstructChangeError
    deriving (Generic, Eq, Show)

-- | Indicates that a portion of the minted assets were not spent or burned.
--
-- This situation occurs if the following inequality does not hold:
--
-- >>> assetsToMint `leq` (assetsToBurn <> requestedOutputAssets)
--
-- The existence of this error reflects a deliberate design choice: all minted
-- assets must either be explicitly spent or explicitly burned by the caller.
-- In future, we could revise this design to allow excess minted assets (those
-- that are neither spent nor burned) to be returned to the wallet as change.
--
data OutputsInsufficientError = OutputsInsufficientError
    { assetsToMint
        :: !TokenMap
      -- ^ The assets to mint
    , assetsToBurn
        :: !TokenMap
      -- ^ The assets to burn
    , requestedOutputAssets
        :: !TokenMap
      -- ^ The complete set of assets found within the user-specified outputs
    } deriving (Generic, Eq, Show)

-- | Computes the portion of minted assets that are not spent or burned.
--
missingOutputAssets :: OutputsInsufficientError -> TokenMap
missingOutputAssets e =
    -- We use 'difference' which will show us the quantities in 'assetsToMint'
    -- that are not in 'assetsToBurn <> requestedOutputAssets'.
    --
    -- Any asset quantity present in 'assetsToBurn <> requestedOutputAssets'
    -- but not present in 'assetsToMint' will simply be zeroed out, which is
    -- the behaviour we want for this error report.
    --
    assetsToMint `TokenMap.difference`
        (assetsToBurn `TokenMap.add` requestedOutputAssets)
  where
    OutputsInsufficientError
        { assetsToMint
        , assetsToBurn
        , requestedOutputAssets
        } = e

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

-- | Calculate the missing balance from a @BalanceInsufficientError@.
balanceMissing :: BalanceInsufficientError -> TokenBundle
balanceMissing (BalanceInsufficientError available required) =
    TokenBundle.difference required available

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

instance Buildable InsufficientMinCoinValueError where
    build (InsufficientMinCoinValueError o c) = unlinesF
        [ nameF "Expected min coin value" (build c)
        , nameF "TxOut" (build o)
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

-- | Performs a coin selection and generates change bundles in one step.
--
-- Returns 'BalanceInsufficient' if the total balance of 'utxoAvailable' is not
-- strictly greater than or equal to the total balance of 'outputsToCover'.
--
-- Returns 'OutputsInsufficientError' if the 'minted' values are not a subset
-- of the 'outputsToCover' plus the 'burned' values. That is, the minted values
-- are not spent or burned.
--
-- Provided that the total balance of 'utxoAvailable' is sufficient to cover
-- the total balance of 'outputsToCover', this function guarantees to return
-- an 'inputsSelected' value that satisfies:
--
--    ada asset balance:
--      balance inputsSelected + balance extraAdaSource
--      > balance outputsToCover + balance changeGenerated
--    non-ada asset balance:
--      balance inputsSelected + balance minted
--      == balance outputsToCover
--       + balance burned
--       + balance changeGenerated
--
-- Note that the ada asset balance equation is an inequality because of the
-- existence of a fee, and the non-ada asset balance is an equality because
-- fees are paid in ada.
--
-- Finally, this function guarantees that:
--
--    inputsSelected ∪ utxoRemaining == utxoAvailable
--    inputsSelected ∩ utxoRemaining == ∅
--    outputsCovered + minted == outputsToCover + burned
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
    -> TokenBundleSizeAssessor
        -- ^ A function that assesses the size of a token bundle. See the
        -- documentation for 'TokenBundleSizeAssessor' to learn about the
        -- expected properties of this function.
    -> SelectionCriteria
        -- ^ The selection goal to satisfy.
    -> m (Either SelectionError (SelectionResult TokenBundle))
performSelection minCoinFor costFor bundleSizeAssessor criteria
    -- Is the minted value all spent or burnt?
    | not (assetsToMint `leq` (assetsToBurn <> requestedOutputAssets)) =
        pure $ Left $ OutputsInsufficient $ OutputsInsufficientError
            {assetsToMint, assetsToBurn, requestedOutputAssets}

    -- Is the total available balance sufficient?
    | not (balanceRequired `leq` balanceAvailable) =
        pure $ Left $ BalanceInsufficient $ BalanceInsufficientError
            { balanceAvailable, balanceRequired }

    -- Are the minimum ada quantities of the outputs too small?
    | not (null insufficientMinCoinValues) =
        pure $ Left $ InsufficientMinCoinValues $
            NE.fromList insufficientMinCoinValues

    | otherwise = do
        state <- runSelection
            selectionLimit extraCoinSource utxoAvailable balanceRequired
        let balanceSelected = fullBalance (selected state) extraCoinSource
        if balanceRequired `leq` balanceSelected then
            makeChangeRepeatedly state
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
        , assetsToMint
        , assetsToBurn
        } = criteria

    requestedOutputs = F.foldMap (view #tokens) outputsToCover
    requestedOutputAssets = view #tokens requestedOutputs

    mkInputsSelected :: UTxOIndex -> NonEmpty (TxIn, TxOut)
    mkInputsSelected =
        fromMaybe invariantSelectAnyInputs . NE.nonEmpty . UTxOIndex.toList

    balanceAvailable :: TokenBundle
    balanceAvailable = fullBalance utxoAvailable extraCoinSource

    balanceRequired :: TokenBundle
    balanceRequired =
        -- of course, we need to satisfy the outputs the caller asked for
        requestedOutputs
        -- we must also find assets to burn
        `TokenBundle.add`
            TokenBundle.fromTokenMap assetsToBurn
        -- but assets minted reduce the quantity of assets we have to select
        `TokenBundle.unsafeSubtract`
            TokenBundle.fromTokenMap assetsToMint

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
            expectedMinCoinValue = minCoinFor (view (#tokens . #tokens) o)

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
        -> [Set AssetId]
    predictChange inputsPreSelected = either
        (const $ invariantResultWithNoCost inputsPreSelected)
        (fmap (TokenMap.getAssets . view #tokens))
        (makeChange MakeChangeCriteria
            { minCoinFor = noMinimumCoin
            , bundleSizeAssessor
            , requiredCost = noCost
            , extraCoinSource
            , inputBundles
            , outputBundles
            , assetsToMint
            , assetsToBurn
            }
        )
      where
        inputBundles = view #tokens . snd <$> mkInputsSelected inputsPreSelected
        outputBundles = view #tokens <$> outputsToCover

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
        :: SelectionState
        -> m (Either SelectionError (SelectionResult TokenBundle))
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
            { minCoinFor
            , bundleSizeAssessor
            , requiredCost
            , extraCoinSource
            , inputBundles = view #tokens . snd <$> inputsSelected
            , outputBundles = view #tokens <$> outputsToCover
            , assetsToMint
            , assetsToBurn
            }

        mkSelectionResult :: [TokenBundle] -> SelectionResult TokenBundle
        mkSelectionResult changeGenerated = SelectionResult
            { inputsSelected
            , extraCoinSource
            , changeGenerated = changeGenerated
            , outputsCovered = NE.toList outputsToCover
            , utxoRemaining = leftover
            }

        selectOneEntry = selectMatchingQuantity selectionLimit $
            WithAdaOnly :| [Any]

        SelectionState {selected, leftover} = s

        requiredCost = costFor SelectionSkeleton
            { skeletonInputCount = UTxOIndex.size selected
            , skeletonOutputs = NE.toList outputsToCover
            , skeletonChange
            }

        skeletonChange = predictChange selected
        inputsSelected = mkInputsSelected selected

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
    , totalAdaBalanceIn :: Coin
    , totalAdaBalanceOut :: Coin
    , adaBalanceOfSelectedInputs :: Coin
    , adaBalanceOfExtraInput :: Coin
    , adaBalanceOfRequestedOutputs :: Coin
    , adaBalanceOfGeneratedChangeOutputs :: Coin
    , numberOfSelectedInputs :: Int
    , numberOfRequestedOutputs :: Int
    , numberOfGeneratedChangeOutputs :: Int
    , numberOfUniqueNonAdaAssetsInSelectedInputs :: Int
    , numberOfUniqueNonAdaAssetsInRequestedOutputs :: Int
    , numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs :: Int
    , sizeOfRemainingUtxoSet :: Int
    }
    deriving (Eq, Generic, Show)

-- | Includes detailed information about a selection.
--
data SelectionReportDetailed = SelectionReportDetailed
    { selectedInputs :: [(TxIn, TxOut)]
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

makeSelectionReport
    :: SelectionResult TokenBundle -> SelectionReport
makeSelectionReport s = SelectionReport
    { summary = makeSelectionReportSummarized s
    , detail = makeSelectionReportDetailed s
    }

makeSelectionReportSummarized
    :: SelectionResult TokenBundle -> SelectionReportSummarized
makeSelectionReportSummarized s = SelectionReportSummarized {..}
  where
    computedFee
        = Coin.distance totalAdaBalanceIn totalAdaBalanceOut
    totalAdaBalanceIn
        = adaBalanceOfSelectedInputs <> adaBalanceOfExtraInput
    totalAdaBalanceOut
        = adaBalanceOfGeneratedChangeOutputs <> adaBalanceOfRequestedOutputs
    adaBalanceOfSelectedInputs
        = F.foldMap (view (#tokens . #coin) . snd) $ view #inputsSelected s
    adaBalanceOfExtraInput
        = F.fold (view #extraCoinSource s)
    adaBalanceOfGeneratedChangeOutputs
        = F.foldMap (view #coin) $ view #changeGenerated s
    adaBalanceOfRequestedOutputs
        = F.foldMap (view (#tokens . #coin)) $ view #outputsCovered s
    numberOfSelectedInputs
        = length $ view #inputsSelected s
    numberOfRequestedOutputs
        = length $ view #outputsCovered s
    numberOfGeneratedChangeOutputs
        = length $ view #changeGenerated s
    numberOfUniqueNonAdaAssetsInSelectedInputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens . snd)
        $ view #inputsSelected s
    numberOfUniqueNonAdaAssetsInRequestedOutputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens)
        $ view #outputsCovered s
    numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs
        = Set.size
        $ F.foldMap TokenBundle.getAssets
        $ view #changeGenerated s
    sizeOfRemainingUtxoSet
        = UTxOIndex.size $ utxoRemaining s

makeSelectionReportDetailed
    :: SelectionResult TokenBundle -> SelectionReportDetailed
makeSelectionReportDetailed s = SelectionReportDetailed
    { selectedInputs
        = F.toList $ view #inputsSelected s
    , requestedOutputs
        = view #outputsCovered s
    , generatedChangeOutputs
        = TokenBundle.Flat <$> view #changeGenerated s
    }

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionResult' value.
instance Buildable (SelectionResult TokenBundle) where
    build = build . makeSelectionReport

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionResult' value.
instance Buildable (SelectionResult TxOut) where
    build = build
        . makeSelectionReport
        . over #changeGenerated (fmap $ view #tokens)

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

    -- NOTE: We run the 'coinSelector' last, because we know that every input
    -- necessarily has a non-zero ada amount. By running the other selectors
    -- first, we increase the probability that the coin selector will be able
    -- to terminate without needing to select an additional coin.
    selectors :: [SelectionState -> m (Maybe SelectionState)]
    selectors =
        reverse (coinSelector : fmap assetSelector minimumAssetQuantities)
      where
        assetSelector = runSelectionStep .
            assetSelectionLens limit
        coinSelector = runSelectionStep $
            coinSelectionLens limit mExtraCoinSource minimumCoinQuantity

    (minimumCoinQuantity, minimumAssetQuantities) =
        TokenBundle.toFlatList minimumBalance

assetSelectionLens
    :: MonadRandom m
    => SelectionLimit
    -> (AssetId, TokenQuantity)
    -> SelectionLens m SelectionState
assetSelectionLens limit (asset, minimumAssetQuantity) = SelectionLens
    { currentQuantity = assetQuantity asset . selected
    , minimumQuantity = unTokenQuantity minimumAssetQuantity
    , selectQuantity = selectMatchingQuantity limit
        (WithAssetOnly asset :| [WithAsset asset])
    }

coinSelectionLens
    :: MonadRandom m
    => SelectionLimit
    -> Maybe Coin
    -- An extra source of ada.
    -> Coin
    -- ^ Minimum coin quantity.
    -> SelectionLens m SelectionState
coinSelectionLens limit mExtraCoinSource minimumCoinQuantity = SelectionLens
    { currentQuantity = \s -> coinQuantity (selected s) mExtraCoinSource
    , minimumQuantity = fromIntegral $ unCoin minimumCoinQuantity
    , selectQuantity  = selectMatchingQuantity limit
        (WithAdaOnly :| [Any])
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
    -> NonEmpty SelectionFilter
        -- ^ A list of selection filters to be traversed from left-to-right,
        -- in descending order of priority.
    -> SelectionState
        -- ^ The current selection state.
    -> m (Maybe SelectionState)
        -- ^ An updated selection state that includes a matching UTxO entry,
        -- or 'Nothing' if no such entry could be found.
selectMatchingQuantity limit filters s
    | limitReached =
        pure Nothing
    | otherwise =
        fmap updateState <$>
            UTxOIndex.selectRandomWithPriority (leftover s) filters
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
    , extraCoinSource :: Maybe Coin
        -- ^ An optional extra source of ada.
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
        OutputTokenBundleSizeExceedsLimit ->
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
        maybe (Left changeError) Right $ do
            adaAvailable <- excessCoin `subtractCoin` requiredCost
            assignCoinsToChangeMaps
                adaAvailable minCoinFor changeMapOutputCoinPairs
  where
    MakeChangeCriteria
        { minCoinFor
        , bundleSizeAssessor
        , requiredCost
        , extraCoinSource
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
                $ assessTokenBundleSize bundleSizeAssessor
                . flip TokenBundle.setCoin (maxBound @Coin)

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

    changeError :: UnableToConstructChangeError
    changeError = UnableToConstructChangeError
        { requiredCost
        , shortfall =
            -- This conversion is safe because we know that the distance is
            -- small-ish. If it wasn't, we would have have enough coins to
            -- construct the change.
            unsafeNaturalToCoin $ distance
                (coinToNatural excessCoin)
                (coinToNatural requiredCost + totalMinCoinValue)
            }
      where
        totalMinCoinValue = F.sum $ coinToNatural . minCoinFor <$>
            NE.zipWith (<>)
                changeForUserSpecifiedAssets
                changeForNonUserSpecifiedAssets

    outputMaps :: NonEmpty TokenMap
    outputMaps = view #tokens <$> outputBundles

    outputCoins :: NonEmpty Coin
    outputCoins = view #coin <$> outputBundles

    totalInputValue :: TokenBundle
    totalInputValue =
        F.fold inputBundles
            <> F.foldMap TokenBundle.fromCoin extraCoinSource
            -- Mints represent extra inputs from "the void"
            <> TokenBundle.fromTokenMap assetsToMint

    totalOutputValue :: TokenBundle
    totalOutputValue =
        F.fold outputBundles
            -- Burns represent extra outputs to "the void"
            <> TokenBundle.fromTokenMap assetsToBurn

    -- Identifiers of all user-specified assets: assets that were included in
    -- the original set of outputs.
    userSpecifiedAssetIds :: Set AssetId
    userSpecifiedAssetIds = TokenBundle.getAssets (F.fold outputBundles)

    -- Identifiers and quantities of all non-user-specified assets: assets that
    -- were not included in the orginal set of outputs, but that were
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
--    - returns 'Nothing' if (and only if) there was not enough ada available
--      to assign the minimum ada quantity to all non-empty change maps.
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
    -> Maybe [TokenBundle]
    -- ^ Resulting change bundles, or 'Nothing' if there was not enough ada
    -- available to assign a minimum ada quantity to all non-empty token maps.
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
            Just $ NE.toList $ NE.zipWith (<>)
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
            Just []

        _ ->
            -- We don't have enough ada available, and there are no empty token
            -- maps available to drop. We have to give up at this point.
            Nothing

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
-- It's also important to note that the change bundle calcualation requires
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
coinQuantity index extraSource =
    fromIntegral . unCoin . TokenBundle.getCoin $ fullBalance index extraSource

fullBalance :: UTxOIndex -> Maybe Coin -> TokenBundle
fullBalance index extraSource
    | UTxOIndex.null index =
        TokenBundle.empty
    | otherwise =
        view #balance index <> F.foldMap TokenBundle.fromCoin extraSource

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
        assetCount = Set.size . TokenMap.getAssets

newtype AssetCount a = AssetCount
    { unAssetCount :: a }
    deriving (Eq, Show)

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
