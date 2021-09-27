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
    ( performSelection
    , SelectionConstraints (..)
    , SelectionParams (..)
    , SelectionError (..)

    , prepareOutputs
    , ErrPrepareOutputs (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)

    -- * Reporting
    , SelectionReport (..)
    , SelectionReportSummarized (..)
    , SelectionReportDetailed (..)
    , makeSelectionReport
    , makeSelectionReportSummarized
    , makeSelectionReportDetailed
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionResult, SelectionSkeleton )
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
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxIn
    , TxOut
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad.Random.Class
    ( MonadRandom )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, withExceptT )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.Semigroup
    ( mtimesDefault )
import Data.Word
    ( Word16 )
import Fmt
    ( Buildable (..), genericF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.Set as Set

-- | Performs a coin selection.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
performSelection
    :: (HasCallStack, MonadRandom m)
    => SelectionConstraints
    -> SelectionParams
    -> ExceptT SelectionError m (SelectionResult TokenBundle)
performSelection constraints params = do
    -- TODO:
    --
    -- https://input-output.atlassian.net/browse/ADP-1037
    -- Adjust coin selection and fee estimation to handle collateral inputs
    --
    -- https://input-output.atlassian.net/browse/ADP-1070
    -- Adjust coin selection and fee estimation to handle pre-existing inputs
    --
    preparedOutputs <- withExceptT SelectionOutputsError $ except
        $ prepareOutputs constraints (view #outputsToCover params)
    withExceptT SelectionBalanceError $ ExceptT
        $ uncurry Balance.performSelection
        $ toBalanceConstraintsParams
            ( constraints
            , params {outputsToCover = preparedOutputs}
            )

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
        , computeSelectionLimit =
            view #computeSelectionLimit constraints
        , assessTokenBundleSize =
            view (#assessTokenBundleSize . #assessTokenBundleSize) constraints
        }
    balanceParams = Balance.SelectionParams
        { assetsToBurn =
            view #assetsToBurn params
        , assetsToMint =
            view #assetsToMint params
        , extraCoinSource =
            view #rewardWithdrawal params <>
            mtimesDefault
                (view #certificateDepositsReturned params)
                (view #depositAmount constraints)
        , extraCoinSink =
            mtimesDefault
                (view #certificateDepositsTaken params)
                (view #depositAmount constraints)
        , outputsToCover =
            view #outputsToCover params
        , utxoAvailable =
            view #utxoAvailable params
        }

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
        :: TokenBundleSizeAssessor
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output. See documentation for
        -- the 'TokenBundleSizeAssessor' type to learn about the expected
        -- properties of this field.
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
    , depositAmount
        :: Coin
        -- ^ Amount that should be taken from/returned back to the wallet for
        -- each stake key registration/de-registration in the transaction.
    , maximumCollateralInputCount
        :: Word16
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
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
    , utxoAvailable
        :: !UTxOIndex
        -- ^ Specifies the set of all available UTxO entries. The algorithm
        -- will choose entries from this set when selecting ordinary inputs
        -- and collateral inputs.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that an error occurred while performing a coin selection.
--
data SelectionError
    = SelectionBalanceError Balance.SelectionError
    | SelectionOutputsError ErrPrepareOutputs
    deriving (Eq, Show)

-- | Prepares the given user-specified outputs, ensuring that they are valid.
--
prepareOutputs
    :: SelectionConstraints
    -> [TxOut]
    -> Either ErrPrepareOutputs [TxOut]
prepareOutputs constraints outputsUnprepared
    | (address, assetCount) : _ <- excessivelyLargeBundles =
        Left $
            -- We encountered one or more excessively large token bundles.
            -- Just report the first such bundle:
            ErrPrepareOutputsTokenBundleSizeExceedsLimit $
            ErrOutputTokenBundleSizeExceedsLimit {address, assetCount}
    | (address, asset, quantity) : _ <- excessiveTokenQuantities =
        Left $
            -- We encountered one or more excessive token quantities.
            -- Just report the first such quantity:
            ErrPrepareOutputsTokenQuantityExceedsLimit $
            ErrOutputTokenQuantityExceedsLimit
                { address
                , asset
                , quantity
                , quantityMaxBound = txOutMaxTokenQuantity
                }
    | otherwise =
        pure outputsToCover
  where
    SelectionConstraints
        { assessTokenBundleSize
        , computeMinimumAdaQuantity
        } = constraints

    -- The complete list of token bundles whose serialized lengths are greater
    -- than the limit of what is allowed in a transaction output:
    excessivelyLargeBundles :: [(Address, Int)]
    excessivelyLargeBundles =
        [ (address, assetCount)
        | output <- F.toList outputsToCover
        , let bundle = view #tokens output
        , bundleIsExcessivelyLarge bundle
        , let address = view #address output
        , let assetCount = Set.size $ TokenBundle.getAssets bundle
        ]

      where
        bundleIsExcessivelyLarge b = case assessSize b of
            TokenBundleSizeWithinLimit -> False
            OutputTokenBundleSizeExceedsLimit -> True
          where
            assessSize = view #assessTokenBundleSize assessTokenBundleSize

    -- The complete list of token quantities that exceed the maximum quantity
    -- allowed in a transaction output:
    excessiveTokenQuantities :: [(Address, AssetId, TokenQuantity)]
    excessiveTokenQuantities =
        [ (address, asset, quantity)
        | output <- F.toList outputsToCover
        , let address = view #address output
        , (asset, quantity) <-
            TokenMap.toFlatList $ view #tokens $ view #tokens output
        , quantity > txOutMaxTokenQuantity
        ]

    outputsToCover =
        Balance.prepareOutputsWith computeMinimumAdaQuantity outputsUnprepared

-- | Indicates a problem when preparing outputs for a coin selection.
--
data ErrPrepareOutputs
    = ErrPrepareOutputsTokenBundleSizeExceedsLimit
        ErrOutputTokenBundleSizeExceedsLimit
    | ErrPrepareOutputsTokenQuantityExceedsLimit
        ErrOutputTokenQuantityExceedsLimit
    deriving (Eq, Generic, Show)

data ErrOutputTokenBundleSizeExceedsLimit = ErrOutputTokenBundleSizeExceedsLimit
    { address :: !Address
      -- ^ The address to which this token bundle was to be sent.
    , assetCount :: !Int
      -- ^ The number of assets within the token bundle.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that a token quantity exceeds the maximum quantity that can
--   appear in a transaction output's token bundle.
--
data ErrOutputTokenQuantityExceedsLimit = ErrOutputTokenQuantityExceedsLimit
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
    , adaBalanceOfExtraCoinSource :: Coin
    , adaBalanceOfExtraCoinSink :: Coin
    , adaBalanceOfRequestedOutputs :: Coin
    , adaBalanceOfGeneratedChangeOutputs :: Coin
    , numberOfSelectedInputs :: Int
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
    :: Balance.SelectionResult TokenBundle -> SelectionReport
makeSelectionReport s = SelectionReport
    { summary = makeSelectionReportSummarized s
    , detail = makeSelectionReportDetailed s
    }

makeSelectionReportSummarized
    :: Balance.SelectionResult TokenBundle -> SelectionReportSummarized
makeSelectionReportSummarized s = SelectionReportSummarized {..}
  where
    computedFee
        = Coin.distance totalAdaBalanceIn totalAdaBalanceOut
    totalAdaBalanceIn
        = adaBalanceOfSelectedInputs <> adaBalanceOfExtraCoinSource
    totalAdaBalanceOut
        = adaBalanceOfGeneratedChangeOutputs <> adaBalanceOfRequestedOutputs
    adaBalanceOfSelectedInputs
        = F.foldMap (view (#tokens . #coin) . snd) $ view #inputsSelected s
    adaBalanceOfExtraCoinSource
        = view #extraCoinSource s
    adaBalanceOfExtraCoinSink
        = view #extraCoinSink s
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
