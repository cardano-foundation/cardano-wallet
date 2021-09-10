{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a high-level interface for coin selection in a Cardano
-- wallet.
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
    ( runWalletCoinSelection
    , SelectionConstraints (..)
    , SelectionParams (..)

    , validateTxOutsForSelection
    , ErrWalletSelection (..)
    , ErrPrepareOutputs (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)

    -- * Internals
    , makeWrapper
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( PerformSelection (..)
    , SelectionCriteria (..)
    , SelectionError
    , SelectionLimit
    , SelectionResult
    , SelectionSkeleton
    , performSelection
    , prepareOutputsWith
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin, scaleCoin, subtractCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxOut (..)
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad
    ( (<=<) )
import Control.Monad.Random.Class
    ( MonadRandom )
import Control.Monad.Trans.Except
    ( ExceptT (..), withExceptT )
import Data.Generics.Internal.VL.Lens
    ( over, set, view )
import Data.Generics.Labels
    ()
import Data.List
    ( foldl', partition )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word16 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

-- | Performs coin selection for a Cardano transaction.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
runWalletCoinSelection
    :: (HasCallStack, MonadRandom m)
    => SelectionConstraints
    -> SelectionParams
    -> ExceptT ErrWalletSelection m (SelectionResult TokenBundle)
runWalletCoinSelection sc sp = do
    (postProcess, params) <- withExceptT ErrWalletSelectionOutputs $ ExceptT $
        pure $ makeWrapper sc sp
    withExceptT ErrWalletSelectionBalance $ ExceptT $
        postProcess <$> performSelection params

type SelectionReturn = Either SelectionError (SelectionResult TokenBundle)

makeWrapper
    :: HasCallStack
    => SelectionConstraints
    -> SelectionParams
    -> Either ErrPrepareOutputs (SelectionReturn -> SelectionReturn, PerformSelection)
makeWrapper sc@SelectionConstraints{..} SelectionParams{..} =
    (fmap repairOutputs,) . getArgs <$> prepareOutputs outputsToCover
  where
    -- TODO: [ADP-1037] Adjust coin selection and fee estimation to handle
    -- collateral inputs.
    --
    -- TODO: [ADP-1070] Adjust coin selection and fee estimation to handle
    -- pre-existing inputs.
    getArgs outputsToCover' = PerformSelection
        { computeMinimumAdaQuantity
        , computeMinimumCost = computeMinimumCostWithExtras
        , selectionCriteria = SelectionCriteria
            { outputsToCover = outputsToCover'
            , selectionLimit = computeSelectionLimit $ F.toList outputsToCover'
             , extraCoinSource = Just extraCoinSource
            , .. }
        , ..
        }

    plusDeposits :: Coin -> Natural -> Coin
    plusDeposits amt n = amt `addCoin` scaleCoin n depositAmount

    computeMinimumCostWithExtras tx =
        (computeMinimumCost tx `addCoin` extraCoinSink) `above` extraCoinSource

    extraCoinSource =
        rewardWithdrawals `plusDeposits` certificateDepositsReturned
    extraCoinSink = scaleCoin certificateDepositsTaken depositAmount

    extraValueSink = TokenBundle.fromCoin $
        extraCoinSink `above` extraCoinSource

    prepareOutputs :: [TxOut] -> Either ErrPrepareOutputs (NonEmpty TxOut)
    prepareOutputs = (validateTxOutsForSelection sc <=< ensureNonEmptyOutputs)
        . addExtraValueSinkToOutputs

    -- At present, the coin selection algorithm does not permit an empty output
    -- list, so validate for this precondition.
    ensureNonEmptyOutputs =
        maybe (Left ErrPrepareOutputsTxOutMissing) Right . NE.nonEmpty

    addExtraValueSinkToOutputs os
        | null os && extraValueSink /= mempty =
            (TxOut dummyAddress extraValueSink : os)
        | null os =
            (TxOut dummyAddress dummyValue : os)
        | otherwise = os
    dummyAddress = Address ""
    dummyValue = TokenBundle.fromCoin $ computeMinimumAdaQuantity mempty

    -- Post-process the coin selection result by removing any dummy outputs
    -- added by 'addExtraValueSink'.
    repairOutputs :: SelectionResult TokenBundle -> SelectionResult TokenBundle
    repairOutputs sel =
        over #changeGenerated (addToHead (surplus ds)) $
        set #outputsCovered outs sel
      where
        (ds, outs) = partition isDummy (view #outputsCovered sel)
        isDummy = ((== dummyAddress) . view #address)

        addToHead :: TokenBundle -> [TokenBundle] -> [TokenBundle]
        addToHead _ []    = []
        addToHead x (h:q) = TokenBundle.add x h : q

        surplus = foldl' (flip TokenBundle.unsafeSubtract) extraValueSink
            . map (view #tokens)

above :: Coin -> Coin -> Coin
above a b = fromMaybe (Coin 0) $ subtractCoin a b

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
    { bundleSizeAssessor
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
    , maximumCollateralInputCount
        :: Word16
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
    , depositAmount
        :: Coin
        -- ^ Amount that should be taken from/returned back to the wallet for
        -- each stake key registration/de-registration in the transaction.
    }

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams = SelectionParams
    { assetsToBurn
        :: !TokenMap
        -- ^ Specifies a set of assets to burn.
    , assetsToMint
        :: !TokenMap
        -- ^ Specifies a set of assets to mint.
    , rewardWithdrawals
        :: !Coin
        -- ^ Specifies the value of a withdrawal from a reward account.
    , certificateDepositsTaken
        :: !Natural
        -- ^ Number of deposits for stake key registrations.
    , certificateDepositsReturned
        :: !Natural
        -- ^ Number of deposits from stake key de-registrations.
    , outputsToCover
        :: ![TxOut]
        -- ^ Specifies a set of outputs that must be paid for.
    , utxoAvailable
        :: !UTxOIndex
        -- ^ Specifies the set of all available UTxO entries. The algorithm
        -- will choose entries from this set when selecting ordinary inputs
        -- and collateral inputs.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that coin selection failed, or a precondition to coin selection
-- failed.
data ErrWalletSelection
    = ErrWalletSelectionOutputs ErrPrepareOutputs
    | ErrWalletSelectionBalance SelectionError
    deriving (Eq, Show)

-- | Prepares the given user-specified outputs, ensuring that they are valid.
validateTxOutsForSelection
    :: SelectionConstraints
    -> NonEmpty TxOut
    -> Either ErrPrepareOutputs (NonEmpty TxOut)
validateTxOutsForSelection constraints outputsUnprepared
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
    | otherwise = pure outputsToCover
  where
    SelectionConstraints
        { bundleSizeAssessor
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
            assessSize = view #assessTokenBundleSize bundleSizeAssessor

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

    outputsToCover = prepareOutputsWith computeMinimumAdaQuantity
        outputsUnprepared

-- | Indicates a problem when preparing outputs for a coin selection.
--
data ErrPrepareOutputs
    = ErrPrepareOutputsTokenBundleSizeExceedsLimit
        ErrOutputTokenBundleSizeExceedsLimit
    | ErrPrepareOutputsTokenQuantityExceedsLimit
        ErrOutputTokenQuantityExceedsLimit
    | ErrPrepareOutputsTxOutMissing
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
