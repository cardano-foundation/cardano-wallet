{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Primitive.CoinSelection.Integrated
    ( performSelection
    , SelectionConstraints (..)
    , SelectionData (..)
    , SelectionError (..)

    , prepareOutputs
    , ErrPrepareOutputs (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balanced
    ( SelectionCriteria (..)
    , SelectionLimit
    , SelectionResult
    , SelectionSkeleton
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxOut
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad.Random.Class
    ( MonadRandom )
import Data.Bifunctor
    ( first )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balanced as Balanced
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.Set as Set

performSelection
    :: (HasCallStack, MonadRandom m)
    => SelectionConstraints
    -> SelectionData
    -> m (Either SelectionError (SelectionResult TokenBundle))
performSelection selectionConstraints selectionData =
    first SelectionBalanceError <$>
    Balanced.performSelection
        computeMinimumAdaQuantity
        computeMinimumCost
        assessTokenBundleSize
        selectionCriteria
  where
    selectionCriteria = SelectionCriteria
        { assetsToBurn
        , assetsToMint
        , extraCoinSource = rewardWithdrawal
        , outputsToCover
        , selectionLimit
        , utxoAvailable
        }
    SelectionConstraints
        { assessTokenBundleSize
        , computeMinimumAdaQuantity
        , computeMinimumCost
        , selectionLimit
        } = selectionConstraints
    SelectionData
        { assetsToBurn
        , assetsToMint
        , outputsToCover
        , rewardWithdrawal
        , utxoAvailable
        } = selectionData

data SelectionConstraints = SelectionConstraints
    { assessTokenBundleSize
        :: TokenBundleSizeAssessor
    , computeMinimumAdaQuantity
        :: TokenMap -> Coin
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
    , selectionLimit
        :: SelectionLimit
    }

data SelectionData = SelectionData
    { assetsToBurn
        :: !TokenMap
    , assetsToMint
        :: !TokenMap
    , outputsToCover
        :: !(NonEmpty TxOut)
    , rewardWithdrawal
        :: !(Maybe Coin)
    , utxoAvailable
        :: !UTxOIndex
    }
    deriving (Eq, Generic, Show)

data SelectionError
    = SelectionBalanceError Balanced.SelectionError
    | SelectionOutputsError ErrPrepareOutputs
    deriving (Eq, Show)

prepareOutputs
    :: SelectionConstraints
    -> NonEmpty TxOut
    -> Either ErrPrepareOutputs (NonEmpty TxOut)
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
        Balanced.prepareOutputsWith computeMinimumAdaQuantity outputsUnprepared

-- | Indicates a problem when preparing outputs for a coin selection.
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
