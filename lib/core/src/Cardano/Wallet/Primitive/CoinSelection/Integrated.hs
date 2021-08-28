{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Primitive.CoinSelection.Integrated
    ( performSelection
    , SelectionData (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balanced
    ( SelectionCriteria (..)
    , SelectionError
    , SelectionLimit
    , SelectionResult
    , SelectionSkeleton
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessor, TxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad.Random.Class
    ( MonadRandom )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balanced as Balanced

performSelection
    :: (HasCallStack, MonadRandom m)
    => (TokenMap -> Coin)
    -> (SelectionSkeleton -> Coin)
    -> TokenBundleSizeAssessor
    -> SelectionLimit
    -> SelectionData
    -> m (Either SelectionError (SelectionResult TokenBundle))
performSelection
    computeMinimumAdaQuantity
    computeMinimumCost
    assessTokenBundleSize
    selectionLimit
    selectionData =
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
    SelectionData
        { assetsToBurn
        , assetsToMint
        , outputsToCover
        , rewardWithdrawal
        , utxoAvailable
        } = selectionData

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
