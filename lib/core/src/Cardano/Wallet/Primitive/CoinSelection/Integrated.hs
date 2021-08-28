module Cardano.Wallet.Primitive.CoinSelection.Integrated
    ( performSelection
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balanced
    ( SelectionCriteria
    , SelectionError
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
    ( TokenBundleSizeAssessor )
import Control.Monad.Random.Class
    ( MonadRandom )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balanced as Balanced

performSelection
    :: (HasCallStack, MonadRandom m)
    => (TokenMap -> Coin)
    -> (SelectionSkeleton -> Coin)
    -> TokenBundleSizeAssessor
    -> SelectionCriteria
    -> m (Either SelectionError (SelectionResult TokenBundle))
performSelection = Balanced.performSelection
