{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides a wallet-specific interface for coin selection.
--
-- Coin selection handles the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- Use the 'performSelection' function to perform a coin selection.
--
module Cardano.Tx.Balance.Internal.CoinSelection
    (
    -- * Selection contexts
      WalletSelectionContext
    , WalletUTxO (..)

    -- * Mapping between external (wallet) types and internal types
    , toExternalUTxO
    , toExternalUTxOMap
    , toInternalUTxO
    , toInternalUTxOMap

    -- * Mapping between external (wallet) selections and internal selections.
    , toExternalSelection
    , toInternalSelection

    -- * Performing selections
    , performSelection
    , Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionParams (..)
    , SelectionStrategy (..)

    -- * Selection skeletons
    , SelectionSkeleton (..)
    , emptySkeleton

    -- * Selection errors
    , BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionCollateralError (..)
    , UnableToConstructChangeError (..)
    )
    where

import Cardano.CoinSelection
    ( SelectionCollateralError (..)
    , SelectionCollateralRequirement (..)
    , SelectionError (..)
    )
import Cardano.CoinSelection.Balance
    ( BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    )
import Cardano.CoinSelection.Size
    ( TokenBundleSizeAssessor (..) )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Primitive.Collateral
    ( asCollateral )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin, txOutMaxTokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Arrow
    ( (&&&) )
import Control.DeepSeq
    ( NFData )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import Prelude

import qualified Cardano.CoinSelection as Internal
import qualified Cardano.CoinSelection.Context as SC
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Selection contexts
--------------------------------------------------------------------------------

-- | A selection context for the wallet.
--
data WalletSelectionContext

instance SC.SelectionContext WalletSelectionContext where
    type Address WalletSelectionContext = Address
    type UTxO WalletSelectionContext = WalletUTxO

--------------------------------------------------------------------------------
-- Mapping between external (wallet) and internal UTxO identifiers
--------------------------------------------------------------------------------

-- | A type of unique UTxO identifier for the wallet.
--
data WalletUTxO = WalletUTxO
    { txIn
        :: !TxIn
    , address
        :: !Address
    }
    deriving (Eq, Generic, Ord, Show)

instance Buildable WalletUTxO where
    build (WalletUTxO i a) = build i <> ":" <> build a

instance Buildable (WalletUTxO, TokenBundle) where
    build (u, b) = build u <> ":" <> build (Flat b)

toExternalUTxO :: (WalletUTxO, TokenBundle) -> (TxIn, TxOut)
toExternalUTxO = toExternalUTxO' id

toExternalUTxOMap :: Map WalletUTxO TokenBundle -> UTxO
toExternalUTxOMap = UTxO . Map.fromList . fmap toExternalUTxO . Map.toList

toInternalUTxO :: (TxIn, TxOut) -> (WalletUTxO, TokenBundle)
toInternalUTxO = toInternalUTxO' id

toInternalUTxOMap :: UTxO -> Map WalletUTxO TokenBundle
toInternalUTxOMap = Map.fromList . fmap toInternalUTxO . Map.toList . unUTxO

toExternalUTxO' :: (b -> TokenBundle) -> (WalletUTxO, b) -> (TxIn, TxOut)
toExternalUTxO' f (WalletUTxO i a, b) = (i, TxOut a (f b))

toInternalUTxO' :: (TokenBundle -> b) -> (TxIn, TxOut) -> (WalletUTxO, b)
toInternalUTxO' f (i, TxOut a b) = (WalletUTxO i a, f b)

--------------------------------------------------------------------------------
-- Selection constraints
--------------------------------------------------------------------------------

-- | Specifies all constraints required for coin selection.
--
-- Selection constraints:
--
--    - are dependent on the current set of protocol parameters.
--
--    - are not specific to a given selection.
--
--    - place limits on the coin selection algorithm, enabling it to produce
--      selections that are acceptable to the ledger.
--
data SelectionConstraints = SelectionConstraints
    { tokenBundleSizeAssessor
        :: TokenBundleSizeAssessor
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output.
    , computeMinimumAdaQuantity
        :: Address -> TokenMap -> Coin
        -- ^ Computes the minimum ada quantity required for a given output.
    , isBelowMinimumAdaQuantity
        :: Address -> TokenBundle -> Bool
      -- ^ Returns 'True' if the given 'TokenBundle' has a 'Coin' value that is
      -- below the minimum required.
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , maximumCollateralInputCount
        :: Int
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
    , minimumCollateralPercentage
        :: Natural
        -- ^ Specifies the minimum required amount of collateral as a
        -- percentage of the total transaction fee.
    , maximumLengthChangeAddress
        :: Address
    }
    deriving Generic

toInternalSelectionConstraints
    :: SelectionConstraints
    -> Internal.SelectionConstraints WalletSelectionContext
toInternalSelectionConstraints SelectionConstraints {..} =
    Internal.SelectionConstraints
        { computeMinimumCost =
            computeMinimumCost . toExternalSelectionSkeleton
        , maximumOutputAdaQuantity =
            txOutMaxCoin
        , maximumOutputTokenQuantity =
            txOutMaxTokenQuantity
        , nullAddress =
            Address ""
        , ..
        }

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams = SelectionParams
    { extraValueIn
        :: !TokenBundle
        -- ^ Specifies extra value on the input side.
    , extraValueOut
        :: !TokenBundle
        -- ^ Specifies extra value on the output side.
    , outputsToCover
        :: ![TxOut]
        -- ^ Specifies a set of outputs that must be paid for.
    , collateralRequirement
        :: !SelectionCollateralRequirement
        -- ^ Specifies the collateral requirement for this selection.
    , utxoAvailableForCollateral
        :: !(Map WalletUTxO TokenBundle)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- collateral inputs.
        --
        -- This set is allowed to intersect with 'utxoAvailableForInputs',
        -- since the ledger does not require that these sets are disjoint.
    , utxoAvailableForInputs
        :: !(UTxOSelection WalletUTxO)
        -- ^ Specifies a set of UTxOs that are available for selection as
        -- ordinary inputs and optionally, a subset that has already been
        -- selected.
        --
        -- Further entries from this set will be selected to cover any deficit.
    , selectionStrategy
        :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving (Eq, Generic, Show)

toInternalSelectionParams
    :: SelectionParams
    -> Internal.SelectionParams WalletSelectionContext
toInternalSelectionParams SelectionParams {..} =
    Internal.SelectionParams
        { utxoAvailableForCollateral =
            Map.mapMaybeWithKey identifyCollateral utxoAvailableForCollateral
        , outputsToCover =
            (view #address &&& view #tokens) <$> outputsToCover
        , ..
        }
  where
    TokenBundle extraCoinIn  assetsToMint = extraValueIn
    TokenBundle extraCoinOut assetsToBurn = extraValueOut

    identifyCollateral :: WalletUTxO -> TokenBundle -> Maybe Coin
    identifyCollateral (WalletUTxO _ a) b = asCollateral (TxOut a b)

--------------------------------------------------------------------------------
-- Selection skeletons
--------------------------------------------------------------------------------

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

-- | Creates an empty 'SelectionSkeleton'.
--
emptySkeleton :: SelectionSkeleton
emptySkeleton = SelectionSkeleton
    { skeletonInputCount = 0
    , skeletonOutputs = mempty
    , skeletonChange = mempty
    }

toExternalSelectionSkeleton
    :: Internal.SelectionSkeleton WalletSelectionContext
    -> SelectionSkeleton
toExternalSelectionSkeleton Internal.SelectionSkeleton {..} =
    SelectionSkeleton
        { skeletonOutputs =
            uncurry TxOut <$> skeletonOutputs
        , ..
        }

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

-- | Represents a balanced selection.
--
data SelectionOf change = Selection
    { inputs
        :: !(NonEmpty (TxIn, TxOut))
        -- ^ Selected inputs.
    , collateral
        :: ![(TxIn, TxOut)]
        -- ^ Selected collateral inputs.
    , outputs
        :: ![TxOut]
        -- ^ User-specified outputs
    , change
        :: ![change]
        -- ^ Generated change outputs.
    , assetsToMint
        :: !TokenMap
        -- ^ Assets to mint.
    , assetsToBurn
        :: !TokenMap
        -- ^ Assets to burn.
    , extraCoinSource
        :: !Coin
        -- ^ An extra source of ada.
    , extraCoinSink
        :: !Coin
        -- ^ An extra sink for ada.
    }
    deriving (Generic, Eq, Show)

instance NFData change => NFData (SelectionOf change)

-- | The default type of selection.
--
-- In this type of selection, change values do not have addresses assigned.
--
type Selection = SelectionOf TokenBundle

toExternalSelection :: Internal.Selection WalletSelectionContext -> Selection
toExternalSelection Internal.Selection {..} =
    Selection
        { collateral = toExternalUTxO' TokenBundle.fromCoin
            <$> collateral
        , inputs = toExternalUTxO
            <$> inputs
        , outputs = uncurry TxOut
            <$> outputs
        , ..
        }

toInternalSelection
    :: (change -> TokenBundle)
    -> SelectionOf change
    -> Internal.Selection WalletSelectionContext
toInternalSelection getChangeBundle Selection {..} =
    Internal.Selection
        { change = getChangeBundle
            <$> change
        , collateral = toInternalUTxO' TokenBundle.getCoin
            <$> collateral
        , inputs = toInternalUTxO
            <$> inputs
        , outputs = (view #address &&& view #tokens)
            <$> outputs
        , ..
        }

--------------------------------------------------------------------------------
-- Performing a selection
--------------------------------------------------------------------------------

-- | Performs a coin selection.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- See 'Internal.performSelection' for more details.
--
performSelection
    :: forall m. (HasCallStack, MonadRandom m)
    => SelectionConstraints
    -> SelectionParams
    -> ExceptT (SelectionError WalletSelectionContext) m Selection
performSelection cs ps =
    toExternalSelection <$>
    Internal.performSelection @m @WalletSelectionContext
        (toInternalSelectionConstraints cs)
        (toInternalSelectionParams ps)
