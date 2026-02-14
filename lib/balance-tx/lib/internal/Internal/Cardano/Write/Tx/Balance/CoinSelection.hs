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
module Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( -- * Selection contexts
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

      -- * Selection errors
    , BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionCollateralError (..)
    , UnableToConstructChangeError (..)

      -- * Type conversions between wallet and coin-selection types
    , toCSCoin
    , fromCSCoin
    , toCSTokenBundle
    , fromCSTokenBundle
    , toCSTokenMap
    , fromCSTokenMap
    , toCSAssetId
    , fromCSAssetId
    , toCSTokenQuantity
    , fromCSTokenQuantity
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
    ( TokenBundleSizeAssessor (..)
    )
import Cardano.CoinSelection.UTxOSelection
    ( UTxOSelection
    )
import Cardano.Wallet.Primitive.Collateral
    ( asCollateral
    )
import Control.Arrow
    ( first
    , (&&&)
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad.Random.Class
    ( MonadRandom (..)
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

import qualified Cardano.CoinSelection as Internal
import qualified Cardano.CoinSelection.Context as SC
import qualified Cardano.CoinSelection.Types.AssetId as CS
    ( AssetId (..)
    )
import qualified Cardano.CoinSelection.Types.AssetName as CS
    ( AssetName (..)
    )
import qualified Cardano.CoinSelection.Types.Coin as CS
    ( Coin (..)
    )
import qualified Cardano.CoinSelection.Types.Hash as CS
    ( Hash (..)
    )
import qualified Cardano.CoinSelection.Types.TokenBundle as CS
    ( TokenBundle (..)
    )
import qualified Cardano.CoinSelection.Types.TokenMap as CS
    ( TokenMap
    )
import qualified Cardano.CoinSelection.Types.TokenMap as CS.TokenMap
import qualified Cardano.CoinSelection.Types.TokenPolicyId as CS
    ( TokenPolicyId (..)
    )
import qualified Cardano.CoinSelection.Types.TokenQuantity as CS
    ( TokenQuantity (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Address as W
    ( Address (..)
    )
import qualified Cardano.Wallet.Primitive.Types.AssetId as W
    ( AssetId (..)
    )
import qualified Cardano.Wallet.Primitive.Types.AssetName as W
    ( AssetName (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Hash as W
    ( Hash (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
    ( TokenMap
    )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W.TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicyId as W
    ( TokenPolicyId (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
    ( TokenQuantity (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
    ( TxIn (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (..)
    )
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
    ( UTxO (..)
    )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Selection contexts
--------------------------------------------------------------------------------

-- | A selection context for the wallet.
data WalletSelectionContext

instance SC.SelectionContext WalletSelectionContext where
    type Address WalletSelectionContext = W.Address
    type UTxO WalletSelectionContext = WalletUTxO

--------------------------------------------------------------------------------
-- Mapping between external (wallet) and internal UTxO identifiers
--------------------------------------------------------------------------------

-- | A type of unique UTxO identifier for the wallet.
data WalletUTxO = WalletUTxO
    { txIn
        :: !W.TxIn
    , address
        :: !W.Address
    }
    deriving (Eq, Generic, Ord, Show)

instance Buildable WalletUTxO where
    build (WalletUTxO i a) = build i <> ":" <> build a

instance Buildable (WalletUTxO, W.TokenBundle) where
    build (u, b) = build u <> ":" <> build (W.TokenBundle.Flat b)

toExternalUTxO :: (WalletUTxO, W.TokenBundle) -> (W.TxIn, W.TxOut)
toExternalUTxO = toExternalUTxO' id

toExternalUTxOMap :: Map WalletUTxO W.TokenBundle -> W.UTxO
toExternalUTxOMap = W.UTxO . Map.fromList . fmap toExternalUTxO . Map.toList

toInternalUTxO :: (W.TxIn, W.TxOut) -> (WalletUTxO, W.TokenBundle)
toInternalUTxO = toInternalUTxO' id

toInternalUTxOMap :: W.UTxO -> Map WalletUTxO W.TokenBundle
toInternalUTxOMap = Map.fromList . fmap toInternalUTxO . Map.toList . W.unUTxO

toExternalUTxO'
    :: (b -> W.TokenBundle) -> (WalletUTxO, b) -> (W.TxIn, W.TxOut)
toExternalUTxO' f (WalletUTxO i a, b) = (i, W.TxOut a (f b))

toInternalUTxO'
    :: (W.TokenBundle -> b) -> (W.TxIn, W.TxOut) -> (WalletUTxO, b)
toInternalUTxO' f (i, W.TxOut a b) = (WalletUTxO i a, f b)

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
data SelectionConstraints = SelectionConstraints
    { tokenBundleSizeAssessor
        :: TokenBundleSizeAssessor
    -- ^ Assesses the size of a token bundle relative to the upper limit of
    -- what can be included in a transaction output.
    , computeMinimumAdaQuantity
        :: W.Address -> W.TokenMap -> W.Coin
    -- ^ Computes the minimum ada quantity required for a given output.
    , isBelowMinimumAdaQuantity
        :: W.Address -> W.TokenBundle -> Bool
    -- ^ Returns 'True' if the given 'TokenBundle' has a 'Coin' value that is
    -- below the minimum required.
    , computeMinimumCost
        :: SelectionSkeleton -> W.Coin
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
        :: W.Address
    }
    deriving (Generic)

toInternalSelectionConstraints
    :: SelectionConstraints
    -> Internal.SelectionConstraints WalletSelectionContext
toInternalSelectionConstraints SelectionConstraints{..} =
    Internal.SelectionConstraints
        { tokenBundleSizeAssessor =
            tokenBundleSizeAssessor
        , computeMinimumAdaQuantity = \addr tm ->
            toCSCoin $ computeMinimumAdaQuantity addr (fromCSTokenMap tm)
        , isBelowMinimumAdaQuantity = \addr tb ->
            isBelowMinimumAdaQuantity addr (fromCSTokenBundle tb)
        , computeMinimumCost =
            toCSCoin . computeMinimumCost . toExternalSelectionSkeleton
        , maximumOutputAdaQuantity =
            toCSCoin W.txOutMaxCoin
        , maximumOutputTokenQuantity =
            toCSTokenQuantity W.txOutMaxTokenQuantity
        , nullAddress =
            W.Address ""
        , maximumCollateralInputCount =
            maximumCollateralInputCount
        , minimumCollateralPercentage =
            minimumCollateralPercentage
        , maximumLengthChangeAddress =
            maximumLengthChangeAddress
        }

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

-- | Specifies all parameters that are specific to a given selection.
data SelectionParams = SelectionParams
    { extraValueIn
        :: !W.TokenBundle
    -- ^ Specifies extra value on the input side.
    , extraValueOut
        :: !W.TokenBundle
    -- ^ Specifies extra value on the output side.
    , outputsToCover
        :: ![W.TxOut]
    -- ^ Specifies a set of outputs that must be paid for.
    , collateralRequirement
        :: !SelectionCollateralRequirement
    -- ^ Specifies the collateral requirement for this selection.
    , utxoAvailableForCollateral
        :: !(Map WalletUTxO W.TokenBundle)
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
toInternalSelectionParams SelectionParams{..} =
    Internal.SelectionParams
        { assetsToBurn =
            toCSTokenMap assetsToBurn
        , assetsToMint =
            toCSTokenMap assetsToMint
        , extraCoinIn =
            toCSCoin extraCoinIn
        , extraCoinOut =
            toCSCoin extraCoinOut
        , utxoAvailableForCollateral =
            Map.mapMaybeWithKey
                identifyCollateral
                utxoAvailableForCollateral
        , outputsToCover =
            (\o -> (view #address o, toCSTokenBundle (view #tokens o)))
                <$> outputsToCover
        , collateralRequirement =
            collateralRequirement
        , utxoAvailableForInputs =
            utxoAvailableForInputs
        , selectionStrategy =
            selectionStrategy
        }
  where
    W.TokenBundle extraCoinIn assetsToMint = extraValueIn
    W.TokenBundle extraCoinOut assetsToBurn = extraValueOut

    identifyCollateral
        :: WalletUTxO -> W.TokenBundle -> Maybe CS.Coin
    identifyCollateral (WalletUTxO _ a) b =
        toCSCoin <$> asCollateral (W.TxOut a b)

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
data SelectionSkeleton = SelectionSkeleton
    { skeletonInputCount
        :: !Int
    , skeletonOutputs
        :: ![W.TxOut]
    , skeletonChange
        :: ![Set W.AssetId]
    }
    deriving (Eq, Generic, Show)

toExternalSelectionSkeleton
    :: Internal.SelectionSkeleton WalletSelectionContext
    -> SelectionSkeleton
toExternalSelectionSkeleton Internal.SelectionSkeleton{..} =
    SelectionSkeleton
        { skeletonOutputs =
            uncurry W.TxOut
                . fmap fromCSTokenBundle
                <$> skeletonOutputs
        , skeletonChange =
            Set.map fromCSAssetId <$> skeletonChange
        , ..
        }

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

-- | Represents a balanced selection.
data SelectionOf change = Selection
    { inputs
        :: !(NonEmpty (W.TxIn, W.TxOut))
    -- ^ Selected inputs.
    , collateral
        :: ![(W.TxIn, W.TxOut)]
    -- ^ Selected collateral inputs.
    , outputs
        :: ![W.TxOut]
    -- ^ User-specified outputs
    , change
        :: ![change]
    -- ^ Generated change outputs.
    , assetsToMint
        :: !W.TokenMap
    -- ^ Assets to mint.
    , assetsToBurn
        :: !W.TokenMap
    -- ^ Assets to burn.
    , extraCoinSource
        :: !W.Coin
    -- ^ An extra source of ada.
    , extraCoinSink
        :: !W.Coin
    -- ^ An extra sink for ada.
    }
    deriving (Generic, Eq, Show)

instance NFData change => NFData (SelectionOf change)

-- | The default type of selection.
--
-- In this type of selection, change values do not have addresses assigned.
type Selection = SelectionOf W.TokenBundle

toExternalSelection
    :: Internal.Selection WalletSelectionContext -> Selection
toExternalSelection Internal.Selection{..} =
    Selection
        { inputs =
            toExternalUTxO' fromCSTokenBundle <$> inputs
        , collateral =
            toExternalUTxO' (W.TokenBundle.fromCoin . fromCSCoin)
                <$> collateral
        , outputs =
            uncurry W.TxOut . fmap fromCSTokenBundle <$> outputs
        , change =
            fromCSTokenBundle <$> change
        , assetsToMint =
            fromCSTokenMap assetsToMint
        , assetsToBurn =
            fromCSTokenMap assetsToBurn
        , extraCoinSource =
            fromCSCoin extraCoinSource
        , extraCoinSink =
            fromCSCoin extraCoinSink
        }

toInternalSelection
    :: (change -> W.TokenBundle)
    -> SelectionOf change
    -> Internal.Selection WalletSelectionContext
toInternalSelection getChangeBundle Selection{..} =
    Internal.Selection
        { change =
            toCSTokenBundle . getChangeBundle
                <$> change
        , collateral =
            toInternalUTxO'
                (toCSCoin . W.TokenBundle.getCoin)
                <$> collateral
        , inputs =
            toInternalUTxO' toCSTokenBundle
                <$> inputs
        , outputs =
            (view #address &&& toCSTokenBundle . view #tokens)
                <$> outputs
        , assetsToMint = toCSTokenMap assetsToMint
        , assetsToBurn = toCSTokenMap assetsToBurn
        , extraCoinSource = toCSCoin extraCoinSource
        , extraCoinSink = toCSCoin extraCoinSink
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
performSelection
    :: forall m
     . (HasCallStack, MonadRandom m)
    => SelectionConstraints
    -> SelectionParams
    -> ExceptT (SelectionError WalletSelectionContext) m Selection
performSelection cs ps =
    toExternalSelection
        <$> Internal.performSelection @m @WalletSelectionContext
            (toInternalSelectionConstraints cs)
            (toInternalSelectionParams ps)

--------------------------------------------------------------------------------
-- Type conversions between wallet types and coin-selection types
--------------------------------------------------------------------------------

toCSCoin :: W.Coin -> CS.Coin
toCSCoin (W.Coin n) = CS.Coin n

fromCSCoin :: CS.Coin -> W.Coin
fromCSCoin (CS.Coin n) = W.Coin n

toCSTokenQuantity :: W.TokenQuantity -> CS.TokenQuantity
toCSTokenQuantity (W.TokenQuantity n) = CS.TokenQuantity n

fromCSTokenQuantity :: CS.TokenQuantity -> W.TokenQuantity
fromCSTokenQuantity (CS.TokenQuantity n) = W.TokenQuantity n

toCSAssetId :: W.AssetId -> CS.AssetId
toCSAssetId (W.AssetId p a) = CS.AssetId (toCSPolicyId p) (toCSAssetName a)
  where
    toCSPolicyId (W.UnsafeTokenPolicyId (W.Hash h)) =
        CS.UnsafeTokenPolicyId (CS.Hash h)
    toCSAssetName (W.UnsafeAssetName n) =
        CS.UnsafeAssetName n

fromCSAssetId :: CS.AssetId -> W.AssetId
fromCSAssetId (CS.AssetId p a) =
    W.AssetId (fromCSPolicyId p) (fromCSAssetName a)
  where
    fromCSPolicyId (CS.UnsafeTokenPolicyId (CS.Hash h)) =
        W.UnsafeTokenPolicyId (W.Hash h)
    fromCSAssetName (CS.UnsafeAssetName n) =
        W.UnsafeAssetName n

toCSTokenMap :: W.TokenMap -> CS.TokenMap
toCSTokenMap =
    CS.TokenMap.fromFlatList
        . fmap (first toCSAssetId . fmap toCSTokenQuantity)
        . W.TokenMap.toFlatList

fromCSTokenMap :: CS.TokenMap -> W.TokenMap
fromCSTokenMap =
    W.TokenMap.fromFlatList
        . fmap (first fromCSAssetId . fmap fromCSTokenQuantity)
        . CS.TokenMap.toFlatList

toCSTokenBundle :: W.TokenBundle -> CS.TokenBundle
toCSTokenBundle (W.TokenBundle c tm) =
    CS.TokenBundle (toCSCoin c) (toCSTokenMap tm)

fromCSTokenBundle :: CS.TokenBundle -> W.TokenBundle
fromCSTokenBundle (CS.TokenBundle c tm) =
    W.TokenBundle (fromCSCoin c) (fromCSTokenMap tm)
