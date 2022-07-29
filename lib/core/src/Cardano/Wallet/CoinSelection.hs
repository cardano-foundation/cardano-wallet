{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
module Cardano.Wallet.CoinSelection
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
    , SelectionLimit
    , SelectionLimitOf (..)
    , SelectionOf (..)
    , SelectionParams (..)
    , SelectionStrategy (..)

    -- * Selection skeletons
    , SelectionSkeleton (..)
    , emptySkeleton

    -- * Selection errors
    , BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionCollateralError
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , UnableToConstructChangeError (..)

    -- * Selection reports
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , SelectionReportDetailed
    , SelectionReportSummarized

    -- * Selection deltas
    , balanceMissing
    , selectionDelta
    )
    where

import Cardano.Wallet.CoinSelection.Internal
    ( SelectionCollateralError
    , SelectionCollateralRequirement (..)
    , SelectionError (..)
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    )
import Cardano.Wallet.CoinSelection.Internal.Balance
    ( BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionLimit
    , SelectionLimitOf (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , balanceMissing
    )
import Cardano.Wallet.Primitive.Collateral
    ( asCollateral )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Constants
    ( minLengthAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( Flat (..), TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment
    , TxIn
    , TxOut (..)
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Control.Arrow
    ( (&&&) )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), genericF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import Prelude

import qualified Cardano.Wallet.CoinSelection.Internal as Internal
import qualified Cardano.Wallet.CoinSelection.Internal.Context as SC
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
        :: TxIn
    , address
        :: Address
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
    { assessTokenBundleSize
        :: TokenBundle -> TokenBundleSizeAssessment
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output. See documentation for
        -- the 'TokenBundleSizeAssessor' type to learn about the expected
        -- properties of this field.
    , certificateDepositAmount
        :: Coin
        -- ^ Amount that should be taken from/returned back to the wallet for
        -- each stake key registration/de-registration in the transaction.
    , computeMinimumAdaQuantity
        :: Address -> TokenMap -> Coin
        -- ^ Computes the minimum ada quantity required for a given output.
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , computeSelectionLimit
        :: [TxOut] -> SelectionLimit
        -- ^ Computes an upper bound for the number of ordinary inputs to
        -- select, given a current set of outputs.
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
        , computeSelectionLimit =
            computeSelectionLimit . fmap (uncurry TxOut)
        , maximumOutputAdaQuantity =
            txOutMaxCoin
        , maximumOutputTokenQuantity =
            txOutMaxTokenQuantity
        , minimumLengthChangeAddress =
            minLengthAddress
        , ..
        }

--------------------------------------------------------------------------------
-- Selection parameters
--------------------------------------------------------------------------------

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams = SelectionParams
    { assetsToBurn
        :: !TokenMap
        -- ^ Specifies a set of assets to burn.
    , assetsToMint
        :: !TokenMap
        -- ^ Specifies a set of assets to mint.
    , extraCoinIn
        :: !Coin
       -- ^ Specifies extra 'Coin' in.
    , extraCoinOut
        :: !Coin
        -- ^ Specifies extra 'Coin' out.
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

--------------------------------------------------------------------------------
-- Selection deltas
--------------------------------------------------------------------------------

-- | Computes the ada surplus of a selection, assuming there is a surplus.
--
selectionDelta
    :: (change -> Coin)
    -- ^ A function to extract the coin value from a change value.
    -> SelectionOf change
    -> Coin
selectionDelta getChangeCoin
    = Internal.selectionSurplusCoin
    . toInternalSelection (TokenBundle.fromCoin . getChangeCoin)

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
    , adaBalanceOfSelectedInputs :: Coin
    , adaBalanceOfExtraCoinSource :: Coin
    , adaBalanceOfExtraCoinSink :: Coin
    , adaBalanceOfRequestedOutputs :: Coin
    , adaBalanceOfGeneratedChangeOutputs :: Coin
    , numberOfSelectedInputs :: Int
    , numberOfSelectedCollateralInputs :: Int
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
    , selectedCollateral :: [(TxIn, TxOut)]
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

makeSelectionReport :: Selection -> SelectionReport
makeSelectionReport s = SelectionReport
    { summary = makeSelectionReportSummarized s
    , detail = makeSelectionReportDetailed s
    }

makeSelectionReportSummarized :: Selection -> SelectionReportSummarized
makeSelectionReportSummarized s = SelectionReportSummarized {..}
  where
    computedFee
        = selectionDelta TokenBundle.getCoin s
    adaBalanceOfSelectedInputs
        = F.foldMap (view (#tokens . #coin) . snd) $ view #inputs s
    adaBalanceOfExtraCoinSource
        = view #extraCoinSource s
    adaBalanceOfExtraCoinSink
        = view #extraCoinSink s
    adaBalanceOfGeneratedChangeOutputs
        = F.foldMap (view #coin) $ view #change s
    adaBalanceOfRequestedOutputs
        = F.foldMap (view (#tokens . #coin)) $ view #outputs s
    numberOfSelectedInputs
        = length $ view #inputs s
    numberOfSelectedCollateralInputs
        = length $ view #collateral s
    numberOfRequestedOutputs
        = length $ view #outputs s
    numberOfGeneratedChangeOutputs
        = length $ view #change s
    numberOfUniqueNonAdaAssetsInSelectedInputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens . snd)
        $ view #inputs s
    numberOfUniqueNonAdaAssetsInRequestedOutputs
        = Set.size
        $ F.foldMap (TokenBundle.getAssets . view #tokens)
        $ view #outputs s
    numberOfUniqueNonAdaAssetsInGeneratedChangeOutputs
        = Set.size
        $ F.foldMap TokenBundle.getAssets
        $ view #change s

makeSelectionReportDetailed :: Selection -> SelectionReportDetailed
makeSelectionReportDetailed s = SelectionReportDetailed
    { selectedInputs
        = F.toList $ view #inputs s
    , selectedCollateral
        = F.toList $ view #collateral s
    , requestedOutputs
        = view #outputs s
    , generatedChangeOutputs
        = TokenBundle.Flat <$> view #change s
    }

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionOf TokenBundle' value.
instance Buildable (SelectionOf TokenBundle) where
    build = build . makeSelectionReport

-- A convenience instance for 'Buildable' contexts that include a nested
-- 'SelectionOf TxOut' value.
instance Buildable (SelectionOf TxOut) where
    build = build
        . makeSelectionReport
        . over #change (fmap $ view #tokens)
