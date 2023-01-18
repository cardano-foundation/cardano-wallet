{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides generators and shrinkers for the 'TxSeq' type.
--
-- Usage:
--
--  - Use 'genTxSeq' to generate a 'ShrinkableTxSeq' value.
--  - Use 'getTxSeq' to extract a 'TxSeq' from a 'ShrinkableTxSeq'.
--  - Use 'shrinkTxSeq' to shrink a 'ShrinkableTxSeq' value.
--
module Cardano.Wallet.Primitive.Types.Tx.TxSeq.Gen
    (
    -- * Public interface
      ShrinkableTxSeq
    , genTxSeq
    , getTxSeq
    , shrinkTxSeq

    -- * Internal types and functions (exported for testing)
    , ShrinkState (..)
    , ShrinkPhase (..)
    , ShrinkAction (..)
    , getShrinkPhase
    , getShrinkState
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( chooseCoin, genCoinPositive )
import Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( genRewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundlePartitionNonNull )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxScriptValidity (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( TxWithoutId (..), txWithoutIdToTx )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxSeq
    ( TxSeq )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( selectUTxOEntries )
import Control.Monad.Util
    ( applyNM )
import Data.Function
    ( on )
import Data.Maybe
    ( fromMaybe, listToMaybe, mapMaybe )
import Test.QuickCheck
    ( Gen, chooseInt, elements, frequency, sized, vectorOf )
import Test.QuickCheck.Extra
    ( genMapWith )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.TxSeq as TxSeq
import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

-- | A transaction sequence with extra state to record shrinking progress.
--
data ShrinkableTxSeq = ShrinkableTxSeq
    { shrinkState
        :: !ShrinkState
    , txSeq
        :: !TxSeq
    }
    deriving (Eq, Show)

instance Ord ShrinkableTxSeq where
    compare = compare `on` show

-- | Generates a shrinkable transaction sequence.
--
genTxSeq :: Gen UTxO -> Gen Address -> Gen ShrinkableTxSeq
genTxSeq genUTxO genAddr = fmap toShrinkable $ sized $ \size ->
    applyNM size extendTxSeq . TxSeq.fromUTxO =<< genUTxO
  where
    extendTxSeq :: TxSeq -> Gen TxSeq
    extendTxSeq s = frequency
        [ (1, appendTxGroupBoundary)
        , (4, appendTx)
        ]
      where
        appendTxGroupBoundary =
            pure $ TxSeq.appendTxGroupBoundary s
        appendTx =
            fromMaybe reportFailure . flip TxSeq.appendTx s
                <$> genTxFromUTxO genAddr (TxSeq.lastUTxO s)
          where
            reportFailure =
                error "genTxSeq: Unable to generate a valid transaction"

    toShrinkable :: TxSeq -> ShrinkableTxSeq
    toShrinkable s = ShrinkableTxSeq (initialShrinkState s) s

-- | Extracts an ordinary 'TxSeq' from a 'ShrinkableTxSeq'.
--
getTxSeq :: ShrinkableTxSeq -> TxSeq
getTxSeq = txSeq

-- | Shrinks a transaction sequence.
--
shrinkTxSeq :: ShrinkableTxSeq -> [ShrinkableTxSeq]
shrinkTxSeq ShrinkableTxSeq {shrinkState, txSeq} =
    mapMaybe toShrinkable (applyShrinkStateAction shrinkState txSeq <> [txSeq])
  where
    toShrinkable :: TxSeq -> Maybe ShrinkableTxSeq
    toShrinkable s = flip ShrinkableTxSeq s <$> nextShrinkState s shrinkState

--------------------------------------------------------------------------------
-- Internal types and functions
--------------------------------------------------------------------------------

-- | Records the current shrink state for a 'ShrinkableTxSeq'.
--
-- Shrinking proceeds through a sequence of /phases/.
--
-- At the beginning of each phase, we generate a number of shrink /actions/
-- specifically for that phase. Once we have finished executing all shrink
-- actions for a given phase, we transition to the next phase.
--
-- Shrinking terminates when all shrink phases are complete.
--
data ShrinkState
    = ShrinkState !ShrinkPhase ![ShrinkAction]
    -- ^ Indicates the current shrink phase and the remaining actions for that
    -- phase.
    | ShrinkStateFinished
    -- ^ Indicates that all phases are complete and that shrinking has
    -- terminated.
    deriving (Eq, Show)

-- | Represents a single phase of shrinking for a transaction sequence.
--
-- Each phase can generate zero or more shrink actions. The number of shrink
-- actions generated will depend on the contents of the transaction sequence
-- at the start of the phase.
--
-- Phases are ordered according to their aggressiveness (ability to shrink) and
-- their efficiency (computation overhead): phases that are more aggressive
-- and require less computation are placed earlier in the sequence.
--
data ShrinkPhase
    = ShrinkPhaseReduceToPrefixes
    | ShrinkPhaseReduceToSuffixes
    | ShrinkPhaseDropNullTxs
    | ShrinkPhaseDropGroupBoundaries
    | ShrinkPhaseDropGroupBoundary
    | ShrinkPhaseRemoveAssetIds
    | ShrinkPhaseShrinkAssetIds
    | ShrinkPhaseShrinkTxIds
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Represents a single shrink action.
--
data ShrinkAction
    = ShrinkActionReduceToPrefixes
    | ShrinkActionReduceToSuffixes
    | ShrinkActionDropNullTxs
    | ShrinkActionDropGroupBoundaries
    | ShrinkActionDropGroupBoundary
    | ShrinkActionRemoveAssetId !AssetId
    | ShrinkActionShrinkAssetIds
    | ShrinkActionShrinkTxIds
    deriving (Eq, Show)

-- | Generates a list of shrink actions for the current phase and partially
--   shrunk sequence.
--
shrinkPhaseActions :: TxSeq -> ShrinkPhase -> [ShrinkAction]
shrinkPhaseActions txSeq = \case
    ShrinkPhaseReduceToPrefixes ->
        [ShrinkActionReduceToPrefixes]
    ShrinkPhaseReduceToSuffixes ->
        [ShrinkActionReduceToSuffixes]
    ShrinkPhaseDropNullTxs ->
        [ShrinkActionDropNullTxs]
    ShrinkPhaseDropGroupBoundaries ->
        [ShrinkActionDropGroupBoundaries]
    ShrinkPhaseDropGroupBoundary ->
        [ShrinkActionDropGroupBoundary | _ <- [1 .. groupBoundaryCount]]
    ShrinkPhaseRemoveAssetIds ->
        [ShrinkActionRemoveAssetId a | a <- assetIds]
    ShrinkPhaseShrinkAssetIds ->
        [ShrinkActionShrinkAssetIds]
    ShrinkPhaseShrinkTxIds ->
        [ShrinkActionShrinkTxIds]
  where
    assetIds = F.toList $ TxSeq.assetIds txSeq
    groupBoundaryCount = TxSeq.txGroupBoundaryCount txSeq

-- | Transforms a shrink action into a shrinking function for a partially
--   shrunk sequence.
--
applyShrinkAction :: ShrinkAction -> TxSeq -> [TxSeq]
applyShrinkAction action txSeq = case action of
    ShrinkActionReduceToPrefixes ->
        TxSeq.prefixes txSeq
    ShrinkActionReduceToSuffixes ->
        TxSeq.suffixes txSeq
    ShrinkActionDropNullTxs ->
        [TxSeq.dropNullTxs txSeq]
    ShrinkActionDropGroupBoundaries ->
        [TxSeq.dropGroupBoundaries txSeq]
    ShrinkActionDropGroupBoundary ->
        TxSeq.dropGroupBoundary txSeq
    ShrinkActionRemoveAssetId assetId ->
        [TxSeq.removeAssetId txSeq assetId]
    ShrinkActionShrinkAssetIds ->
        [TxSeq.shrinkAssetIds txSeq]
    ShrinkActionShrinkTxIds ->
        [TxSeq.shrinkTxIds txSeq]

-- | Transforms a shrink state into a shrinking function for a partially
--   shrunk sequence.
--
applyShrinkStateAction :: ShrinkState -> TxSeq -> [TxSeq]
applyShrinkStateAction state txSeq = case state of
    ShrinkState _ (action : _) ->
        applyShrinkAction action txSeq
    ShrinkState _ [] ->
        []
    ShrinkStateFinished ->
        []

initialShrinkPhase :: ShrinkPhase
initialShrinkPhase = minBound

initialShrinkState :: TxSeq -> ShrinkState
initialShrinkState = shrinkPhaseToState initialShrinkPhase

-- | Transitions to the next shrink phase, if one is available.
--
nextShrinkPhase :: ShrinkPhase -> Maybe ShrinkPhase
nextShrinkPhase = boundedEnumSucc

-- | Transitions to the next shrink state, if one is available.
--
nextShrinkState :: TxSeq -> ShrinkState -> Maybe ShrinkState
nextShrinkState txSeq = \case
    ShrinkState phase (_ : actions) ->
        Just $ ShrinkState phase actions
    ShrinkState phase [] ->
        Just $ case nextShrinkPhase phase of
            Nothing -> ShrinkStateFinished
            Just sp -> shrinkPhaseToState sp txSeq
    ShrinkStateFinished ->
        Nothing

-- | Initializes a 'ShrinkState' for the given phase and partially shrunk
--   sequence.
--
shrinkPhaseToState :: ShrinkPhase -> TxSeq -> ShrinkState
shrinkPhaseToState phase txSeq =
    ShrinkState phase (shrinkPhaseActions txSeq phase)

getShrinkPhase :: ShrinkableTxSeq -> Maybe ShrinkPhase
getShrinkPhase txSeq = case shrinkState txSeq of
    ShrinkState phase _ -> Just phase
    ShrinkStateFinished -> Nothing

getShrinkState :: ShrinkableTxSeq -> ShrinkState
getShrinkState = shrinkState

-- | Generates a valid transaction for the given 'UTxO' set.
--
genTxFromUTxO :: Gen Address -> UTxO -> Gen Tx
genTxFromUTxO genAddr u = do
    (inputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 2)
    (collateralInputs, _) <-
        selectUTxOEntries u =<< chooseInt (1, 2)
    withdrawals <-
        genMapWith genRewardAccount genCoinPositive
    let inputValue = mconcat
            [ F.foldMap (tokens . snd) inputs
            , F.foldMap TokenBundle.fromCoin withdrawals
            ]
    let collateralInputValue =
            F.foldMap (tokens . snd) collateralInputs
    feeCoin <-
        min (TokenBundle.coin inputValue) <$> chooseCoin (Coin 1, Coin 4)
    let inputValueMinusFee =
            inputValue `TokenBundle.difference` TokenBundle.fromCoin feeCoin
    outputBundles <-
        genTokenBundlePartitionNonNull inputValueMinusFee =<< chooseInt (1, 3)
    collateralOutputBundles <-
        elements [[], [collateralInputValue]]
    outputAddresses <-
        vectorOf (length outputBundles) genAddr
    collateralOutputAddresses <-
        vectorOf (length collateralOutputBundles) genAddr
    scriptValidity <- elements
        [ Nothing
        , Just TxScriptValid
        , Just TxScriptInvalid
        ]
    pure $ txWithoutIdToTx TxWithoutId
        { fee = Just feeCoin
        , resolvedInputs = fmap Just <$> inputs
        , resolvedCollateralInputs = fmap Just <$> collateralInputs
        , outputs = zipWith TxOut outputAddresses outputBundles
        , collateralOutput = listToMaybe $
            zipWith TxOut collateralOutputAddresses collateralOutputBundles
        , metadata = Nothing
        , withdrawals
        , scriptValidity
        }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

boundedEnumSucc :: (Bounded a, Enum a, Ord a) => a -> Maybe a
boundedEnumSucc a
    | a >= maxBound = Nothing
    | otherwise = Just (succ a)
