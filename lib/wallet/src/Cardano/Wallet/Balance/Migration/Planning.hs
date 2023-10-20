{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module contains an algorithm for planning migrations at a high level.
--
-- It determines how to partition the UTxO set into entries of different types,
-- and in which order to add entries to selections, in order to maximize the
-- number of entries that can be successfully migrated.
--
-- Use 'createPlan' to create a migration plan.
--
module Cardano.Wallet.Balance.Migration.Planning
    (
    -- * Migration planning
      createPlan
    , MigrationPlan (..)

    -- * UTxO entry categorization
    , CategorizedUTxO (..)
    , UTxOEntryCategory (..)
    , categorizeUTxO
    , categorizeUTxOEntries
    , categorizeUTxOEntry
    , uncategorizeUTxO
    , uncategorizeUTxOEntries

    ) where

import Prelude

import Cardano.Wallet.Balance.Migration.Selection
    ( RewardWithdrawal (..)
    , Selection (..)
    , SelectionError (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Data.Either
    ( isRight
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Balance.Migration.Selection as Selection
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Migration planning
--------------------------------------------------------------------------------

-- | Represents a plan for migrating a set of UTxO entries.
--
-- Use 'createPlan' to create a migration plan.
--
data MigrationPlan input = MigrationPlan
    { selections :: ![Selection input]
      -- ^ A list of generated selections: each selection is the basis for a
      -- single transaction.
    , unselected :: !(CategorizedUTxO input)
      -- ^ The portion of the UTxO that was not selected.
    , totalFee :: !Coin
      -- ^ The total fee payable: equal to the sum of the fees of the
      -- individual selections.
    }
    deriving (Eq, Generic, Show)

-- | Creates a migration plan for the given categorized UTxO set and reward
--   withdrawal amount.
--
-- See 'MigrationPlan'.
--
createPlan
    :: TxConstraints
    -> CategorizedUTxO input
    -> RewardWithdrawal
    -> MigrationPlan input
createPlan constraints =
    run []
  where
    run !selections !utxo !reward =
        case createSelection constraints utxo reward of
            Just (utxo', selection) ->
                run (selection : selections) utxo' (RewardWithdrawal $ Coin 0)
            Nothing -> MigrationPlan
                { selections
                , unselected = utxo
                , totalFee = F.foldMap (view #fee) selections
                }

-- | Creates an individual selection for inclusion in a migration plan.
--
-- A selection is the basis for an individual transaction.
--
-- Returns 'Nothing' if it was not possible to create a selection with the UTxO
-- entries that remain.
--
createSelection
    :: TxConstraints
    -> CategorizedUTxO input
    -> RewardWithdrawal
    -> Maybe (CategorizedUTxO input, Selection input)
createSelection constraints utxo rewardWithdrawal =
    initializeSelection constraints utxo rewardWithdrawal
    <&> extendSelectionUntilFull constraints

-- | Initializes a selection with a single entry.
--
-- Returns 'Nothing' if it was not possible to initialize a selection with the
-- UTxO entries that remain.
--
initializeSelection
    :: TxConstraints
    -> CategorizedUTxO input
    -> RewardWithdrawal
    -> Maybe (CategorizedUTxO input, Selection input)
initializeSelection constraints utxoAtStart reward =
    initializeWith =<< utxoAtStart `select` Supporter
  where
    initializeWith (entry, utxo) =
        case Selection.create constraints reward [entry] of
            Right selection -> Just (utxo, selection)
            Left _ -> Nothing

-- | Extends a selection repeatedly, until the selection is full.
--
-- This function terminates when the selection cannot be extended further
-- (because doing so would cause it to exceed the size limit of a transaction),
-- or when there are no more UTxO entries available for selection.
--
-- Priority is given to selecting "freerider" entries: entries that cannot pay
-- for themselves. A "supporter" entry is only added to the selection if there
-- is not enough ada to pay for a "freerider" entry.
--
extendSelectionUntilFull
    :: TxConstraints
    -> (CategorizedUTxO input, Selection input)
    -> (CategorizedUTxO input, Selection input)
extendSelectionUntilFull constraints = extendWithFreerider
  where
    extendWithFreerider (!utxo, !selection) =
        case extendWith Freerider constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                extendWithSupporter (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

    extendWithSupporter (!utxo, !selection) =
        case extendWith Supporter constraints (utxo, selection) of
            Right (utxo', selection') ->
                extendWithFreerider (utxo', selection')
            Left ExtendSelectionAdaInsufficient ->
                (utxo, selection)
            Left ExtendSelectionEntriesExhausted ->
                (utxo, selection)
            Left ExtendSelectionFull ->
                (utxo, selection)

data ExtendSelectionError
    = ExtendSelectionAdaInsufficient
    | ExtendSelectionEntriesExhausted
    | ExtendSelectionFull

extendWith
    :: UTxOEntryCategory
    -> TxConstraints
    -> (CategorizedUTxO input, Selection input)
    -> Either ExtendSelectionError (CategorizedUTxO input, Selection input)
extendWith category constraints (utxo, selection) =
    case utxo `select` category of
        Just (entry, utxo') ->
            case Selection.extend constraints selection entry of
                Right selection' ->
                    Right (utxo', selection')
                Left SelectionAdaInsufficient ->
                    Left ExtendSelectionAdaInsufficient
                Left SelectionFull {} ->
                    Left ExtendSelectionFull
        Nothing ->
            Left ExtendSelectionEntriesExhausted

select
    :: CategorizedUTxO input
    -> UTxOEntryCategory
    -> Maybe ((input, TokenBundle), CategorizedUTxO input)
select utxo = \case
    Supporter -> selectSupporter
    Freerider -> selectFreerider
    Ignorable -> selectIgnorable
  where
    selectSupporter = case supporters utxo of
        entry : remaining -> Just (entry, utxo {supporters = remaining})
        [] -> Nothing
    selectFreerider = case freeriders utxo of
        entry : remaining -> Just (entry, utxo {freeriders = remaining})
        [] ->  Nothing
    selectIgnorable =
        -- We never select an entry that should be ignored:
        Nothing

--------------------------------------------------------------------------------
-- Categorization of UTxO entries
--------------------------------------------------------------------------------

data UTxOEntryCategory
    = Supporter
    -- ^ A coin or bundle that is capable of paying for its own marginal fee
    -- and the base transaction fee.
    | Freerider
    -- ^ A coin or bundle that is not capable of paying for itself.
    | Ignorable
    -- ^ A coin that should not be added to a selection, because its value is
    -- lower than the marginal fee for an input.
    deriving (Eq, Show)

data CategorizedUTxO input = CategorizedUTxO
    { supporters :: ![(input, TokenBundle)]
    , freeriders :: ![(input, TokenBundle)]
    , ignorables :: ![(input, TokenBundle)]
    }
    deriving (Eq, Show)

categorizeUTxO
    :: TxConstraints
    -> UTxO
    -> CategorizedUTxO (TxIn, TxOut)
categorizeUTxO constraints (UTxO u) = categorizeUTxOEntries constraints $
    (\(i, o) -> ((i, o), view #tokens o)) <$> Map.toList u

categorizeUTxOEntries
    :: forall input. TxConstraints
    -> [(input, TokenBundle)]
    -> CategorizedUTxO input
categorizeUTxOEntries constraints uncategorizedEntries = CategorizedUTxO
    { supporters = entriesMatching Supporter
    , freeriders = entriesMatching Freerider
    , ignorables = entriesMatching Ignorable
    }
  where
    categorizedEntries :: [(input, (TokenBundle, UTxOEntryCategory))]
    categorizedEntries = uncategorizedEntries
        <&> (\(i, b) -> (i, (b, categorizeUTxOEntry constraints b)))

    entriesMatching :: UTxOEntryCategory -> [(input, TokenBundle)]
    entriesMatching category =
        fmap fst <$> L.filter ((== category) . snd . snd) categorizedEntries

categorizeUTxOEntry
    :: TxConstraints
    -> TokenBundle
    -> UTxOEntryCategory
categorizeUTxOEntry constraints b
    | Just c <- TokenBundle.toCoin b, coinIsIgnorable c =
        Ignorable
    | bundleIsSupporter =
        Supporter
    | otherwise =
        Freerider
  where
    bundleIsSupporter :: Bool
    bundleIsSupporter = isRight $
        Selection.create constraints (RewardWithdrawal $ Coin 0) [((), b)]

    coinIsIgnorable :: Coin -> Bool
    coinIsIgnorable c = c <= txInputCost constraints

uncategorizeUTxO :: CategorizedUTxO (TxIn, TxOut) -> UTxO
uncategorizeUTxO = UTxO . Map.fromList . fmap fst . uncategorizeUTxOEntries

uncategorizeUTxOEntries :: CategorizedUTxO input -> [(input, TokenBundle)]
uncategorizeUTxOEntries utxo = mconcat
    [ supporters utxo
    , freeriders utxo
    , ignorables utxo
    ]
