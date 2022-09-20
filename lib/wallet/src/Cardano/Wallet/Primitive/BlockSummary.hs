{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'BlockSummary' type
-- which summarizes a contiguous sequence of blocks.
--
module Cardano.Wallet.Primitive.BlockSummary
    ( BlockSummary (..)
    , LightSummary

    -- * Chain Events
    , ChainEvents
    , fromBlockEvents
    , toAscBlockEvents

    -- * Block Events
    , BlockEvents (..)
    , fromEntireBlock

    -- * Sublist
    , Sublist
    , filterSublist
    , wholeList

    -- * Internal & Testing
    , summarizeOnTxOut
    , mkChainEvents
    , mergeSublist
    , unsafeMkSublist
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate
    , Slot
    , chainPointFromBlockHeader
    , dlgCertAccount
    , toSlot
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..) )
import Data.Foldable
    ( Foldable (toList) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Quantity
    ( Quantity )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types as Block
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    BlockSummary
-------------------------------------------------------------------------------}
-- | A 'BlockSummary' summarizes the data contained in a contiguous sequence
-- of blocks.
--
-- However, instead of storing the sequence of blocks of directly as a Haskell
-- list, the 'BlockSummary' only provides a 'query' function
-- which looks up all transactions associated to a given addresses.
-- In addition, this query function is monadic, which means that it
-- can call out to an external data source.
--
data BlockSummary m addr txs = BlockSummary
    { from  :: !BlockHeader
    , to    :: !BlockHeader
    , query :: addr -> m txs
    } deriving (Generic)

-- | 'BlockSummary' used for light-mode.
type LightSummary m =
    BlockSummary m (Either Address RewardAccount) ChainEvents

{-------------------------------------------------------------------------------
    ChainEvents
-------------------------------------------------------------------------------}
-- | 'BlockEvents', always ordered by slot.
newtype ChainEvents = ChainEvents (Map Slot BlockEvents)
    deriving (Eq, Ord, Show)

mkChainEvents :: Map Slot BlockEvents -> ChainEvents
mkChainEvents = ChainEvents

instance Semigroup ChainEvents where
    (ChainEvents bs1) <> (ChainEvents bs2) =
        ChainEvents $ Map.unionWith mergeSameBlock bs1 bs2

instance Monoid ChainEvents where
    mempty = ChainEvents mempty

-- | Create 'ChainEvents' from a list of block events
-- (which do not need to be in order.)
fromBlockEvents :: [BlockEvents] -> ChainEvents
fromBlockEvents = ChainEvents
    . Map.fromListWith mergeSameBlock
    . map (\bl -> (slot bl, bl))
    . filter (not . nullBlockEvents)

-- | List of 'BlockEvents', in ascending order.
-- (No duplicate blocks, all transactions within block in order).
toAscBlockEvents :: ChainEvents -> [BlockEvents]
toAscBlockEvents (ChainEvents bs) = Map.elems bs

{-------------------------------------------------------------------------------
    BlockEvents
-------------------------------------------------------------------------------}
-- | Events (such as txs, delegations) within a single block
-- that are potentially relevant to the wallet.
-- This can be the entire block, or a pre-filtered version of it.
data BlockEvents = BlockEvents
    { slot :: !Slot
    , blockHeight :: !(Quantity "block" Word32)
    , transactions :: Sublist Tx
        -- ^ (Index of the transaction within the block, transaction data)
        -- INVARIANT: The list is ordered by ascending index.
    , delegations :: Sublist DelegationCertificate
        -- ^ (Index of the delegation within the block, delegation data)
        -- INVARIANT: The list is ordered by ascending index.
    } deriving (Eq, Ord, Generic, Show)

type Index1 = Natural
type Index2 = Natural

-- | A data type representing a sublist of a total list.
-- Such a sublist typically arises by filtering and keeps
-- track of the indices of the filtered list elements.
--
-- In order to represent sublists of 'DelegationCertificate',
-- we do not use a single 'Int', but a pair @(Index1,Index2)@
-- as index internally.
-- This internal index is not part of the (safe) API of 'Sublist'.
--
-- The main purpose of this data type is optimization:
-- When processing whole 'Block', we want to avoid copying
-- and redecorating the entire list of transactions in that 'Block';
-- instead, we want to copy a pointer to this list.
data Sublist a = All [a] | Some (Map (Index1, Index2) a)
    deriving (Eq, Ord, Show)

-- | Construct a 'Sublist' representing the whole list.
wholeList :: [a] -> Sublist a
wholeList = All

-- | Construct a 'Sublist' from a list of indexed items.
unsafeMkSublist :: [((Index1, Index2), a)] -> Sublist a
unsafeMkSublist = Some . Map.fromList

-- | Filter a 'Sublist' by a predicate.
filterSublist :: (a -> Bool) -> Sublist a -> Sublist a
filterSublist p (All xs) =
    filterSublist p $ unsafeMkSublist $ zip (map (,0) [0..]) xs
filterSublist p (Some ixs) = Some $ Map.filter p ixs

instance Functor Sublist where
    fmap f (All xs) = All (map f xs)
    fmap f (Some ixs) = Some $ f <$> ixs

instance Foldable Sublist where
    foldr f b = foldr f b . toList
    null = null . toList
    toList (All as) = as
    toList (Some ixs) = Map.elems ixs

-- | Returns 'True' if the 'BlockEvents' contains empty
-- 'transactions' and 'delegations'.
nullBlockEvents :: BlockEvents -> Bool
nullBlockEvents BlockEvents{transactions,delegations}
    = null transactions && null delegations

-- | Merge two 'Sublist' assuming that they are sublists of the /same/ list.
mergeSublist :: Sublist a -> Sublist a -> Sublist a
mergeSublist (All xs) _ = All xs -- result cannot be larger
mergeSublist _ (All ys) = All ys
mergeSublist (Some xs) (Some ys) = Some $ Map.union xs ys

-- | Merge block events that belong to the same block.
mergeSameBlock :: BlockEvents -> BlockEvents -> BlockEvents
mergeSameBlock
    BlockEvents{slot,blockHeight,transactions=txs1,delegations=dlg1}
    BlockEvents{transactions=txs2,delegations=dlg2}
  = BlockEvents
    { slot
    , blockHeight
    , transactions = mergeSublist txs1 txs2
    , delegations = mergeSublist dlg1 dlg2
    }

-- | Get the 'BlockEvents' corresponding to an entire 'Block'.
fromEntireBlock :: Block -> BlockEvents
fromEntireBlock Block{header,transactions,delegations} = BlockEvents
    { slot = toSlot $ chainPointFromBlockHeader header
    , blockHeight = Block.blockHeight header
    , transactions = All transactions
    , delegations = All delegations
    }

{-------------------------------------------------------------------------------
    Testing
-------------------------------------------------------------------------------}
-- | For testing:
-- Convert a list of blocks into a 'BlockSummary'.
-- Unfortunately, as 'TxIn' references are not resolved,
-- we can only find transactions with relevant 'TxOut'.
summarizeOnTxOut :: NonEmpty Block -> LightSummary Identity
summarizeOnTxOut bs = BlockSummary
    { from = header . NE.head $ bs
    , to = header . NE.last $ bs
    , query = \q -> pure
        . fromBlockEvents . map (filterBlock q) $ NE.toList bs
    }

filterBlock :: Either Address RewardAccount -> Block -> BlockEvents
filterBlock question block = case fromEntireBlock block of
    BlockEvents{slot,blockHeight,transactions,delegations} -> BlockEvents
        { slot
        , blockHeight
        , transactions = case question of
            Left addr -> filterSublist (isRelevantTx addr) transactions
            Right _   -> Some mempty
        , delegations = case question of
            Left _     -> Some mempty
            Right racc -> filterSublist (isRelevantDelegation racc) delegations
        }
  where
    -- NOTE: Currently used the full address,
    -- containing both payment and staking parts.
    -- We may want to query only for the payment part at some point.
    isRelevantTx addr = any ((addr ==) . address) . outputs
    isRelevantDelegation racc = (racc == ) . dlgCertAccount
