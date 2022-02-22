{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

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

    -- * Testing
    , summarizeOnTxOut
    , mkChainEvents
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( RewardAccount )
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
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..) )
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
-- | 'BlockEvents', orderd by slot.
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
    , transactions :: [(Int, Tx)]
        -- ^ (Index of the transaction within the block, transaction data)
        -- INVARIANT: The list is ordered by ascending index.
    , delegations :: [(Int, DelegationCertificate)]
        -- ^ (Index of the delegation within the block, delegation data)
        -- INVARIANT: The list is ordered by ascending index.
    } deriving (Eq, Ord, Generic, Show)

nullBlockEvents :: BlockEvents -> Bool
nullBlockEvents BlockEvents{transactions=[],delegations=[]} = True
nullBlockEvents _ = False

-- | Merge block events that belong to the same block.
mergeSameBlock :: BlockEvents -> BlockEvents -> BlockEvents
mergeSameBlock
    BlockEvents{slot,blockHeight,transactions=txs1,delegations=dlg1}
    BlockEvents{transactions=txs2,delegations=dlg2}
  = BlockEvents
    { slot
    , blockHeight
    , transactions = mergeOn fst const txs1 txs2
    , delegations = mergeOn fst const dlg1 dlg2
    }

-- | Merge two lists in sorted order. Remove duplicate items.
mergeOn :: Ord key => (a -> key) -> (a -> a -> a) -> [a] -> [a] -> [a]
mergeOn _ _ [] ys = ys
mergeOn _ _ xs [] = xs
mergeOn f g (x:xs) (y:ys) = case compare (f x) (f y) of
    LT -> x : mergeOn f g xs (y:ys)
    EQ -> mergeOn f g (g x y:xs) ys
    GT -> y : mergeOn f g (x:xs) ys

-- | Get the 'BlockEvents' corresponding to an entire 'Block'.
fromEntireBlock :: Block -> BlockEvents
fromEntireBlock Block{header,transactions,delegations} = BlockEvents
    { slot = toSlot $ chainPointFromBlockHeader header
    , blockHeight = Block.blockHeight header
    , transactions = zip [0..] transactions
    , delegations = zip [0..] delegations
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
            Left addr -> filter (isRelevantTx addr) transactions
            Right _   -> []
        , delegations = case question of
            Left _     -> []
            Right racc -> filter (isRelevantDelegation racc) delegations
        }
  where
    -- NOTE: Currently used the full address,
    -- containing both payment and staking parts.
    -- We may want to query only for the payment part at some point.
    isRelevantTx addr = any ((addr ==) . address) . outputs . snd
    isRelevantDelegation racc = (racc == ) . dlgCertAccount . snd
