{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

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
    , BlockEvents (..)
    , ChainEvents
    -- * Testing
    , summarizeOnTxOut
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate
    , SlotNo
    , dlgCertAccount
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxOut (..) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Quantity
    ( Quantity )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.List.NonEmpty as NE

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

-- | 'BlockEvents', orderd by slot.
type ChainEvents = [BlockEvents]

-- | Events (Txs, delegations) within a single block
-- that are potentially relevant to the wallet.
-- This can be the entire block, or a pre-filtered version of it.
data BlockEvents = BlockEvents
    { slotNo :: !SlotNo
    , blockHeight :: !(Quantity "block" Word32)
    , transactions :: [Tx]
    , delegations :: [DelegationCertificate]
    } deriving (Eq, Ord, Generic)

nullBlockEvents :: BlockEvents -> Bool
nullBlockEvents BlockEvents{transactions=[],delegations=[]} = True
nullBlockEvents _ = False

-- | For testing:
-- Convert a list of blocks into a 'BlockSummary'.
-- Unfortunately, as 'TxIn' references are not resolved,
-- we can only find transactions with relevant 'TxOut'.
summarizeOnTxOut :: NonEmpty Block -> LightSummary Identity
summarizeOnTxOut bs = BlockSummary
    { from = header . NE.head $ bs
    , to = header . NE.last $ bs
    , query = \q -> pure
        . filter (not . nullBlockEvents) . map (filterBlock q) $ NE.toList bs
    }

filterBlock :: Either Address RewardAccount -> Block -> BlockEvents
filterBlock question block = BlockEvents
    { slotNo = block ^. (#header . #slotNo)
    , blockHeight = block ^. (#header . #blockHeight)
    , transactions = txs  -- order in which they appear in the block
    , delegations = dlgs
    }
  where
    -- NOTE: Currently used the full address,
    -- containing both payment and staking parts.
    -- We may want to query only for the payment part at some point.
    txs = case question of
        Left addr -> filter (isRelevantTx addr) $ block ^. #transactions
        Right _   -> []
    dlgs = case question of
        Left _     -> []
        Right racc ->
            filter (isRelevantDelegation racc) $  block ^. #delegations
    isRelevantTx addr = any ((addr ==) . address) . outputs
    isRelevantDelegation racc = (racc == ) . dlgCertAccount
