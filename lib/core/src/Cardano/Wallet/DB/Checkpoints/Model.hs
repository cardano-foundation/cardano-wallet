{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Data type that represents a collection of checkpoints.
-- Each checkpoints is associated with a 'Slot'.
module Cardano.Wallet.DB.Checkpoints.Model
    ( -- * Checkpoints
      Checkpoints
    , checkpoints
    , loadCheckpoints
    , fromGenesis
    , getLatest
    , findNearestPoint

    -- * Delta types
    , DeltaCheckpoints (..)

    -- * Wallet state component
    , getBlockHeight
    , getSlot
    , toWallet
    , fromWallet
    , WalletCheckpoint (..)
    ) where

import Prelude

import Cardano.Wallet.DB.Checkpoints.AddressBook
    ( AddressBookIso (Discoveries, Prologue), addressIso )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Delta
    ( Delta (..) )
import Data.Generics.Internal.VL
    ( withIso )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), listF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
{- NOTE [PointSlotNo]

'SlotNo' cannot represent the genesis point.

Historical hack. The DB layer can't represent 'Origin' in the database,
instead we have mapped it to 'SlotNo 0', which is wrong.

Rolling back to SlotNo 0 instead of Origin is fine for followers starting
from genesis (which should be the majority of cases). Other, non-trivial
rollbacks to genesis cannot occur on mainnet (genesis is years within
stable part, and there were no rollbacks in byron).

Could possibly be problematic in the beginning of a testnet without a
byron era. /Perhaps/ this is what is happening in the
>>> [cardano-wallet.pools-engine:Error:1293] [2020-11-24 10:02:04.00 UTC]
>>> Couldn't store production for given block before it conflicts with
>>> another block. Conflicting block header is:
>>> 5bde7e7b<-[f1b35b98-4290#2008]
errors observed in the integration tests.

The issue has been partially fixed in that 'rollbackTo' now takes
a 'Slot' as argument, which can represent the 'Origin'.
However, the database itself mostly stores slot numbers.

FIXME LATER during ADP-1043: As we move towards in-memory data,
all slot numbers in the DB file will either be replaced by
the 'Slot' type, or handled slightly differently when it
is clear that the data cannot exist at the genesis point
(e.g. for TxHistory).

-}

{-------------------------------------------------------------------------------
    Checkpoints
-------------------------------------------------------------------------------}

-- | Collection of checkpoints indexed by 'Slot'.
newtype Checkpoints a = Checkpoints
    { checkpoints :: Map W.Slot a
    -- ^ Map of checkpoints. Always contains the genesis checkpoint.
    }
    deriving (Eq, Show, Generic)

-- FIXME LATER during ADP-1043:
--    Use a more sophisticated 'Checkpoints' type that stores deltas.

-- | Turn the list of checkpoints into a map of checkpoints.
--
-- FIXME LATER during ADP-1043:
--     The database actually does not store the checkpoint at genesis,
--     but the checkpoint after that.
--     Hence, this function does not check whether the genesis checkpoint
--     is in the list of checkpoints.
loadCheckpoints :: [(W.Slot, a)] -> Checkpoints a
loadCheckpoints = Checkpoints . Map.fromList

-- | Begin with the genesis checkpoint.
fromGenesis :: a -> Checkpoints a
fromGenesis a = Checkpoints $ Map.singleton W.Origin a

-- | Get the checkpoint with the largest 'SlotNo'.
getLatest :: Checkpoints a -> (W.Slot, a)
getLatest = fromMaybe err . Map.lookupMax . view #checkpoints
  where
    err = error
        "getLatest: there should always be at least a genesis checkpoint"

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint :: Checkpoints a -> W.Slot -> Maybe W.Slot
findNearestPoint m key = fst <$> Map.lookupLE key (view #checkpoints m)

{-------------------------------------------------------------------------------
    Delta type for Checkpoints
-------------------------------------------------------------------------------}
data DeltaCheckpoints a
    = PutCheckpoint W.Slot a
    -- | Rolls back to the latest checkpoint at or before this slot.
    | RollbackTo W.Slot
    -- | Restrict to the intersection of this list with
    -- the checkpoints that are already present.
    -- The genesis checkpoint will always be present.
    |    RestrictTo [W.Slot]

instance Delta (DeltaCheckpoints a) where
    type Base (DeltaCheckpoints a) = Checkpoints a
    apply (PutCheckpoint pt a) = over #checkpoints $ Map.insert pt a
    apply (RollbackTo pt) =
        over #checkpoints $
            Map.filterWithKey (\k _ -> k <= pt)
    apply (RestrictTo pts) = over #checkpoints $ \m ->
        Map.restrictKeys m $ Set.fromList (W.Origin:pts)

instance Buildable (DeltaCheckpoints a) where
    build (PutCheckpoint slot _) = "PutCheckpoint " <> build slot
    build (RollbackTo slot) = "RollbackTo " <> build slot
    build (RestrictTo slots) = "RestrictTo " <> listF slots

{-------------------------------------------------------------------------------
    Wallet Checkpoint
-------------------------------------------------------------------------------}

-- | Data stored in a single checkpoint.
-- Only includes the 'UTxO' and the 'Discoveries', but not the 'Prologue'.
data WalletCheckpoint s = WalletCheckpoint
    { currentTip :: BlockHeader
    , utxo :: UTxO
    , discoveries :: Discoveries s
    }
    deriving (Generic)

deriving instance AddressBookIso s => Eq (WalletCheckpoint s)

-- | Helper function: Get the block height of a wallet checkpoint.
getBlockHeight :: WalletCheckpoint s -> Word32
getBlockHeight (WalletCheckpoint currentTip' _ _) =
    currentTip' ^. (#blockHeight . #getQuantity)

-- | Helper function: Get the 'Slot' of a wallet checkpoint.
getSlot :: WalletCheckpoint s -> W.Slot
getSlot (WalletCheckpoint currentTip' _ _) =
    W.toSlot . W.chainPointFromBlockHeader $ currentTip'

-- | Convert a stored 'WalletCheckpoint' to the legacy 'W.Wallet' state.
toWallet :: AddressBookIso s => Prologue s -> WalletCheckpoint s -> W.Wallet s
toWallet pro (WalletCheckpoint pt utxo' dis) =
    W.unsafeInitWallet utxo' pt $ withIso addressIso
        $ \_ from -> from (pro, dis)

-- | Convert a legacy 'W.Wallet' state to a 'Prologue' and a 'WalletCheckpoint'
fromWallet :: AddressBookIso s => W.Wallet s -> (Prologue s, WalletCheckpoint s)
fromWallet w = (pro, WalletCheckpoint (W.currentTip w) (W.utxo w) dis)
  where
    (pro, dis) = withIso addressIso $ \to _ -> to (w ^. #getState)
