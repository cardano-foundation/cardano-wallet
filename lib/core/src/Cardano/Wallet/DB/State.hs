{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Pure data type which represents the entire wallet state,
-- including all checkpoints.
--
-- FIXME during ADP-1043: Actually include everything,
-- e.g. TxHistory, Pending transactions, …

module Cardano.Wallet.DB.State
    ( -- * Wallet state
      WalletState (..)
    , fromGenesis
    , getLatest
    , findNearestPoint


    -- * Delta types
    , DeltaWalletState1 (..)
    , DeltaWalletState
    , DeltaMap (..)
    ) where

import Prelude

import Cardano.Wallet.DB.Store.Checkpoints.AddressBook
    ( AddressBookIso (..), Prologue )
import Cardano.Wallet.DB.Store.Checkpoints.Model
    ( Checkpoints, DeltaCheckpoints, WalletCheckpoint, fromWallet, toWallet )
import Data.Delta
    ( Delta (..) )
import Data.DeltaMap
    ( DeltaMap (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Fmt
    ( Buildable (..), pretty )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.DB.Store.Checkpoints.Model as CPS

-- import qualified Cardano.Wallet.DB.Model as Model
-- import Cardano.Wallet.DB.Store.TxHistory.Model
--    ( DeltaTxHistory, TxHistory (TxHistory) )
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W


{-------------------------------------------------------------------------------
    Wallet State
-------------------------------------------------------------------------------}
-- | Wallet state. Currently includes:
--
-- * Prologue of the address discovery state
-- * Checkpoints of UTxO and of discoveries of the address discovery state.
--
-- FIXME during ADP-1043: Include also TxHistory, pending transactions, …,
-- everything.
data WalletState s = WalletState
    { prologue    :: Prologue s
    , checkpoints :: Checkpoints (WalletCheckpoint s)
    -- , transactions :: TxHistory
    } deriving (Generic)

deriving instance AddressBookIso s => Eq (WalletState s)

-- | Create a wallet from the genesis block.
fromGenesis
    :: AddressBookIso s
    => W.Wallet s
    -- -> Model.TxHistory
    -> Maybe (WalletState s)
fromGenesis cp -- txs
    | W.isGenesisBlockHeader header = Just $
        WalletState{ prologue
            , checkpoints = CPS.fromGenesis checkpoint
            -- , transactions = TxHistory txs
            }
    | otherwise = Nothing
  where
    header = cp ^. #currentTip
    (prologue, checkpoint) = fromWallet cp

-- | Get the wallet checkpoint with the largest slot number
getLatest :: AddressBookIso s => WalletState s -> W.Wallet s
getLatest w =
    toWallet (w ^. #prologue) . snd $ CPS.getLatest (w ^. #checkpoints)

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint :: WalletState s -> W.Slot -> Maybe W.Slot
findNearestPoint = CPS.findNearestPoint . view #checkpoints

{-------------------------------------------------------------------------------
    Delta type for the wallet state
-------------------------------------------------------------------------------}
type DeltaWalletState s = [DeltaWalletState1 s]

data DeltaWalletState1 s
    = ReplacePrologue (Prologue s)
    -- ^ Replace the prologue of the address discovery state
    | UpdateCheckpoints (DeltaCheckpoints (WalletCheckpoint s))
    -- ^ Update the wallet checkpoints.
    -- | UpdateTransactions DeltaTxHistory

instance Delta (DeltaWalletState1 s) where
    type Base (DeltaWalletState1 s) = WalletState s
    apply (ReplacePrologue p) = over #prologue $ const p
    apply (UpdateCheckpoints d) = over #checkpoints $ apply d
    -- apply (UpdateTransactions d) = over #transactions $ apply d

instance Buildable (DeltaWalletState1 s) where
    build (ReplacePrologue _) = "ReplacePrologue …"
    build (UpdateCheckpoints d) = "UpdateCheckpoints (" <> build d <> ")"
    -- build (UpdateTransactions d) = "UpdateTransactions (" <> build d <> ")"

instance Show (DeltaWalletState1 s) where
    show = pretty
