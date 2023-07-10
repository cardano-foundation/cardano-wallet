{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Pure data type which represents the entire wallet state,
-- including all checkpoints.
--
-- FIXME during ADP-1043: Actually include everything,
-- e.g. TxHistory, Pending transactions, …

module Cardano.Wallet.DB.WalletState
    ( -- * Wallet state
      WalletState (..)
    , fromGenesis
    , getLatest
    , findNearestPoint

    -- * WalletCheckpoint (internal use mostly)
    , WalletCheckpoint (..)
    , toWallet
    , fromWallet
    , getBlockHeight
    , getSlot

    -- * Delta types
    , DeltaWalletState1 (..)
    , DeltaWalletState

    -- * Helpers
    , updateCheckpoints
    , updateSubmissions
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..), Discoveries, Prologue )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK) )
import Cardano.Wallet.Checkpoints
    ( Checkpoints )
import Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations, DeltaDelegations )
import Cardano.Wallet.DB.Store.Info.Store
    ( DeltaWalletInfo, WalletInfo (..) )
import Cardano.Wallet.DB.Store.PrivateKey.Store
    ( DeltaPrivateKey )
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( emptyTxSubmissions )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DeltaTxSubmissions, TxSubmissions )
import Cardano.Wallet.Flavor
    ( KeyOf )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Delta
    ( Delta (..), Replace (..) )
import Data.Delta.Update
    ( Update, updateField )
import Data.Generics.Internal.VL
    ( withIso )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), pretty )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Checkpoints as CPS
import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W

{-------------------------------------------------------------------------------
    Wallet Checkpoint
-------------------------------------------------------------------------------}
-- | Data stored in a single checkpoint.
-- Only includes the 'UTxO' and the 'Discoveries', but not the 'Prologue'.
data WalletCheckpoint s = WalletCheckpoint
    { currentTip :: !BlockHeader
    , utxo :: !UTxO
    , discoveries :: !(Discoveries s)
    } deriving (Generic)

deriving instance AddressBookIso s => Eq (WalletCheckpoint s)

-- | Helper function: Get the block height of a wallet checkpoint.
getBlockHeight :: WalletCheckpoint s -> Word32
getBlockHeight (WalletCheckpoint currentTip _ _) =
    currentTip ^. (#blockHeight . #getQuantity)

-- | Helper function: Get the 'Slot' of a wallet checkpoint.
getSlot :: WalletCheckpoint s -> W.Slot
getSlot (WalletCheckpoint currentTip _ _) =
    W.toSlot . W.chainPointFromBlockHeader $ currentTip

-- | Convert a stored 'WalletCheckpoint' to the legacy 'W.Wallet' state.
toWallet :: AddressBookIso s => Prologue s -> WalletCheckpoint s -> W.Wallet s
toWallet pro (WalletCheckpoint pt utxo dis) =
    W.unsafeInitWallet utxo pt $ withIso addressIso $ \_ from -> from (pro,dis)

-- | Convert a legacy 'W.Wallet' state to a 'Prologue' and a 'WalletCheckpoint'
fromWallet :: AddressBookIso s => W.Wallet s -> (Prologue s, WalletCheckpoint s)
fromWallet w = (pro, WalletCheckpoint (W.currentTip w) (W.utxo w) dis)
  where
    (pro, dis) = withIso addressIso $ \to _ -> to (w ^. #getState)

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
    { prologue    :: !(Prologue s)
    , checkpoints :: !(Checkpoints (WalletCheckpoint s))
    , submissions :: !TxSubmissions
    , info :: !WalletInfo
    , credentials :: Maybe (HashedCredentials (KeyOf s))
    , delegations :: Delegations
    , rewards :: Coin
    } deriving (Generic)

deriving instance
    (AddressBookIso s, Eq (KeyOf s 'RootK XPrv))
    => Eq (WalletState s)

-- | Create a wallet from the genesis block.
fromGenesis
    :: AddressBookIso s
    => W.Wallet s
    -> WalletInfo
    -> Maybe (WalletState s)
fromGenesis cp winfo
    | W.isGenesisBlockHeader header =
        Just
            $ WalletState
                { prologue
                , checkpoints = CPS.fromGenesis checkpoint
                , submissions = emptyTxSubmissions
                , info = winfo
                , credentials = Nothing
                , delegations = mempty
                , rewards = mempty
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
    | UpdateCheckpoints (CPS.DeltasCheckpoints (WalletCheckpoint s))
    -- ^ Update the wallet checkpoints.
    | UpdateSubmissions DeltaTxSubmissions
    | UpdateInfo DeltaWalletInfo
    | UpdateCredentials (DeltaPrivateKey (KeyOf s))
    | UpdateDelegations DeltaDelegations
    | UpdateRewards (Replace Coin)

instance Delta (DeltaWalletState1 s) where
    type Base (DeltaWalletState1 s) = WalletState s
    apply (ReplacePrologue p) = over #prologue $ const p
    apply (UpdateCheckpoints d) = over #checkpoints $ apply d
    apply (UpdateSubmissions d) = over #submissions $ apply d
    apply (UpdateInfo d) = over #info $ apply d
    apply (UpdateCredentials d) = over #credentials $ apply d
    apply (UpdateDelegations d) = over #delegations $ apply d
    apply (UpdateRewards d) = over #rewards $ apply d

instance Buildable (DeltaWalletState1 s) where
    build (ReplacePrologue _) = "ReplacePrologue …"
    build (UpdateCheckpoints d) = "UpdateCheckpoints (" <> build d <> ")"
    build (UpdateSubmissions d) = "UpdateSubmissions (" <> build d <> ")"
    build (UpdateInfo d) = "UpdateInfo (" <> build d <> ")"
    build (UpdateCredentials _d) = "UpdatePrivateKey"
    build (UpdateDelegations d) = "UpdateDelegations (" <> build d <> ")"
    build (UpdateRewards d) = "UpdateRewards (" <> build d <> ")"

instance Show (DeltaWalletState1 s) where
    show = pretty

{-------------------------------------------------------------------------------
    Helper functions
-------------------------------------------------------------------------------}
updateCheckpoints
    :: Update (CPS.DeltasCheckpoints (WalletCheckpoint s)) r
    -> Update (DeltaWalletState s) r
updateCheckpoints = updateField checkpoints ((:[]) . UpdateCheckpoints)

updateSubmissions
    :: Update DeltaTxSubmissions r
    -> Update (DeltaWalletState s) r
updateSubmissions = updateField submissions ((:[]) . UpdateSubmissions)

instance Buildable (Replace Coin) where
    build (Replace x) = build x
