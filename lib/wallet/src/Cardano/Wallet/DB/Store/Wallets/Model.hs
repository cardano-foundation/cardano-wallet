{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2018-2022 IOHK
 License: Apache-2.0

Pure model for the transactions ('Tx') and metadata about them ('TxMeta')
in a collection of wallets.

-}
module Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , TxWalletsHistory
    , walletsLinkedTransactions
    , inAnyWallet
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory (..)
    , WalletsMeta
    , mkTxMetaHistory
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxSet (..), mkTxSet )
import Data.Delta
    ( Delta (..) )
import Data.DeltaMap
    ( DeltaMap (Adjust, Insert) )
import Data.Foldable
    ( toList )
import Data.Generics.Internal.VL
    ( view )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable, build )

import qualified Cardano.Wallet.DB.Store.Meta.Model as TxMetaStore
import qualified Cardano.Wallet.DB.Store.Transactions.Model as TxStore
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as WT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data DeltaTxWalletsHistory
    = ExpandTxWalletsHistory W.WalletId [(WT.Tx, WT.TxMeta)]
    | RollbackTxWalletsHistory W.WalletId W.SlotNo
        -- ^ Roll back a single wallet
    | GarbageCollectTxWalletsHistory
    | RemoveWallet W.WalletId
    deriving ( Show, Eq )

instance Buildable DeltaTxWalletsHistory where
    build = build . show

type TxWalletsHistory =
    (TxSet, WalletsMeta)

instance Delta DeltaTxWalletsHistory where
    type Base DeltaTxWalletsHistory = TxWalletsHistory
    apply (ExpandTxWalletsHistory wid cs) (txh,mtxmh) =
        ( apply (TxStore.Append $ mkTxSet $ fst <$> cs) txh
        , flip apply mtxmh $ case Map.lookup wid mtxmh of
              Nothing -> Insert wid (mkTxMetaHistory wid cs)
              Just _ ->
                  Adjust wid
                  $ TxMetaStore.Expand
                  $ mkTxMetaHistory wid cs)
    apply (RollbackTxWalletsHistory wid slot) (x, mtxmh) =
        -- Roll back all wallets to a given slot (number)
        -- and garbage collect transactions that no longer
        -- have a 'TxMeta' associated with them.
        garbageCollectTxWalletsHistory
            (x, garbageCollectEmptyWallets $ apply (Adjust wid change) mtxmh)
      where
        change
            = TxMetaStore.Manipulate
            $ TxMetaStore.RollBackTxMetaHistory slot
    apply (RemoveWallet wid) (x , mtxmh) =
        garbageCollectTxWalletsHistory (x, Map.delete wid mtxmh)
    apply GarbageCollectTxWalletsHistory x = garbageCollectTxWalletsHistory x

-- | Garbage collect all transactions that are no longer referenced
-- by any 'TxMeta'.
garbageCollectTxWalletsHistory :: TxWalletsHistory -> TxWalletsHistory
garbageCollectTxWalletsHistory (TxSet txh, mtxmh) = (TxSet (gc txh), mtxmh)
  where
    gc :: Map TxId x -> Map TxId x
    gc x = Map.restrictKeys x $ walletsLinkedTransactions mtxmh

-- necessary because database will not distinguish between
-- a missing wallet in the map
-- and a wallet that has no meta-transactions
garbageCollectEmptyWallets :: Map k TxMetaHistory
    -> Map k TxMetaHistory
garbageCollectEmptyWallets = Map.filter (not . null . view #relations)

linkedTransactions :: TxMetaHistory -> Set TxId
linkedTransactions (TxMetaHistory m) = Map.keysSet m

walletsLinkedTransactions
    :: Map W.WalletId TxMetaHistory -> Set TxId
walletsLinkedTransactions = Set.unions . toList . fmap linkedTransactions

-- | Is a transaction present in any wallet?
inAnyWallet
    :: TxId
    -> Map W.WalletId TxMetaHistory
    -> Bool
inAnyWallet txid = any inAnyTxMetaHistory
  where
    inAnyTxMetaHistory (TxMetaHistory m) = Map.member txid m
