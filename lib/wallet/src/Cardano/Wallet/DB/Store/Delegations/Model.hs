{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations
    , DeltaDelegations
    ) where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet.Delegation.Model
    ( History
    , Operation (..)
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
import Fmt
    ( Buildable (..)
    )

-- | Wallet delegation history
type Delegations = History SlotNo DRep PoolId

-- | Delta of wallet delegation history. As always with deltas, the
-- order of the operations matters and it's reversed! (ask the architects)
type DeltaDelegations = Operation SlotNo DRep PoolId

instance Buildable DeltaDelegations where
    build = \case
            Deregister slot -> "Deregister " <> build slot
            VoteAndDelegate vote pool slot ->
                    "Delegate " <> build pool
                        <> " and vote "<> build vote <> " " <> build slot
            Rollback slot -> "Rollback " <> build slot
