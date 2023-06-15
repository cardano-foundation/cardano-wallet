{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations
    , DeltaDelegations
    ) where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.Delegation.Model
    ( History, Operation (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo )
import Fmt
    ( Buildable (..), listF' )

-- | Wallet delegation history
type Delegations = History SlotNo PoolId

-- | Delta of wallet delegation history. As always with deltas, the
-- order of the operations matters and it's reversed! (ask the architects)
type DeltaDelegations = [Operation SlotNo PoolId]

instance Buildable DeltaDelegations where
    build = listF' build1
      where
        build1 = \case
            Register slot -> "Register " <> build slot
            Deregister slot -> "Deregister " <> build slot
            Delegate pool slot -> "Delegate " <> build pool <> " " <> build slot
            Rollback slot -> "Rollback " <> build slot
