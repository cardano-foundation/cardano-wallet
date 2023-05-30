module Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations
    , DeltaDelegations
    ) where


import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.Delegation.Model
    ( History, Operation )
import Cardano.Wallet.Primitive.Types
    ( SlotNo )

-- | Wallet delegation history
type Delegations = History SlotNo PoolId

-- | Delta of wallet delegation history. As always with deltas, the
-- order of the operations matters and it's reversed! (ask the architects)
type DeltaDelegations = [Operation SlotNo PoolId]
