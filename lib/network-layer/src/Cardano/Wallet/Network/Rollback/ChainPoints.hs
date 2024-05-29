module Cardano.Wallet.Network.Rollback.ChainPoints
    ( ChainPoints (..)
    , WithChainPoint (..)
    , chainPointDifference
    ,
    )
where

import Prelude

import Cardano.Wallet.Read
    ( ChainPoint (..)
    , SlotNo (..)
    )
import Numeric.Natural
    ( Natural
    )

-- | Abstract data type representing a store for values at chain points.
data ChainPoints value = ChainPoints
    { rollback :: ChainPoint -> ChainPoints value
    -- ^ Rollback to the given chain point, returning the new chain point and
    -- the new store.
    , feed :: ChainPoint -> value -> ChainPoints value
    -- ^ Feed a new value
    , current :: value
    -- ^ The current stored value
    }

-- | A value associated with a chain point.
data WithChainPoint a = Valued
    { point :: ChainPoint
    , value :: a
    }

-- | Calculate the difference in slots between two chain points. It will not
-- return a negative number which means that if the second chain point is
-- after the first one, the result will be 0.
chainPointDifference :: ChainPoint -> ChainPoint -> Natural
chainPointDifference x y = max 0 $ slotNo x - slotNo y
  where
    slotNo :: ChainPoint -> Natural
    slotNo GenesisPoint = 0
    slotNo (BlockPoint s _) = unSlotNo s
