module Cardano.Wallet.Write.Tx.Size
    ( sizeOfSignedWithdrawal
    ) where

import Prelude

import Numeric.Natural
    ( Natural )

sizeOfSignedWithdrawal :: Natural
sizeOfSignedWithdrawal =
    sizeOfWithdrawal + sizeOfCredential
  where
    sizeOfCredential = 96 -- ?

-- withdrawals =
--   { * reward_account => coin }
sizeOfWithdrawal :: Natural
sizeOfWithdrawal
    = sizeOf_Hash28
    + sizeOf_LargeUInt

-- A Blake2b-224 hash, resulting in a 28-byte digest wrapped in CBOR, so
-- with 2 bytes overhead (length <255, but length > 23)
sizeOf_Hash28 :: Natural
sizeOf_Hash28
    = 30

sizeOf_LargeUInt :: Natural
sizeOf_LargeUInt = 9
