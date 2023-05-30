module Cardano.Wallet.DB.Store.Delegations.Layer
    ( isStakeKeyRegistered
    , putDelegationCertificate
    , readDelegation
    )
where

import Prelude

import Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations, DeltaDelegations )
import Cardano.Wallet.Primitive.Types
    ( DelegationCertificate, SlotNo, WalletDelegation )


isStakeKeyRegistered :: Delegations -> Bool
isStakeKeyRegistered = error "TODO: isStakeKeyRegistered"

-- ^ Binds a stake pool id to a wallet. This will have an influence on
-- the wallet metadata: the last known certificate will indicate to
-- which pool a wallet is currently delegating.
--
-- This is done separately from 'putWalletMeta' because certificate
-- declarations are:
--
-- 1. Stored on-chain.
-- 2. Affected by rollbacks (or said differently, tied to a 'SlotNo').
putDelegationCertificate
  :: DelegationCertificate
  -> SlotNo
  -> DeltaDelegations
putDelegationCertificate = error "TODO: putDelegationCertificate"


readDelegation :: SlotNo -> Delegations -> WalletDelegation
readDelegation = error "TODO: readDelegation"
