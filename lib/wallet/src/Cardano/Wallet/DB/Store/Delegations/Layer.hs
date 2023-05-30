{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.Layer
    ( isStakeKeyRegistered
    , putDelegationCertificate
    , readDelegation
    , ReadDelegationSlots (..)
    , mkReadDelegationSlots
    )
where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations, DeltaDelegations )
import Cardano.Wallet.Delegation.Model
    ( Operation (..), Status (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, firstSlotInEpoch, interpretQuery )
import Cardano.Wallet.Primitive.Types
    ( DelegationCertificate (..)
    , EpochNo
    , SlotNo
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    )
import Data.Foldable
    ( find )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( lookupMax )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( catMaybes, fromMaybe )

-- | Check whether the stake key is registered in the delegation state.
isStakeKeyRegistered :: Delegations -> Bool
isStakeKeyRegistered m = fromMaybe False $ do
    (_, v) <- lookupMax m
    pure $ v /= Inactive

-- | Binds a stake pool id to a wallet. This will have an influence on
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
putDelegationCertificate cert sl = case cert of
    CertDelegateNone _ -> [Deregister sl]
    CertDelegateFull _ pool -> [Delegate pool sl, Register sl]
    CertRegisterKey _ -> [Register sl]

-- | Arguments to 'readDelegation'.
data ReadDelegationSlots = ReadDelegationSlots
    { currentEpoch :: EpochNo
    -- ^ The current epoch.
    , currentEpochStartSlot :: SlotNo
    -- ^ The current epoch start slot.
    , previousEpochStartSlot :: Maybe SlotNo
    -- ^ The previous epoch start slot, if any.
    }

-- | Read the delegation status of a wallet.
readDelegation :: ReadDelegationSlots -> Delegations -> WalletDelegation
readDelegation (ReadDelegationSlots epoch cur Nothing) hist =
    WalletDelegation currentDelegation nextDelegations
  where
    currentDelegation = NotDelegating
    nextDelegations =
        catMaybes
            [ nextDelegation (epoch + 2)
                $ readDelegationStatus (>= cur) hist
            ]
readDelegation (ReadDelegationSlots epoch cur (Just prev)) hist =
    WalletDelegation currentDelegation nextDelegations
  where
    currentDelegation = readDelegationStatus (< prev) hist
        & fromMaybe NotDelegating
    nextDelegations =
        catMaybes
            [ nextDelegation (epoch + 1)
                $ readDelegationStatus (\sl -> sl >= prev && sl < cur) hist
            , nextDelegation (epoch + 2)
                $ readDelegationStatus (>= cur) hist
            ]

nextDelegation
    :: Functor f
    => EpochNo
    -> f WalletDelegationStatus
    -> f WalletDelegationNext
nextDelegation = fmap . WalletDelegationNext

readDelegationStatus
    :: (SlotNo -> Bool)
    -> Delegations
    -> Maybe WalletDelegationStatus
readDelegationStatus cond =
    fmap (walletDelegationStatus . snd)
        . find (cond . fst)
        . reverse
        . Map.assocs

walletDelegationStatus :: Status PoolId -> WalletDelegationStatus
walletDelegationStatus = \case
    Inactive -> NotDelegating
    Registered -> NotDelegating
    Active pid -> Delegating pid

-- | Construct 'ReadDelegationSlots' from an 'EpochNo' using a 'TimeInterpreter'
-- .
mkReadDelegationSlots
    :: forall m
     . Monad m
    => TimeInterpreter m
    -> EpochNo
    -> m ReadDelegationSlots
mkReadDelegationSlots ti epoch =
    ReadDelegationSlots epoch
        <$> slotOf epoch
        <*> case epoch of
            0 -> pure Nothing
            epoch' -> Just <$> slotOf (epoch' - 1)
  where
    slotOf :: EpochNo -> m SlotNo
    slotOf = interpretQuery ti . firstSlotInEpoch
