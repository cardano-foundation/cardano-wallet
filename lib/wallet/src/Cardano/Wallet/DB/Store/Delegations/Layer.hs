{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.Delegations.Layer
    ( isStakeKeyRegistered
    , putDelegationCertificate
    , readDelegation
    , CurrentEpochSlotting (..)
    , mkCurrentEpochSlotting
    , getVoting
    )
where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet.DB.Store.Delegations.Model
    ( Delegations
    , DeltaDelegations
    )
import Cardano.Wallet.Delegation.Model
    ( Operation (..)
    , Status (..)
    , Transition (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , firstSlotInEpoch
    , interpretQuery
    )
import Cardano.Wallet.Primitive.Types
    ( DelegationCertificate (..)
    , EpochNo
    , SlotNo
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
import Data.Foldable
    ( find
    )
import Data.Map.Strict
    ( lookupMax
    )
import Data.Maybe
    ( catMaybes
    , fromMaybe
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Map.Strict as Map

-- | Check whether the stake key is registered in the delegation state.
isStakeKeyRegistered :: Delegations -> Bool
isStakeKeyRegistered m = fromMaybe False $ do
    (_, v) <- lookupMax m
    pure $ v /= Inactive

-- | Get the voting in the delegation state.
getVoting :: Delegations -> Maybe DRep
getVoting m = fromMaybe Nothing $ do
    (_, v) <- lookupMax m
    let votedAlready = \case
            Active (Just drep) _ -> Just drep
            _ -> Nothing
    pure $ votedAlready v

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
    CertDelegateNone _ -> ApplyTransition Deregister sl
    CertVoteAndDelegate _ pool drep -> ApplyTransition
        (VoteAndDelegate drep pool) sl

-- | Arguments to 'readDelegation'.
data CurrentEpochSlotting = CurrentEpochSlotting
    { currentEpoch :: EpochNo
    -- ^ The current epoch.
    , currentEpochStartSlot :: SlotNo
    -- ^ The current epoch start slot.
    , previousEpochStartSlot :: Maybe SlotNo
    -- ^ The previous epoch start slot, if any.
    }
    deriving (Eq, Show, Generic)

-- | Read the delegation status of a wallet.
readDelegation :: CurrentEpochSlotting -> Delegations -> WalletDelegation
readDelegation CurrentEpochSlotting{..} history =
    case previousEpochStartSlot of
        Nothing ->
            WalletDelegation
            { active = NotDelegating
            , next =
                catMaybes
                    [ WalletDelegationNext (currentEpoch + 2) <$>
                        readDelegationStatus (>= currentEpochStartSlot) history
                    ]
            }
        Just previousEpochStart ->
            WalletDelegation
            { active =
                fromMaybe NotDelegating $
                    readDelegationStatus (< previousEpochStart) history
            , next =
                catMaybes
                    [ WalletDelegationNext (currentEpoch + 1) <$>
                        let condition slot
                                = slot >= previousEpochStart
                                && slot < currentEpochStartSlot
                        in readDelegationStatus condition history
                    , WalletDelegationNext (currentEpoch + 2) <$>
                        readDelegationStatus (>= currentEpochStartSlot) history
                    ]
            }
  where
    readDelegationStatus ::
        (SlotNo -> Bool) -> Delegations -> Maybe WalletDelegationStatus
    readDelegationStatus cond =
        (walletDelegationStatus . snd <$>) . find (cond . fst) . Map.toDescList

    walletDelegationStatus :: Status DRep PoolId -> WalletDelegationStatus
    walletDelegationStatus = \case
        Inactive -> NotDelegating
        Active Nothing Nothing  -> NotDelegating
        Active Nothing (Just pid) -> Delegating pid
        Active (Just vote) Nothing -> Voting vote
        Active (Just vote) (Just pid) -> DelegatingVoting pid vote

-- | Construct 'CurrentEpochSlotting' from an 'EpochNo' using a 'TimeInterpreter'
-- .
mkCurrentEpochSlotting
    :: forall m
     . Monad m
    => TimeInterpreter m
    -> EpochNo
    -> m CurrentEpochSlotting
mkCurrentEpochSlotting ti epoch =
    CurrentEpochSlotting epoch
        <$> slotOf epoch
        <*> case epoch of
            0 -> pure Nothing
            epoch' -> Just <$> slotOf (epoch' - 1)
  where
    slotOf :: EpochNo -> m SlotNo
    slotOf = interpretQuery ti . firstSlotInEpoch
