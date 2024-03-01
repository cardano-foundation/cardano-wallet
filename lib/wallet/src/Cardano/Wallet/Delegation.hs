{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Delegation
    ( joinStakePoolDelegationAction
    , guardJoin
    , guardQuit
    , quitStakePoolDelegationAction
    , DelegationRequest(..)
    ) where

import Prelude

import qualified Cardano.Wallet.DB.Store.Delegations.Layer as Dlgs
import qualified Cardano.Wallet.DB.WalletState as WalletState
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Transaction as Tx
import qualified Data.Set as Set

import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Wallet
    ( ErrCannotQuit (..)
    , ErrStakePoolDelegation (..)
    , PoolRetirementEpochInfo (..)
    )
import Cardano.Wallet.DB.Store.Delegations.Layer
    ( CurrentEpochSlotting
    )
import Cardano.Wallet.Primitive.Types
    ( IsDelegatingTo (..)
    , PoolLifeCycleStatus
    , WalletDelegation (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Transaction
    ( ErrCannotJoin (..)
    , Withdrawal (..)
    )
import Control.Error
    ( lastMay
    )
import Control.Monad
    ( forM_
    , unless
    , when
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Set
    ( Set
    )

-- | The data type that represents client's delegation request.
-- Stake key registration is made implicit by design:
-- the library figures out if stake key needs to be registered first
-- so that clients don't have to worry about this concern.
data DelegationRequest
    = Join PoolId
    -- ^ Delegate to a pool using the default staking key (derivation index 0),
    -- registering the stake key if needed.
    | Quit
    -- ^ Stop delegating if the wallet is delegating.
    deriving (Eq, Show)

{-----------------------------------------------------------------------------
    Join stake pool
------------------------------------------------------------------------------}
joinStakePoolDelegationAction
    :: WalletState.WalletState s
    -> CurrentEpochSlotting
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> Either ErrStakePoolDelegation Tx.DelegationAction
joinStakePoolDelegationAction
    wallet currentEpochSlotting knownPools poolId poolStatus
  = case guardJoin knownPools delegation poolId retirementInfo of
        Left e -> Left $ ErrStakePoolJoin e
        Right () -> Right $
            if stakeKeyIsRegistered
            then Tx.Join poolId
            else Tx.JoinRegisteringKey poolId
  where
    stakeKeyIsRegistered =
        Dlgs.isStakeKeyRegistered
        $ WalletState.delegations wallet
    delegation =
        Dlgs.readDelegation currentEpochSlotting
        $ WalletState.delegations wallet
    retirementInfo =
        PoolRetirementEpochInfo (currentEpochSlotting ^. #currentEpoch)
            . view #retirementEpoch <$>
            W.getPoolRetirementCertificate poolStatus

guardJoin
    :: Set PoolId
    -> WalletDelegation
    -> PoolId
    -> Maybe PoolRetirementEpochInfo
    -> Either ErrCannotJoin ()
guardJoin knownPools delegation pid mRetirementEpochInfo = do
    when (pid `Set.notMember` knownPools) $
        Left (ErrNoSuchPool pid)

    forM_ mRetirementEpochInfo $ \info ->
        when (currentEpoch info >= retirementEpoch info) $
            Left (ErrNoSuchPool pid)

    when ((null next) && isDelegatingTo (== pid) active) $
        Left (ErrAlreadyDelegating pid)

    when (not (null next) && isDelegatingTo (== pid) (last next)) $
        Left (ErrAlreadyDelegating pid)
  where
    WalletDelegation {active, next} = delegation

{-----------------------------------------------------------------------------
    Quit stake pool
------------------------------------------------------------------------------}
-- | Given the state of the wallet,
-- return a 'DelegationAction' for quitting the current stake pool.
quitStakePoolDelegationAction
    :: forall s
     . WalletState.WalletState s
    -> Coin
    -- ^ Reward balance of the wallet
    -> CurrentEpochSlotting
    -> Withdrawal
    -> Either ErrStakePoolDelegation Tx.DelegationAction
quitStakePoolDelegationAction wallet rewards currentEpochSlotting withdrawal =
    case guardQuit delegation withdrawal rewards of
        Left e -> Left $ ErrStakePoolQuit e
        Right () -> Right Tx.Quit
  where
    delegation =
        Dlgs.readDelegation currentEpochSlotting
        $ WalletState.delegations wallet

guardQuit :: WalletDelegation -> Withdrawal -> Coin -> Either ErrCannotQuit ()
guardQuit WalletDelegation{active,next} wdrl rewards = do
    let last_ = maybe active (view #status) $ lastMay next
    let anyone _ = True
    unless (isDelegatingTo anyone last_) $ Left ErrNotDelegatingOrAboutTo
    case wdrl of
        WithdrawalSelf {} -> Right ()
        _
            | rewards == Coin 0  -> Right ()
            | otherwise          -> Left $ ErrNonNullRewards rewards
