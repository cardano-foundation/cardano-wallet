{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wallet.Delegation
    ( joinStakePoolDelegationAction
    , guardJoin
    , guardQuit
    , quitStakePoolDelegationAction
    , DelegationRequest(..)
    ) where

import Prelude

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
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
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
    , when
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Set
    ( Set
    )

import qualified Cardano.Wallet.DB.Store.Delegations.Layer as Dlgs
import qualified Cardano.Wallet.DB.WalletState as WalletState
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Transaction as Tx
import qualified Data.Set as Set
import qualified Internal.Cardano.Write.Tx as Write

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
    :: Write.IsRecentEra era
    => Write.RecentEra era
    -> WalletState.WalletState s
    -> CurrentEpochSlotting
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> Maybe Bool
    -> Either
        ErrStakePoolDelegation
        (Tx.DelegationAction, Maybe Tx.VotingAction)
joinStakePoolDelegationAction
    era wallet currentEpochSlotting knownPools poolId poolStatus votedTheSameM
  = case guardJoin era knownPools delegation poolId retirementInfo votedTheSameM of
        Left e -> Left $ ErrStakePoolJoin e
        Right () -> Right
            ( if stakeKeyIsRegistered
                then Tx.Join poolId
                else Tx.JoinRegisteringKey poolId
            , case era of
                Write.RecentEraBabbage -> Nothing
                Write.RecentEraConway -> Just $
                    if stakeKeyIsRegistered
                    then Tx.Vote Abstain
                    else Tx.VoteRegisteringKey Abstain
            )
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
    :: Write.IsRecentEra era
    => Write.RecentEra era
    -> Set PoolId
    -> WalletDelegation
    -> PoolId
    -> Maybe PoolRetirementEpochInfo
    -> Maybe Bool
    -> Either ErrCannotJoin ()
guardJoin era knownPools delegation pid mRetirementEpochInfo votedTheSameM = do
    when (pid `Set.notMember` knownPools) $
        Left (ErrNoSuchPool pid)

    forM_ mRetirementEpochInfo $ \info ->
        when (currentEpoch info >= retirementEpoch info) $
            Left (ErrNoSuchPool pid)

    when ((null next) && isDelegatingTo (== pid) active) eraVotingLogic

    when (not (null next) && isDelegatingTo (== pid) (last next)) eraVotingLogic
  where
    WalletDelegation {active, next} = delegation
    eraVotingLogic = case (era, votedTheSameM) of
        (Write.RecentEraBabbage,_) ->
            Left (ErrAlreadyDelegating pid)
        (Write.RecentEraConway, Nothing) ->
            Left (ErrAlreadyDelegating pid)
        (Write.RecentEraConway, Just True) ->
            Left (ErrAlreadyDelegatingVoting pid)
        (Write.RecentEraConway, Just False) ->
            pure ()

{-----------------------------------------------------------------------------
    Quit stake pool
------------------------------------------------------------------------------}
-- | Given the state of the wallet,
-- return a 'DelegationAction' for quitting the current stake pool.
quitStakePoolDelegationAction
    :: forall s. ()
    => WalletState.WalletState s
    -> Coin
    -- ^ Reward balance of the wallet
    -> CurrentEpochSlotting
    -> Withdrawal
    -> Either ErrStakePoolDelegation Tx.DelegationAction
quitStakePoolDelegationAction wallet rewards currentEpochSlotting withdrawal =
    case guardQuit delegation withdrawal rewards voting of
        Left e -> Left $ ErrStakePoolQuit e
        Right () -> Right Tx.Quit
  where
    voting =
        Dlgs.isVoting
        $ WalletState.delegations wallet
    delegation =
        Dlgs.readDelegation currentEpochSlotting
        $ WalletState.delegations wallet

guardQuit
    :: WalletDelegation
    -> Withdrawal
    -> Coin
    -> Bool
    -> Either ErrCannotQuit ()
guardQuit WalletDelegation{active,next} wdrl rewards voting = do
    let last_ = maybe active (view #status) $ lastMay next
    let anyone _ = True
    when (not (isDelegatingTo anyone last_) && not voting)
        $ Left ErrNotDelegatingOrAboutTo
    case wdrl of
        WithdrawalSelf {} -> Right ()
        _
            | rewards == Coin 0  -> Right ()
            | otherwise          -> Left $ ErrNonNullRewards rewards
