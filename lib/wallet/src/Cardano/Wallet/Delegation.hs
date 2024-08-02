{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wallet.Delegation
    ( joinStakePoolDelegationAction
    , guardJoin
    , guardQuit
    , guardVoting
    , quitStakePoolDelegationAction
    , joinDRepVotingAction
    , DelegationRequest(..)
    , VoteRequest (..)
    ) where

import Prelude

import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Wallet
    ( ErrCannotQuit (..)
    , ErrCannotVote (..)
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
import Data.Bifunctor
    ( second
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Maybe
    ( fromJust
    , isNothing
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

data VoteRequest = NotVotedYet | VotedSameAsBefore | VotedDifferently | NotVotedThisTime
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
    -> VoteRequest
    -> Either
        ErrStakePoolDelegation
        (Tx.DelegationAction, Maybe Tx.VotingAction)
joinStakePoolDelegationAction
    era wallet currentEpochSlotting knownPools poolId poolStatus votingRequest
  = case guardJoin era knownPools delegation poolId retirementInfo votingRequest of
        Left e -> Left $ ErrStakePoolJoin e
        Right () -> Right
            ( if stakeKeyIsRegistered
                then Tx.Join poolId
                else Tx.JoinRegisteringKey poolId
            , case era of
                Write.RecentEraBabbage -> Nothing
                Write.RecentEraConway ->
                    if not stakeKeyIsRegistered then
                        Just $ Tx.VoteRegisteringKey Abstain
                    else if votingRequest /= NotVotedThisTime then
                        Just $ Tx.Vote Abstain
                    else
                        Nothing
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
    -> VoteRequest
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
        (Write.RecentEraConway, NotVotedYet) ->
            pure ()
        (Write.RecentEraConway, NotVotedThisTime) ->
            Left (ErrAlreadyDelegating pid)
        (Write.RecentEraConway, VotedSameAsBefore) ->
            Left (ErrAlreadyDelegatingVoting pid)
        (Write.RecentEraConway, VotedDifferently) ->
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

guardVoting
    :: Maybe DelegationRequest
    -> Maybe (Bool,DRep)
    -> Either ErrCannotVote ()
guardVoting optionalDelegationAction votingSameAgainM = do
    when (isNothing optionalDelegationAction && (fst <$> votingSameAgainM) == Just True ) $
        Left $ ErrAlreadyVoted $ snd (fromJust votingSameAgainM)

guardOnlyVoting
    :: Write.IsRecentEra era
    => Write.RecentEra era
    -> (Bool,DRep)
    -> Either ErrCannotVote ()
guardOnlyVoting era votingSameAgain = do
    when (fst votingSameAgain) $
        Left $ ErrAlreadyVoted $ snd votingSameAgain

    case era of
        Write.RecentEraBabbage ->
            Left ErrWrongEra
        Write.RecentEraConway ->
            Right ()

joinDRepVotingAction
    :: Write.IsRecentEra era
    => Write.RecentEra era
    -> DRep
    -> W.WalletDelegation
    -> Bool
    -> Either ErrCannotVote Tx.VotingAction
joinDRepVotingAction era action dlg stakeKeyIsRegistered =
    second (const vAction) $ guardOnlyVoting era votingRequest
  where
    isDRepSame (W.Voting drep) = drep == action
    isDRepSame (W.DelegatingVoting _ drep) = drep == action
    isDRepSame _ = False

    isSameNext (W.WalletDelegationNext _ deleg) = isDRepSame deleg

    sameWalletDelegation (W.WalletDelegation current coming) =
        isDRepSame current || any isSameNext coming

    (vAction, votingRequest) =
        if stakeKeyIsRegistered
        then (Tx.Vote action, (sameWalletDelegation dlg, action))
        else (Tx.VoteRegisteringKey action, (sameWalletDelegation dlg,action))
