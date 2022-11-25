{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Delegation
    ( joinStakePoolDelegationAction
    , guardJoin
    , quitStakePool
    , guardQuit
    , quitStakePoolDelegationAction
    , DelegationRequest(..)
    , handleDelegationRequest
    ) where

import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Set as Set

import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Wallet
    ( ErrCannotQuit (..)
    , ErrNoSuchWallet (..)
    , ErrStakePoolDelegation (..)
    , PoolRetirementEpochInfo (..)
    , WalletException (..)
    , WalletLog (..)
    , fetchRewardBalance
    , readRewardAccount
    , transactionExpirySlot
    , withNoSuchWallet
    )
import Cardano.Wallet.DB
    ( DBLayer (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , IsDelegatingTo (..)
    , PoolLifeCycleStatus
    , ProtocolParameters
    , WalletDelegation (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrCannotJoin (..)
    , TransactionCtx
    , Withdrawal (..)
    , defaultTransactionCtx
    , txDelegationAction
    , txValidityInterval
    , txWithdrawal
    )
import Control.Error
    ( lastMay )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forM_, unless, when )
import Control.Monad.Except
    ( ExceptT, mapExceptT, runExceptT, withExceptT )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( except )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Set
    ( Set )

-- | The data type that represents client's delegation request.
-- Stake key registration is made implicit by design:
-- the library figures out if stake key needs to be registered first
-- so that clients don't have to worry about this concern.
data DelegationRequest
    = StartDelegatingRegisteringKey PoolId
    -- ^ Delegate to a pool using the default staking key (derivation index 0),
    -- registering the stake key if needed.
    | StopDelegating
    -- ^ Stop delegating if the wallet is delegating.
    deriving (Eq, Show)

handleDelegationRequest
    :: forall s k
     . Tracer IO WalletLog
    -> DBLayer IO s k
    -> ProtocolParameters
    -> W.EpochNo
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> WalletId
    -> Withdrawal
    -> DelegationRequest
    -> ExceptT ErrStakePoolDelegation IO DelegationAction
handleDelegationRequest
    tr db pp currEpoch getKnownPools getPoolStatus walletId withdrawal = \case
    StartDelegatingRegisteringKey poolId -> do
        poolStatus <- liftIO $ getPoolStatus poolId
        pools <- liftIO getKnownPools
        joinStakePoolDelegationAction
            tr db pp currEpoch pools poolId poolStatus walletId
    StopDelegating ->
        liftIO $ quitStakePoolDelegationAction db pp walletId withdrawal

joinStakePoolDelegationAction
    :: forall s k
     . Tracer IO WalletLog
    -> DBLayer IO s k
    -> ProtocolParameters
    -> W.EpochNo
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> WalletId
    -> ExceptT ErrStakePoolDelegation IO DelegationAction
joinStakePoolDelegationAction
    tr DBLayer{..} pp currentEpoch knownPools poolId poolStatus wid = do
    (walletDelegation, stakeKeyIsRegistered) <-
        mapExceptT atomically $
            withExceptT ErrStakePoolDelegationNoSuchWallet $
                (,) <$> withNoSuchWallet wid (fmap snd <$> readWalletMeta wid)
                    <*> isStakeKeyRegistered wid

    let retirementInfo =
            PoolRetirementEpochInfo currentEpoch . view #retirementEpoch <$>
                W.getPoolRetirementCertificate poolStatus

    withExceptT ErrStakePoolJoin $ except $
        guardJoin knownPools walletDelegation poolId retirementInfo

    liftIO $ traceWith tr $ MsgIsStakeKeyRegistered stakeKeyIsRegistered

    pure $
        if stakeKeyIsRegistered
        then Join poolId
        else JoinRegsteringKey poolId (W.stakeKeyDeposit pp)

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

-- | Helper function to factor necessary logic for quitting a stake pool.
quitStakePoolDelegationAction
    :: forall s k
     . DBLayer IO s k
    -> ProtocolParameters
    -> WalletId
    -> Withdrawal
    -> IO DelegationAction
quitStakePoolDelegationAction db@DBLayer{..} pp walletId withdrawal = do
    (_, delegation) <- atomically (readWalletMeta walletId)
        >>= maybe
            (throwIO (ExceptionStakePoolDelegation
                (ErrStakePoolDelegationNoSuchWallet
                    (ErrNoSuchWallet walletId))))
            pure
    rewards <- liftIO $ fetchRewardBalance @s @k db walletId
    either (throwIO . ExceptionStakePoolDelegation . ErrStakePoolQuit) pure
        (guardQuit delegation withdrawal rewards)
    pure $ Quit $ W.stakeKeyDeposit pp

quitStakePool
    :: forall (n :: NetworkDiscriminant)
     . NetworkLayer IO Block
    -> DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> ProtocolParameters
    -> TimeInterpreter (ExceptT PastHorizonException IO)
    -> WalletId
    -> IO TransactionCtx
quitStakePool netLayer db pp timeInterpreter walletId = do
    (rewardAccount, _, derivationPath) <-
        runExceptT (readRewardAccount db walletId)
            >>= either (throwIO . ExceptionReadRewardAccount) pure
    withdrawal <- WithdrawalSelf rewardAccount derivationPath
        <$> getCachedRewardAccountBalance netLayer rewardAccount
    action <- quitStakePoolDelegationAction db pp walletId withdrawal
    ttl <- transactionExpirySlot timeInterpreter  Nothing
    pure defaultTransactionCtx
        { txWithdrawal = withdrawal
        , txValidityInterval = (Nothing, ttl)
        , txDelegationAction = Just action
        }

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
