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
    , joinStakePool
    , guardJoin
    , quitStakePool
    , guardQuit
    , quitStakePoolDelegationAction
    , DelegationRequest(..)
    , handleDelegationRequest
    ) where

import Prelude

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Transaction as Tx
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
    , mkNoSuchWalletError
    , readRewardAccount
    , transactionExpirySlot
    , withNoSuchWallet
    )
import Cardano.Wallet.DB
    ( DBLayer (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( IsDelegatingTo (..)
    , PoolLifeCycleStatus
    , WalletDelegation (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Transaction
    ( ErrCannotJoin (..)
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
    ( forM_, unless, when, (>=>) )
import Control.Monad.Except
    ( ExceptT, runExceptT )
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
    = Join PoolId
    -- ^ Delegate to a pool using the default staking key (derivation index 0),
    -- registering the stake key if needed.
    | Quit
    -- ^ Stop delegating if the wallet is delegating.
    deriving (Eq, Show)

handleDelegationRequest
    :: forall s k
     . Tracer IO WalletLog
    -> DBLayer IO s k
    -> W.EpochNo
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> WalletId
    -> Withdrawal
    -> DelegationRequest
    -> ExceptT ErrStakePoolDelegation IO Tx.DelegationAction
handleDelegationRequest
    tr db currEpoch getKnownPools getPoolStatus walletId withdrawal = \case
    Join poolId -> liftIO $ do
        poolStatus <- getPoolStatus poolId
        pools <- getKnownPools
        joinStakePoolDelegationAction
            tr db currEpoch pools poolId poolStatus walletId
    Quit -> liftIO $ quitStakePoolDelegationAction db walletId withdrawal

joinStakePoolDelegationAction
    :: Tracer IO WalletLog
    -> DBLayer IO s k
    -> W.EpochNo
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> WalletId
    -> IO Tx.DelegationAction
joinStakePoolDelegationAction
    tr DBLayer{..} currentEpoch knownPools poolId poolStatus wid = do
    (walletDelegation, stakeKeyIsRegistered) <-
        atomically . throwInIO ErrStakePoolDelegationNoSuchWallet $
            (,) <$> withNoSuchWallet wid (fmap snd <$> readWalletMeta)
                <*> mkNoSuchWalletError wid isStakeKeyRegistered

    let retirementInfo =
            PoolRetirementEpochInfo currentEpoch . view #retirementEpoch <$>
                W.getPoolRetirementCertificate poolStatus

    throwInIO ErrStakePoolJoin . except $
        guardJoin knownPools walletDelegation poolId retirementInfo

    traceWith tr $ MsgIsStakeKeyRegistered stakeKeyIsRegistered

    pure $
        if stakeKeyIsRegistered
        then Tx.Join poolId
        else Tx.JoinRegisteringKey poolId

  where
    throwInIO ::
        MonadIO m => (e -> ErrStakePoolDelegation) -> ExceptT e m a -> m a
    throwInIO f = runExceptT >=>
        either (liftIO . throwIO . ExceptionStakePoolDelegation . f) pure

joinStakePool
    :: Tracer IO WalletLog
    -> TimeInterpreter (ExceptT PastHorizonException IO)
    -> DBLayer IO s k
    -> W.EpochNo
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> WalletId
    -> IO TransactionCtx
joinStakePool tr ti db curEpoch pools poolId poolStatus walletId = do
    action <- joinStakePoolDelegationAction
        tr
        db
        curEpoch
        pools
        poolId
        poolStatus
        walletId
    ttl <- transactionExpirySlot ti Nothing
    pure defaultTransactionCtx
        { txWithdrawal = NoWithdrawal
        , txValidityInterval = (Nothing, ttl)
        , txDelegationAction = Just action
        }

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
    -> WalletId
    -> Withdrawal
    -> IO Tx.DelegationAction
quitStakePoolDelegationAction db@DBLayer{..} walletId withdrawal = do
    (_, delegation) <- atomically readWalletMeta
        >>= maybe
            (throwIO (ExceptionStakePoolDelegation
                (ErrStakePoolDelegationNoSuchWallet
                    (ErrNoSuchWallet walletId))))
            pure
    rewards <- liftIO $ fetchRewardBalance @s @k db walletId
    either (throwIO . ExceptionStakePoolDelegation . ErrStakePoolQuit) pure
        (guardQuit delegation withdrawal rewards)
    pure Tx.Quit

quitStakePool
    :: forall n block
     . NetworkLayer IO block
    -> DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> TimeInterpreter (ExceptT PastHorizonException IO)
    -> WalletId
    -> IO TransactionCtx
quitStakePool netLayer db timeInterpreter walletId = do
    (rewardAccount, _, derivationPath) <-
        runExceptT (readRewardAccount db walletId)
            >>= either (throwIO . ExceptionReadRewardAccount) pure
    withdrawal <- WithdrawalSelf rewardAccount derivationPath
        <$> getCachedRewardAccountBalance netLayer rewardAccount
    action <- quitStakePoolDelegationAction db walletId withdrawal
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
