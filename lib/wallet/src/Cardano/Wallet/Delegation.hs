{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Delegation
  ( joinStakePoolDelegationAction
  , joinStakePool
  , guardJoin
  , quitStakePool
  , guardQuit
  , quitStakePoolDelegationAction
  , DelegationRequest (..)
  , handleDelegationRequest
  )
where

import Cardano.Pool.Types
  ( PoolId (..)
  )
import Cardano.Wallet
  ( ErrCannotQuit (..)
  , ErrStakePoolDelegation (..)
  , PoolRetirementEpochInfo (..)
  , WalletException (..)
  , WalletLog (..)
  , fetchRewardBalance
  , getCurrentEpochSlotting
  , isStakeKeyRegistered
  , readDelegation
  , readRewardAccount
  , transactionExpirySlot
  )
import Cardano.Wallet.Address.Derivation.Shelley
  ( ShelleyKey (..)
  )
import Cardano.Wallet.Address.Discovery.Sequential
  ( SeqState (..)
  )
import Cardano.Wallet.DB
  ( DBLayer (..)
  )
import Cardano.Wallet.DB.Store.Delegations.Layer
  ( CurrentEpochSlotting
  )
import Cardano.Wallet.Network
  ( NetworkLayer (..)
  )
import Cardano.Wallet.Primitive.Slotting
  ( PastHorizonException
  , TimeInterpreter
  )
import Cardano.Wallet.Primitive.Types
  ( IsDelegatingTo (..)
  , PoolLifeCycleStatus
  , WalletDelegation (..)
  )
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..)
  )
import Cardano.Wallet.Transaction
  ( ErrCannotJoin (..)
  , TransactionCtx
  , Withdrawal (..)
  , defaultTransactionCtx
  , txDelegationAction
  , txValidityInterval
  , txWithdrawal
  )
import Cardano.Wallet.Transaction qualified as Tx
import Control.Error
  ( lastMay
  )
import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( forM_
  , unless
  , when
  , (>=>)
  )
import Control.Monad.Except
  ( ExceptT
  , runExceptT
  )
import Control.Monad.IO.Class
  ( MonadIO (..)
  )
import Control.Monad.Trans.Except
  ( except
  )
import Control.Tracer
  ( Tracer
  , traceWith
  )
import Data.Generics.Internal.VL.Lens
  ( view
  , (^.)
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Prelude

-- | The data type that represents client's delegation request.
-- Stake key registration is made implicit by design:
-- the library figures out if stake key needs to be registered first
-- so that clients don't have to worry about this concern.
data DelegationRequest
  = -- | Delegate to a pool using the default staking key (derivation index 0),
    -- registering the stake key if needed.
    Join PoolId
  | -- | Stop delegating if the wallet is delegating.
    Quit
  deriving (Eq, Show)

handleDelegationRequest
  :: forall s
   . Tracer IO WalletLog
  -> DBLayer IO s
  -> CurrentEpochSlotting
  -> IO (Set PoolId)
  -> (PoolId -> IO PoolLifeCycleStatus)
  -> Withdrawal
  -> DelegationRequest
  -> ExceptT ErrStakePoolDelegation IO Tx.DelegationAction
handleDelegationRequest
  tr
  db
  currentEpochSlotting
  getKnownPools
  getPoolStatus
  withdrawal = \case
    Join poolId -> liftIO $ do
      poolStatus <- getPoolStatus poolId
      pools <- getKnownPools
      joinStakePoolDelegationAction
        tr
        db
        currentEpochSlotting
        pools
        poolId
        poolStatus
    Quit ->
      liftIO
        $ quitStakePoolDelegationAction db currentEpochSlotting withdrawal

joinStakePoolDelegationAction
  :: Tracer IO WalletLog
  -> DBLayer IO s
  -> CurrentEpochSlotting
  -> Set PoolId
  -> PoolId
  -> PoolLifeCycleStatus
  -> IO Tx.DelegationAction
joinStakePoolDelegationAction
  tr
  DBLayer {..}
  currentEpochSlotting
  knownPools
  poolId
  poolStatus = do
    (walletDelegation, stakeKeyIsRegistered) <-
      atomically
        $ (,)
          <$> readDelegation walletState
          <*> isStakeKeyRegistered walletState

    let
      retirementInfo =
        PoolRetirementEpochInfo (currentEpochSlotting ^. #currentEpoch)
          . view #retirementEpoch
          <$> W.getPoolRetirementCertificate poolStatus

    throwInIO ErrStakePoolJoin . except
      $ guardJoin
        knownPools
        (walletDelegation currentEpochSlotting)
        poolId
        retirementInfo

    traceWith tr $ MsgIsStakeKeyRegistered stakeKeyIsRegistered

    pure
      $ if stakeKeyIsRegistered
        then Tx.Join poolId
        else Tx.JoinRegisteringKey poolId
    where
      throwInIO
        :: MonadIO m => (e -> ErrStakePoolDelegation) -> ExceptT e m a -> m a
      throwInIO f =
        runExceptT
          >=> either (liftIO . throwIO . ExceptionStakePoolDelegation . f) pure

joinStakePool
  :: Tracer IO WalletLog
  -> TimeInterpreter (ExceptT PastHorizonException IO)
  -> DBLayer IO s
  -> CurrentEpochSlotting
  -> Set PoolId
  -> PoolId
  -> PoolLifeCycleStatus
  -> IO TransactionCtx
joinStakePool tr ti db currentEpochSlotting pools poolId poolStatus = do
  action <-
    joinStakePoolDelegationAction
      tr
      db
      currentEpochSlotting
      pools
      poolId
      poolStatus
  ttl <- transactionExpirySlot ti Nothing
  pure
    defaultTransactionCtx
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
  when (pid `Set.notMember` knownPools)
    $ Left (ErrNoSuchPool pid)

  forM_ mRetirementEpochInfo $ \info ->
    when (currentEpoch info >= retirementEpoch info)
      $ Left (ErrNoSuchPool pid)

  when ((null next) && isDelegatingTo (== pid) active)
    $ Left (ErrAlreadyDelegating pid)

  when (not (null next) && isDelegatingTo (== pid) (last next))
    $ Left (ErrAlreadyDelegating pid)
  where
    WalletDelegation {active, next} = delegation

-- | Helper function to factor necessary logic for quitting a stake pool.
quitStakePoolDelegationAction
  :: forall s
   . DBLayer IO s
  -> CurrentEpochSlotting
  -> Withdrawal
  -> IO Tx.DelegationAction
quitStakePoolDelegationAction db@DBLayer {..} currentEpochSlotting withdrawal = do
  delegation <- atomically $ readDelegation walletState
  rewards <- liftIO $ fetchRewardBalance db
  either
    (throwIO . ExceptionStakePoolDelegation . ErrStakePoolQuit)
    pure
    (guardQuit (delegation currentEpochSlotting) withdrawal rewards)
  pure Tx.Quit

quitStakePool
  :: forall n block
   . NetworkLayer IO block
  -> DBLayer IO (SeqState n ShelleyKey)
  -> TimeInterpreter (ExceptT PastHorizonException IO)
  -> IO TransactionCtx
quitStakePool netLayer db timeInterpreter = do
  (rewardAccount, _, derivationPath) <- readRewardAccount db
  withdrawal <-
    WithdrawalSelf rewardAccount derivationPath
      <$> getCachedRewardAccountBalance netLayer rewardAccount
  currentEpochSlotting <- getCurrentEpochSlotting netLayer
  action <- quitStakePoolDelegationAction db currentEpochSlotting withdrawal
  ttl <- transactionExpirySlot timeInterpreter Nothing
  pure
    defaultTransactionCtx
      { txWithdrawal = withdrawal
      , txValidityInterval = (Nothing, ttl)
      , txDelegationAction = Just action
      }

guardQuit :: WalletDelegation -> Withdrawal -> Coin -> Either ErrCannotQuit ()
guardQuit WalletDelegation {active, next} wdrl rewards = do
  let
    last_ = maybe active (view #status) $ lastMay next
  let
    anyone _ = True
  unless (isDelegatingTo anyone last_) $ Left ErrNotDelegatingOrAboutTo
  case wdrl of
    WithdrawalSelf {} -> Right ()
    _
      | rewards == Coin 0 -> Right ()
      | otherwise -> Left $ ErrNonNullRewards rewards
