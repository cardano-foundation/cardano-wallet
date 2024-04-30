{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Delegation functionality used by Daedalus.
--
module Cardano.Wallet.IO.Delegation
    ( handleDelegationVoteRequest
    , selectCoinsForJoin
    , selectCoinsForQuit
    , joinStakePool
    , quitStakePool
    )
    where

import Prelude

import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Wallet
    ( WalletException (..)
    , WalletLayer (..)
    , WalletLog (..)
    , dbLayer
    , isStakeKeyRegistered
    , logger
    , networkLayer
    , readDelegation
    , transactionLayer
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso
    )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , delegationAddressS
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Discovery
    ( GenChange (..)
    , IsOurs
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
import Cardano.Wallet.Delegation
    ( VoteRequest (..)
    )
import Cardano.Wallet.Flavor
    ( Excluding
    , WalletFlavor (..)
    , keyOfWallet
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase
    )
import Cardano.Wallet.Primitive.Types
    ( PoolLifeCycleStatus
    , ProtocolParameters (..)
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Cardano.Wallet.Transaction
    ( PreSelection (..)
    , TransactionCtx (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Control.Exception
    ( throwIO
    )
import Control.Monad
    ( forM
    )
import Control.Tracer
    ( traceWith
    )
import Data.DBVar
    ( readDBVar
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Maybe
    ( fromJust
    )
import Data.Set
    ( Set
    )
import Data.Time.Clock
    ( UTCTime
    )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Delegation as WD
import qualified Cardano.Wallet.Transaction as Tx
import qualified Internal.Cardano.Write.Tx as Write

{-----------------------------------------------------------------------------
    Used by constructTransaction
------------------------------------------------------------------------------}
handleDelegationVoteRequest
    :: forall s
     . WalletLayer IO s
    -> CurrentEpochSlotting
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> Withdrawal
    -> Maybe WD.DelegationRequest
    -> Maybe DRep
    -> IO (Maybe Tx.DelegationAction, Maybe Tx.VotingAction)
handleDelegationVoteRequest
    ctx currentEpochSlotting getKnownPools getPoolStatus withdrawal
    delRequestM drepM = do
        (optionalVoteAction, votingRequest) <- case drepM of
            Just action -> do
                (vAction, votingRequest) <- voteAction ctx action
                pure (Just vAction, votingRequest)
            Nothing ->
                pure (Nothing, NotVotedYet)
        optionalDelegationAction <- forM delRequestM $
            handleDelegationRequest ctx currentEpochSlotting getKnownPools
                getPoolStatus withdrawal votingRequest

        either (throwIO . ExceptionVoting) pure
            (WD.guardVoting delRequestM $ toDrepEnriched votingRequest)
        pure (optionalDelegationAction, optionalVoteAction)
  where
    toDrepEnriched NotVotedYet = Nothing
    toDrepEnriched VotedSameLikeBefore = Just (True, fromJust drepM)
    toDrepEnriched VotedDifferently = Just (False, fromJust drepM)

handleDelegationRequest
    :: forall s
     . WalletLayer IO s
    -> CurrentEpochSlotting
    -> IO (Set PoolId)
    -> (PoolId -> IO PoolLifeCycleStatus)
    -> Withdrawal
    -> VoteRequest
    -> WD.DelegationRequest
    -> IO Tx.DelegationAction
handleDelegationRequest
    ctx currentEpochSlotting getKnownPools getPoolStatus withdrawal voteRequest = \case
    WD.Join poolId -> do
        poolStatus <- getPoolStatus poolId
        pools <- getKnownPools
        fst <$> joinStakePoolDelegationAction
            ctx
            currentEpochSlotting
            pools
            poolId
            poolStatus
            voteRequest
    WD.Quit ->
        quitStakePoolDelegationAction
            ctx
            currentEpochSlotting
            withdrawal

voteAction
    :: WalletLayer IO s
    -> DRep
    -> IO (Tx.VotingAction, VoteRequest)
voteAction ctx action = do
    currentEpochSlotting <- W.getCurrentEpochSlotting netLayer
    (calculateWalletDelegations, stakeKeyIsRegistered) <-
        db & \DBLayer{atomically,walletState} -> atomically $
            (,) <$> readDelegation walletState
                <*> W.isStakeKeyRegistered walletState
    let dlg = calculateWalletDelegations currentEpochSlotting

    traceWith tr $ W.MsgWallet $ MsgIsStakeKeyRegistered stakeKeyIsRegistered

    pure $
        if stakeKeyIsRegistered
        then (Tx.Vote action, sameWalletDelegation dlg)
        else (Tx.VoteRegisteringKey action, sameWalletDelegation dlg)
  where
    db = ctx ^. dbLayer
    tr = ctx ^. logger
    netLayer = ctx ^. networkLayer

    isDRepSame (Voting drep) = drep == action
    isDRepSame (DelegatingVoting _ drep) = drep == action
    isDRepSame _ = False

    isSameNext (WalletDelegationNext _ deleg) = isDRepSame deleg

    sameWalletDelegation (WalletDelegation current coming) =
        if isDRepSame current || any isSameNext coming then
            VotedSameLikeBefore
        else
            VotedDifferently

{-----------------------------------------------------------------------------
    Used by Daedalus
------------------------------------------------------------------------------}
-- | Perform a coin selection for a transaction that joins a stake pool.
selectCoinsForJoin
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressBookIso s
        , Seq.SupportsDiscovery n k
        , DelegationAddress k 'CredFromKeyK
        )
    => WalletLayer IO s
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> IO W.CoinSelection
selectCoinsForJoin ctx pools poolId poolStatus = do
    (Write.PParamsInAnyRecentEra era pp, timeTranslation)
        <- W.readNodeTipStateForTxWrite netLayer
    currentEpochSlotting <- W.getCurrentEpochSlotting netLayer

    (action, _) <- joinStakePoolDelegationAction
        ctx
        currentEpochSlotting
        pools
        poolId
        poolStatus
        NotVotedYet

    let changeAddrGen = W.defaultChangeAddressGen (delegationAddressS @n)

    optionalVoteAction <-
        W.handleVotingWhenMissingInConway era db

    let txCtx = defaultTransactionCtx
            { txDelegationAction = Just action
            , txVotingAction = optionalVoteAction
            , txDeposit = Just $ W.getStakeKeyDeposit pp
            }

    let paymentOuts = []

    (tx, walletState) <-
        W.buildTransaction @s era
            db timeTranslation changeAddrGen pp txCtx paymentOuts

    pure
        $ W.buildCoinSelectionForTransaction @s @n
            walletState
            paymentOuts
            (W.getStakeKeyDeposit pp)
            (Just action)
            tx
  where
    db = ctx ^. dbLayer
    netLayer = ctx ^. networkLayer

-- | Perform a coin selection for a transactions that quits a stake pool.
selectCoinsForQuit
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressBookIso s
        , Seq.SupportsDiscovery n k
        , DelegationAddress k 'CredFromKeyK
        )
    => WalletLayer IO s
    -> IO W.CoinSelection
selectCoinsForQuit ctx = do
    (Write.PParamsInAnyRecentEra era pp, timeTranslation)
        <- W.readNodeTipStateForTxWrite netLayer
    currentEpochSlotting <- W.getCurrentEpochSlotting netLayer

    withdrawal <- W.shelleyOnlyMkSelfWithdrawal
        netLayer
        (W.txWitnessTagForKey $ keyOfWallet $ walletFlavor @s)
        db

    action <-
        quitStakePoolDelegationAction ctx currentEpochSlotting withdrawal

    let changeAddrGen = W.defaultChangeAddressGen (delegationAddressS @n)

    let txCtx = defaultTransactionCtx
            { txDelegationAction = Just action
            , txWithdrawal = withdrawal
            , txDeposit = Just $ W.getStakeKeyDeposit pp
            }

    let paymentOuts = []

    (tx, walletState) <-
        W.buildTransaction @s era
            db timeTranslation changeAddrGen pp txCtx paymentOuts

    pure
        $ W.buildCoinSelectionForTransaction @s @n
            walletState
            paymentOuts
            (W.getStakeKeyDeposit pp)
            (Just action)
            tx
  where
    db = ctx ^. dbLayer
    netLayer = ctx ^. networkLayer

{-----------------------------------------------------------------------------
    Join stake pool
------------------------------------------------------------------------------}
joinStakePoolDelegationAction
    :: WalletLayer IO s
    -> CurrentEpochSlotting
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> VoteRequest
    -> IO (Tx.DelegationAction, Maybe Tx.VotingAction)
joinStakePoolDelegationAction
    ctx currentEpochSlotting knownPools poolId poolStatus votedRequest
  = do
    (wallet, stakeKeyIsRegistered) <-
        db & \DBLayer{atomically,walletState} -> atomically $
            (,) <$> readDBVar walletState
                <*> isStakeKeyRegistered walletState

    traceWith tr
        $ W.MsgWallet $ W.MsgIsStakeKeyRegistered stakeKeyIsRegistered

    (Write.PParamsInAnyRecentEra era _, _)
        <- W.readNodeTipStateForTxWrite netLayer

    either (throwIO . ExceptionStakePoolDelegation) pure
        $ WD.joinStakePoolDelegationAction
            era
            wallet
            currentEpochSlotting
            knownPools
            poolId
            poolStatus
            votedRequest
  where
    db = ctx ^. dbLayer
    tr = ctx ^. logger
    netLayer = ctx ^. networkLayer

-- | Send a transaction to the network where we join a stake pool.
joinStakePool
    :: forall s n k.
        ( s ~ SeqState n k
        , WalletFlavor s
        , Excluding '[SharedKey] k
        , AddressIndexDerivationType k ~ 'Soft
        , GenChange s
        , IsOurs (SeqState n k) RewardAccount
        , AddressBookIso s
        , Seq.SupportsDiscovery n k
        , DelegationAddress k 'CredFromKeyK
        )
    => WalletLayer IO s
    -> WalletId
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> Passphrase "user"
    -> IO (W.BuiltTx, UTCTime)
joinStakePool ctx wid pools poolId poolStatus passphrase = do
    pp <- currentProtocolParameters netLayer
    currentEpochSlotting <- W.getCurrentEpochSlotting netLayer
    calculateWalletDelegations <-
        db & \DBLayer{atomically,walletState} -> atomically $
            readDelegation walletState
    let dlg = calculateWalletDelegations currentEpochSlotting

    (delegation, votingM) <-
        joinStakePoolDelegationAction
            ctx
            currentEpochSlotting
            pools
            poolId
            poolStatus
            (votingWalletDelegation dlg)

    ttl <- W.transactionExpirySlot ti Nothing
    let transactionCtx =
            defaultTransactionCtx
                { txWithdrawal = NoWithdrawal
                , txValidityInterval = (Nothing, ttl)
                , txDelegationAction = Just delegation
                , txVotingAction = votingM
                , txDeposit = Just $ stakeKeyDeposit pp
                }

    let changeAddrGen = W.defaultChangeAddressGen @s (delegationAddressS @n)
    W.buildSignSubmitTransaction @s
        db
        netLayer
        txLayer
        passphrase
        wid
        changeAddrGen
        (PreSelection [])
        transactionCtx
  where
    db = ctx ^. dbLayer
    netLayer = ctx ^. networkLayer
    ti = timeInterpreter netLayer
    txLayer = ctx ^. transactionLayer

    votingWalletDelegation (WalletDelegation current coming) =
        if isVoting current || any isVotingNext coming then
            VotedSameLikeBefore
        else
            VotedDifferently
      where
        isVoting (Voting _) = True
        isVoting (DelegatingVoting _ _) = True
        isVoting _ = False

        isVotingNext (WalletDelegationNext _ deleg) = isVoting deleg

{-----------------------------------------------------------------------------
    Quit stake pool
------------------------------------------------------------------------------}
quitStakePoolDelegationAction
    :: WalletLayer IO s
    -> CurrentEpochSlotting
    -> Withdrawal
    -> IO Tx.DelegationAction
quitStakePoolDelegationAction ctx currentEpochSlotting withdrawal = do
    wallet <- db & \DBLayer{atomically,walletState} ->
        atomically $ readDBVar walletState
    rewards <- W.fetchRewardBalance db
    either (throwIO . ExceptionStakePoolDelegation) pure
        $ WD.quitStakePoolDelegationAction
            wallet
            rewards
            currentEpochSlotting
            withdrawal
  where
    db = ctx ^. dbLayer

-- | Send a transaction to the network where we quit staking.
quitStakePool
    :: forall s n k.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , GenChange s
        , AddressBookIso s
        , IsOurs (SeqState n k) RewardAccount
        , HasSNetworkId n
        )
    => WalletLayer IO s
    -> WalletId
    -> Passphrase "user"
    -> IO (W.BuiltTx, UTCTime)
quitStakePool ctx walletId passphrase = do
    (rewardAccount, _, derivationPath) <- W.readRewardAccount db
    withdrawal <- WithdrawalSelf rewardAccount derivationPath
        <$> getCachedRewardAccountBalance netLayer rewardAccount

    currentEpochSlotting <- W.getCurrentEpochSlotting netLayer
    pp <- currentProtocolParameters netLayer

    action <-
        quitStakePoolDelegationAction ctx currentEpochSlotting withdrawal

    ttl <- W.transactionExpirySlot ti Nothing
    let transactionCtx =
            defaultTransactionCtx
                { txWithdrawal = withdrawal
                , txValidityInterval = (Nothing, ttl)
                , txDelegationAction = Just action
                , txDeposit = Just $ stakeKeyDeposit pp
                }

    let changeAddrGen = W.defaultChangeAddressGen (delegationAddressS @n)
    W.buildSignSubmitTransaction @s
        db
        netLayer
        txLayer
        passphrase
        walletId
        changeAddrGen
        (PreSelection [])
        transactionCtx
  where
    db = ctx ^. dbLayer
    netLayer = ctx ^. networkLayer
    ti = timeInterpreter netLayer
    txLayer = ctx ^. transactionLayer
