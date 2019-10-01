{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides wallet layer functions that are used by API layer. Uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as
-- being intermediary between the three.
--
-- Functions of the wallet layer are often parameterized with variables
-- following the convention below:
--
-- - @s@: A __s__tate used to keep track of known addresses. Typically, possible
--   values for this parameter are described in 'Cardano.Wallet.AddressDiscovery' sub-modules.
--   For instance @SeqState@ or @Rnd State@.
--
-- - @t@: A __t__arget backend which captures details specific to a particular chain
--   producer (binary formats, fee policy, networking layer).
--
-- - @k@: A __k__ey derivation scheme intrisically connected to the underlying discovery
--   state @s@. This describes how the hierarchical structure of a wallet is
--   defined as well as the relationship between secret keys and public
--   addresses.

module Cardano.Wallet
    (
    -- * WalletLayer
      WalletLayer
    , newWalletLayer

    -- * Capabilities
    , HasDBFactory
    , HasDBLayer
    , HasGenesisData
    , HasLogger
    , HasNetworkLayer
    , HasTransactionLayer
    , HasWorkerRegistry

    -- * Interface
    -- ** Wallet
    , attachPrivateKey
    , createWallet
    , listUtxoStatistics
    , listWallets
    , readWallet
    , removeWallet
    , restoreWallet
    , updateWallet
    , updateWalletPassphrase
    , ErrCreateWallet (..)
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)

    -- ** Address
    , listAddresses

    -- ** Transaction
    , createUnsignedTx
    , estimateTxFee
    , listTransactions
    , signTx
    , submitExternalTx
    , submitTx
    , ErrCreateUnsignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrSignTx (..)
    , ErrMkStdTx (..)
    , ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrSubmitTx (..)
    , ErrSubmitExternalTx (..)
    , ErrPostTx (..)
    , ErrDecodeSignedTx (..)
    , ErrValidateSelection
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrListTransactions (..)
    , ErrNetworkUnavailable (..)
    , ErrStartTimeLaterThanEndTime (..)
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logError, logNotice )
import Cardano.Wallet.DB
    ( DBFactory (..), DBLayer )
import Cardano.Wallet.Engine
    ( ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrCreateUnsignedTx (..)
    , ErrDecodeSignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkStdTx (..)
    , ErrNetworkUnavailable (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrSignTx (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitExternalTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , HasDBLayer
    , HasGenesisData
    , HasLogger
    , HasNetworkLayer
    , HasTransactionLayer
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK), Passphrase, WalletKey (..), XPrv )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..), Wallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , DefineTx (..)
    , SortOrder (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxOut (..)
    , TxWitness
    , UTxOStatistics
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , MkWorker (..)
    , WorkerRegistry
    , newWorker
    , workerResource
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( AsyncException (..), SomeException, asyncExceptionFromException )
import Control.Monad
    ( forM_, void )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Fmt
    ( (+||), (||+) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Engine as E
import qualified Cardano.Wallet.Registry as Registry

{-------------------------------------------------------------------------------
                               Wallet Layer(s)
-------------------------------------------------------------------------------}

data WalletLayer s t (k :: Depth -> * -> *)
    = WalletLayer
        (Trace IO Text)
        (Block (Tx t), BlockchainParameters)
        (NetworkLayer IO (Tx t) (Block (Tx t)))
        (TransactionLayer t k)
        (DBFactory IO s t k)
        (WorkerRegistry (DBLayer IO s t k))
    deriving (Generic)

data WorkerLayer s t (k :: Depth -> * -> *)
    = WorkerLayer
        (Trace IO Text)
        (Block (Tx t), BlockchainParameters)
        (NetworkLayer IO (Tx t) (Block (Tx t)))
        (TransactionLayer t k)
        (DBLayer IO s t k)
    deriving (Generic)

instance HasWorkerCtx (DBLayer IO s t k) (WalletLayer s t k) where
    type WorkerCtx (WalletLayer s t k) = WorkerLayer s t k
    hoistResource db (WalletLayer tr bp nw tl _ _) =
        WorkerLayer tr bp nw tl db

-- | Create a new instance of the wallet layer.
newWalletLayer
    :: forall ctx s t k. (ctx ~ WalletLayer s t k, DefineTx t)
    => Trace IO Text
    -> (Block (Tx t), BlockchainParameters)
    -> NetworkLayer IO (Tx t) (Block (Tx t))
    -> TransactionLayer t k
    -> DBFactory IO s t k
    -> [WalletId]
    -> IO ctx
newWalletLayer tr g0 nw tl df wids = do
    re <- Registry.empty
    let ctx = WalletLayer tr g0 nw tl df re
    forM_ wids (registerWorker re ctx)
    return ctx
  where
    registerWorker re ctx wid = do
        let config = MkWorker
                { workerBefore =
                    \_ _ -> return ()

                , workerMain = \ctx' _ -> do
                    -- FIXME:
                    -- Review error handling here
                    unsafeRunExceptT $
                        E.restoreWallet @(WorkerCtx ctx) @s @t @k ctx' wid

                , workerAfter =
                    defaultWorkerAfter

                , workerAcquire =
                    (df ^. #withDatabase) wid
                }
        newWorker @ctx @(DBLayer IO s t k) ctx wid config >>= \case
            Nothing ->
                return ()
            Just worker ->
                Registry.insert re worker

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s t k ctx =
    ( HasType (WorkerRegistry (DBLayer IO s t k)) ctx
    , HasWorkerCtx (DBLayer IO s t k) ctx
    )

workerRegistry
    :: forall s t k ctx. (HasWorkerRegistry s t k ctx)
    => Lens' ctx (WorkerRegistry (DBLayer IO s t k))
workerRegistry =
    typed @(WorkerRegistry (DBLayer IO s t k))

type HasDBFactory s t k = HasType (DBFactory IO s t k)

dbFactory
    :: forall s t k ctx. (HasDBFactory s t k ctx)
    => Lens' ctx (DBFactory IO s t k)
dbFactory =
    typed @(DBFactory IO s t k)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

data ErrCreateWallet
    = ErrCreateWalletAlreadyExists ErrWalletAlreadyExists
        -- ^ Wallet already exists
    | ErrCreateWalletFailedToCreateWorker
        -- ^ Somehow, we couldn't create a worker or open a db connection
    deriving (Eq, Show)

-- | see 'Cardano.Wallet.Engine#createWallet'
createWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasDBFactory s t k ctx
        , HasLogger ctx
        , HasGenesisData t (WorkerCtx ctx)
        , HasNetworkLayer t (WorkerCtx ctx)
        , HasLogger (WorkerCtx ctx)
        , Show s
        , NFData s
        , IsOurs s
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrCreateWallet IO WalletId
createWallet ctx wid a0 a1 =
    liftIO (Registry.lookup re wid) >>= \case
        Just _ ->
            throwE $ ErrCreateWalletAlreadyExists $ ErrWalletAlreadyExists wid
        Nothing -> do
            let config = MkWorker
                    { workerBefore = \ctx' _ -> do
                        -- FIXME:
                        -- Review error handling here
                        void $ unsafeRunExceptT $
                            E.createWallet @(WorkerCtx ctx) @s @t @k ctx' wid a0 a1

                    , workerMain = \ctx' _ -> do
                        -- FIXME:
                        -- Review error handling here
                        unsafeRunExceptT $
                            E.restoreWallet @(WorkerCtx ctx) @s @t @k ctx' wid

                    , workerAfter =
                        defaultWorkerAfter

                    , workerAcquire =
                        (df ^. #withDatabase) wid
                    }
            liftIO (newWorker @ctx @(DBLayer IO s t k) ctx wid config) >>= \case
                Nothing ->
                    throwE ErrCreateWalletFailedToCreateWorker
                Just worker ->
                    liftIO (Registry.insert re worker) $> wid
  where
    re = ctx ^. workerRegistry @s @t @k
    df = ctx ^. dbFactory @s @t @k

-- | see 'Cardano.Wallet.Engine#restoreWallet'
restoreWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasLogger (WorkerCtx ctx)
        , HasNetworkLayer t (WorkerCtx ctx)
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid =
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.restoreWallet @(WorkerCtx ctx) @s @t @k wrk wid

-- | see 'Cardano.Wallet.Engine#readWallet'
readWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata, Set (Tx t))
readWallet ctx wid =
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.readWallet @(WorkerCtx ctx) @s @t @k wrk wid

-- | see 'Cardano.Wallet.Engine#updateWallet'
updateWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        )
    => ctx
    -> WalletId
    -> (WalletMetadata -> WalletMetadata)
    -> ExceptT ErrNoSuchWallet IO ()
updateWallet ctx wid a0 =
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.updateWallet @(WorkerCtx ctx) @s @t @k wrk wid a0

-- | see 'Cardano.Wallet.Engine#updateWalletPassphrase'
updateWalletPassphrase
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , WalletKey k
        )
    => ctx
    -> WalletId
    -> (Passphrase "encryption-old", Passphrase "encryption-new")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphrase ctx wid a0 = do
    let liftE = throwE . ErrUpdatePassphraseNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.updateWalletPassphrase @(WorkerCtx ctx) @s @t @k wrk wid a0

-- | see 'Cardano.Wallet.Engine#listUtxoStatistics'
listUtxoStatistics
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
listUtxoStatistics ctx wid = do
    let liftE = throwE . ErrListUTxOStatisticsNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.listUtxoStatistics @(WorkerCtx ctx) @s @t @k wrk wid

-- | see 'Cardano.Wallet.Engine#removeWallet'
removeWallet
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasDBFactory s t k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
removeWallet ctx wid = do
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.removeWallet @(WorkerCtx ctx) @s @t @k wrk wid
    liftIO $ Registry.remove re wid
    liftIO $ (df ^. #removeDatabase) wid
  where
    re = ctx ^. workerRegistry @s @t @k
    df = ctx ^. dbFactory @s @t @k

-- | Retrieve a list of known wallet IDs.
listWallets
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        )
    => ctx
    -> IO [WalletId]
listWallets ctx =
    Registry.keys re
  where
    re = ctx ^. workerRegistry @s @t @k

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | see 'Cardano.Wallet.Engine#listAddresses'
listAddresses
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , IsOurs s
        , CompareDiscovery s
        , KnownAddresses s
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
listAddresses ctx wid =
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.listAddresses @(WorkerCtx ctx) @s @t @k wrk wid

{-------------------------------------------------------------------------------
                                  Transaction
-------------------------------------------------------------------------------}

-- | see 'Cardano.Wallet.Engine#createUnsignedTx'
createUnsignedTx
    :: forall ctx s t k e.
        ( HasWorkerRegistry s t k ctx
        , HasTransactionLayer t k (WorkerCtx ctx)
        , HasLogger (WorkerCtx ctx)
        , DefineTx t
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
createUnsignedTx ctx wid a0 = do
    let liftE = throwE . ErrCreateUnsignedTxNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.createUnsignedTx @(WorkerCtx ctx) @s @t @k wrk wid a0

-- | see 'Cardano.Wallet.Engine#estimateTxFee'
estimateTxFee
    :: forall ctx s t k e.
        ( HasWorkerRegistry s t k ctx
        , HasTransactionLayer t k (WorkerCtx ctx)
        , DefineTx t
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrEstimateTxFee e) IO Fee
estimateTxFee ctx wid a0 = do
    let liftE = throwE . ErrEstimateTxFeeNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.estimateTxFee @(WorkerCtx ctx) @s @t @k wrk wid a0

-- | see 'Cardano.Wallet.Engine#signTx'
signTx
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasTransactionLayer t k (WorkerCtx ctx)
        , Show s
        , NFData s
        , IsOwned s k
        , GenChange s
        )
    => ctx
    -> WalletId
    -> Passphrase "encryption"
    -> CoinSelection
    -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
signTx ctx wid a0 a1 = do
    let liftE = throwE . ErrSignTxNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.signTx @(WorkerCtx ctx) @s @t @k wrk wid a0 a1

-- | see 'Cardano.Wallet.Engine#submitTx'
submitTx
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , HasNetworkLayer t (WorkerCtx ctx)
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> (Tx t, TxMeta, [TxWitness])
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid a0 = do
    let liftE = throwE . ErrSubmitTxNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.submitTx @(WorkerCtx ctx) @s @t @k wrk wid a0

-- | see 'Cardano.Wallet.Engine#listTransactions'
listTransactions
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> Maybe UTCTime
        -- Inclusive minimum time bound.
    -> Maybe UTCTime
        -- Inclusive maximum time bound.
    -> SortOrder
    -> ExceptT ErrListTransactions IO [TransactionInfo]
listTransactions ctx wid a0 a1 a2 = do
    let liftE = throwE . ErrListTransactionsNoSuchWallet
    withWorkerCtx @ctx @s @t @k ctx wid liftE $ \wrk ->
        E.listTransactions @(WorkerCtx ctx) @s @t @k wrk wid a0 a1 a2

-- | see 'Cardano.Wallet.Engine#submitExternalTx'
submitExternalTx
    :: forall ctx t k.
        ( HasNetworkLayer t ctx
        , HasTransactionLayer t k ctx
        )
    => ctx
    -> ByteString
    -> ExceptT ErrSubmitExternalTx IO (Tx t)
submitExternalTx =
    E.submitExternalTx @ctx @t @k

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}

-- | see 'Cardano.Wallet.Engine#attachPrivateKey'
attachPrivateKey
    :: forall ctx s t k.
        ( HasWorkerRegistry s t k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey ctx wid a0 =
    withWorkerCtx @ctx @s @t @k ctx wid throwE $ \wrk ->
        E.attachPrivateKey @(WorkerCtx ctx) @s @t @k wrk wid a0

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

-- | Run an action in a particular worker context. Fails with 'ErrNoSuchWallet'
-- if there's no worker for a given id.
withWorkerCtx
    :: forall ctx s t k m a.
        ( HasWorkerRegistry s t k ctx
        , MonadIO m
        )
    => ctx
    -> WalletId
    -> (ErrNoSuchWallet -> m a)
    -> (WorkerCtx ctx -> m a)
    -> m a
withWorkerCtx ctx wid onMissing action =
    Registry.lookup re wid >>= \case
        Nothing ->
            onMissing $ ErrNoSuchWallet wid
        Just wrk ->
            action $ hoistResource (workerResource wrk) ctx
  where
    re = ctx ^. workerRegistry @s @t @k

defaultWorkerAfter :: Trace IO Text -> Either SomeException a -> IO ()
defaultWorkerAfter tr = \case
    Right _ ->
        logNotice tr "Worker has exited: main action is over"
    Left e -> case asyncExceptionFromException e of
        Just ThreadKilled ->
            logNotice tr "Worker has exited: killed by parent."
        Just UserInterrupt ->
            logNotice tr "Worker has exited: killed by user."
        _ ->
            logError tr $ "Worker has exited unexpectedly: " +|| e ||+ ""
