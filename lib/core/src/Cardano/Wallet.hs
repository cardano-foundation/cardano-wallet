{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
    -- * Developement
    -- $Development

    -- * WalletLayer
    -- $WalletLayer
      WalletLayer
    , BlockchainParameters (..)
    , IsWalletCtx
    , newWalletLayer

    -- * Capabilities
    -- $Capabilities
    , HasDBLayer
    , HasLogger
    , HasRestorer
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

import Prelude hiding
    ( log )

import Cardano.BM.Trace
    ( Trace, appendName, logDebug, logError, logInfo, logNotice )
import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..), ErrPostTx (..), Restorer (..), TxSubmitter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK)
    , ErrWrongPassphrase (..)
    , Passphrase
    , WalletKey (..)
    , XPrv
    , checkPassphrase
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    , shuffle
    )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , Fee (..)
    , FeeOptions (..)
    , FeePolicy
    , adjustForFee
    , computeFee
    )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..)
    , Wallet
    , applyBlocks
    , availableUTxO
    , blockHeight
    , blockchainParameters
    , currentTip
    , getPending
    , getState
    , initWallet
    , newPending
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , DefineTx (..)
    , Direction (..)
    , Hash (..)
    , Range (..)
    , SlotId (..)
    , SlotParameters (..)
    , SortOrder (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , TxWitness
    , UTxOStatistics
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , computeUtxoStatistics
    , log10
    , slotDifference
    , slotRangeFromTimeRange
    , slotRatio
    , slotStartTime
    , wholeRange
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkStdTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( AsyncException (..), SomeException, asyncExceptionFromException, catch )
import Control.Monad
    ( forM, forM_, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( fold )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import Data.Word
    ( Word16 )
import Fmt
    ( Buildable, blockListF, pretty, (+|), (+||), (|+), (||+) )
import GHC.Exts
    ( Constraint )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- $Development
-- __Naming Conventions__
--
-- Components inside a particular context `ctx` can be called via dedicated
-- lenses (see Cardano.Wallet#Capabilities). These components are extracted from the context
-- in a @where@ clause according to the following naming convention:
--
-- - @db = ctx ^. dbLayer \@s \@t \@k@ for the 'DBLayer'.
-- - @tr = ctx ^. logger@ for the Logger.
-- - @nw = ctx ^. networkLayer \@t@ for the 'NetworkLayer'.
-- - @tl = ctx ^. transactionLayer \@t \@k@ for the 'TransactionLayer'.
-- - @re = ctx ^. workerRegistry@ for the 'WorkerRegistry'.
--
-- __TroubleShooting__
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s t k) ctx
--     arising from a use of ‘myFunction’
--   Matching instances:
-- @
--
-- Occurs when a particular function is missing a top-level constraint
-- (because it uses another function that demands such constraint). Here,
-- `myFunction` needs its surrounding context `ctx` to have a `DBLayer` but
-- the constraint is missing from its host function.
--
-- __Fix__: Add "@HasDBLayer s t k@" as a class-constraint to the surrounding function.
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s t0 k0) ctx
--     arising from a use of ‘myFunction’
--   Matching givens (or their superclasses):
-- @
--
-- Occurs when a function is called in a context where type-level parameters
-- can be inferred. Here, `myFunction` is called but it is unclear
-- whether the parameter `t0` and `k0` of its context are the same as the ones
-- from the function at the call-site.
--
-- __Fix__: Add type-applications at the call-site "@myFunction \@ctx \@s \@t \@k@"

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

-- $WalletLayer
-- This module provides a full-blown 'WalletLayer' which does possess all the
-- capabilities necessary to run all functions from that module. It can be
-- created via 'newWalletLayer'.
--
-- A concrete 'WalletLayer' can therefore be used as a `ctx` for any of the
-- functions below. For example:
--
-- @
-- import Cardano.Wallet
--     ( WalletLayer, newWalletLayer )
--
-- import qualified Cardano.Wallet as W
--
-- main :: IO ()
-- main =
--     {- ... -}
--     ctx <- newWalletLayer logger blockchainParameters db network builder
--     _ <- W.createWallet ctx wId wName state
--     _ <- W.listWallets ctx
--     _ <- W.estimateFee ctx wId txOuts
--     {- and so forth ... -}
-- @

data WalletLayer s t (k :: Depth -> * -> *)
    = WalletLayer
        (Trace IO Text)
        (Block (Tx t), BlockchainParameters)
        (DBLayer IO s t k)
        (Restorer (Block (Tx t)) IO)
        (TransactionLayer t k)
        (TxSubmitter (Tx t) IO)
        WorkerRegistry
    deriving (Generic)

-- |
-- "Magic trick" to ease caller type signatures. This type family binds together
-- constituants of the WalletLayer (or any similar structure) to remove the
-- ambiguity between s, t and k.
--
-- Without this, calls to functions of the wallet layers are considered
-- ambiguous and demand several type applications, even when the context is
-- known concrete type!
--
-- @
-- W.createWallet @ctx @s @t @k ctx wId wName s
-- @
--
-- Now, if we add a constraint as @IsWalletCtx s t k ctx@ to `createWallet`
-- , we can remove the type applications and do:
--
-- @
-- W.createWallet ctx wId wName s
-- @
--
-- For a known context, we can without ambiguity know what are the corresponding
-- parameters. The family is open, so it can be extended for other kind of
-- structure.
type family IsWalletCtx s t (k :: Depth -> * -> *) ctx :: Constraint

type instance IsWalletCtx s t k (WalletLayer s0 t0 k0) =
    (s0 ~ s, t0 ~ t, k0 ~ k)

-- | Create a new instance of the wallet layer.
newWalletLayer
    :: Trace IO Text
    -> (Block (Tx t), BlockchainParameters)
    -> DBLayer IO s t k
    -> Restorer (Block (Tx t)) IO
    -> TransactionLayer t k
    -> TxSubmitter (Tx t) IO
    -> IO (WalletLayer s t k)
newWalletLayer tr g0 db nw tl ts =
    WalletLayer tr g0 db nw tl ts <$> newRegistry

{-------------------------------------------------------------------------------
                            Wallet Capabilities
-------------------------------------------------------------------------------}

-- $Capabilities
-- Each function in the wallet layer is defined in function of a non-specialized
-- context `ctx`. That context may require some extra capabilities via
-- class-constraints in the function signature. Capabilities are expressed in the
-- form of a "@HasXXX@" class-constraints sometimes with extra type parameters.
--
-- For example:
--
-- @
-- listWallets
--     :: forall ctx s t k.
--         ( IsWalletCtx s t k ctx
--         , HasDBLayer s t k ctx
--         )
--     => ctx
--     -> IO [WalletId]
-- @
--
-- Requires that the given context has an access to a database layer 'DBLayer'
-- parameterized over the wallet state, a network target and a key derivation
-- scheme. Components are pulled from the context generically (i.e. the concrete
-- `ctx` must derive 'Generic') using their associated type. The concrete `ctx`
-- is therefore expected to be a product-type of all the necessary components.
--
-- One can build an interface using only a subset of the wallet layer
-- capabilities and functions, for instance, something to fiddle with wallets
-- and their metadata does not require any networking layer.
type HasDBLayer s t k = HasType (DBLayer IO s t k)

type HasGenesisData t = HasType (Block (Tx t), BlockchainParameters)

type HasLogger = HasType (Trace IO Text)

type HasRestorer t = HasType (Restorer t IO)

type HasTransactionLayer t k = HasType (TransactionLayer t k)

type HasWorkerRegistry = HasType WorkerRegistry

type HasTxSubmitter tx = HasType (TxSubmitter tx IO)

dbLayer
    :: forall s t k ctx. HasDBLayer s t k ctx
    => Lens' ctx (DBLayer IO s t k)
dbLayer =
    typed @(DBLayer IO s t k)

genesisData
    :: forall t ctx. HasGenesisData t ctx
    => Lens' ctx (Block (Tx t), BlockchainParameters)
genesisData =
    typed @(Block (Tx t), BlockchainParameters)

logger
    :: forall ctx. HasLogger ctx
    => Lens' ctx (Trace IO Text)
logger =
    typed @(Trace IO Text)

networkLayer
    :: forall b ctx. (HasRestorer b ctx)
    => Lens' ctx (Restorer b IO)
networkLayer =
    typed @(Restorer b IO)

transactionLayer
    :: forall t k ctx. (HasTransactionLayer t k ctx)
    => Lens' ctx (TransactionLayer t k)
transactionLayer =
    typed @(TransactionLayer t k)

workerRegistry
    :: forall ctx. (HasWorkerRegistry ctx)
    => Lens' ctx WorkerRegistry
workerRegistry =
    typed @WorkerRegistry

transactionSubmitter
    :: forall tx ctx. (HasTxSubmitter tx ctx)
    => Lens' ctx (TxSubmitter tx IO)
transactionSubmitter =
    typed @(TxSubmitter tx IO)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID
createWallet
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , HasGenesisData t ctx
        , Show s
        , NFData s
        , IsOurs s
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createWallet ctx wid wname s = do
    let checkpoint = initWallet block0 bp s
    currentTime <- liftIO getCurrentTime
    let metadata = WalletMetadata
            { name = wname
            , creationTime = currentTime
            , passphraseInfo = Nothing
            , status = Restoring minBound
            , delegation = NotDelegating
            }
    DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid
  where
    db = ctx ^. dbLayer @s @t @k
    (block0, bp) = ctx ^. genesisData @t

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
readWallet ctx wid = (,)
   <$> readWalletCheckpoint @ctx @s @t @k ctx wid
   <*> readWalletMeta @ctx @s @t @k ctx wid

-- | Retrieve a wallet's most recent checkpoint
readWalletCheckpoint
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s t)
readWalletCheckpoint ctx wid =
    maybeToExceptT (ErrNoSuchWallet wid) $ do
        MaybeT $ DB.readCheckpoint db (PrimaryKey wid)
  where
    db = ctx ^. dbLayer @s @t @k

-- | Retrieve only metadata associated with a particular wallet
readWalletMeta
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO WalletMetadata
readWalletMeta ctx wid =
    maybeToExceptT (ErrNoSuchWallet wid) $
        MaybeT $ DB.readWalletMeta db (PrimaryKey wid)
  where
    db = ctx ^. dbLayer @s @t @k

-- | Update a wallet's metadata with the given update function.
updateWallet
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> (WalletMetadata -> WalletMetadata)
    -> ExceptT ErrNoSuchWallet IO ()
updateWallet ctx wid modify =
    DB.withLock db $ do
        meta <- readWalletMeta @ctx @s @t @k ctx wid
        DB.putWalletMeta db (PrimaryKey wid) (modify meta)
  where
    db = ctx ^. dbLayer @s @t @k

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphrase
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , WalletKey k
        )
    => ctx
    -> WalletId
    -> (Passphrase "encryption-old", Passphrase "encryption-new")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphrase ctx wid (old, new) =
    withRootKey @ctx @s @t @k ctx wid (coerce old) ErrUpdatePassphraseWithRootKey
        $ \xprv -> withExceptT ErrUpdatePassphraseNoSuchWallet $ do
            let xprv' = changePassphrase old new xprv
            attachPrivateKey @ctx @s @t @k ctx wid (xprv', coerce new)

-- | Retrieve a list of known wallet IDs.
listWallets
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> IO [WalletId]
listWallets ctx =
    fmap (\(PrimaryKey wid) -> wid) <$> DB.listWallets db
  where
    db = ctx ^. dbLayer @s @t @k

-- | List the wallet's UTxO statistics.
listUtxoStatistics
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
listUtxoStatistics ctx wid = do
    (wal, _) <- withExceptT
        ErrListUTxOStatisticsNoSuchWallet (readWallet @ctx @s @t @k ctx wid)
    let utxo = availableUTxO @s @t wal
    pure $ computeUtxoStatistics log10 utxo

-- | Remove an existing wallet. Note that there's no particular work to
-- be done regarding the restoration worker as it will simply terminate
-- on the next tick when noticing that the corresponding wallet is gone.
removeWallet
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , HasWorkerRegistry ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
removeWallet ctx wid = do
    DB.withLock db . DB.removeWallet db . PrimaryKey $ wid
    liftIO $ cancelWorker re wid
  where
    db = ctx ^. dbLayer @s @t @k
    re = ctx ^. workerRegistry

-- | Restore a wallet from its current tip up to the network tip.
--
-- This function returns immediately, starting a worker thread in the
-- background that will fetch and apply remaining blocks until the
-- network tip is reached or until failure.
restoreWallet
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasLogger ctx
        , HasDBLayer s t k ctx
        , HasRestorer (Block (Tx t)) ctx
        , HasWorkerRegistry ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = do
    (cp, _) <- readWallet @ctx @s @t @k ctx wid
    let workerName = "worker." <> T.take 8 (toText wid)
    let workerCtx = ctx & logger .~ appendName workerName tr
    liftIO $ logInfo tr $ "Restoring wallet "+| wid |+"..."
    let worker = do
            runExceptT (networkTip nw) >>= \case
                Left e -> do
                    logError tr $ "Failed to get network tip: " +|| e ||+ ""
                    restoreSleep @ctx @s @t @k workerCtx wid (currentTip cp)
                Right tip -> do
                    restoreStep @ctx @s @t @k workerCtx wid (currentTip cp, tip)
    let onError e = case asyncExceptionFromException e of
            Just ThreadKilled ->
                logNotice tr "Worker exited: killed by parent."
            Just UserInterrupt ->
                logNotice tr "Worker exited: killed by user."
            _ ->
                logError tr $ "Worker exited unexpectedly: " +|| e ||+ ""
    liftIO $ registerWorker re wid worker onError
  where
    nw = ctx ^. networkLayer @(Block (Tx t))
    tr = ctx ^. logger
    re = ctx ^. workerRegistry

-- | Infinite restoration loop. We drain the whole available chain and try
-- to catch up with the node. In case of error, we log it and wait a bit
-- before retrying.
--
-- The function only terminates if the wallet has disappeared from the DB.
restoreStep
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasLogger ctx
        , HasRestorer (Block (Tx t)) ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> (BlockHeader, (BlockHeader, Quantity "block" Natural))
    -> IO ()
restoreStep ctx wid (localTip, (nodeTip, nodeHeight)) = do
    runExceptT (nextBlocks nw localTip) >>= \case
        Left e -> do
            logError tr $ "Failed to get next blocks: " +|| e ||+ "."
            restoreSleep @ctx @s @t @k ctx wid localTip
        Right [] -> do
            logDebug tr "Wallet restored."
            restoreSleep @ctx @s @t @k ctx wid localTip
        Right (blockFirst : blocksRest) -> do
            let blocks = blockFirst :| blocksRest
            let nextLocalTip = view #header . NE.last $ blocks
            let measuredTip = (nodeTip ^. #slotId, nodeHeight)
            let action = restoreBlocks @ctx @s @t @k ctx wid blocks measuredTip
            runExceptT action >>= \case
                Left (ErrNoSuchWallet _) ->
                    logNotice tr "Wallet is gone! Terminating worker..."
                Right () -> do
                    restoreStep @ctx @s @t @k ctx wid
                        (nextLocalTip, (nodeTip, nodeHeight))
  where
    nw = ctx ^. networkLayer @(Block (Tx t))
    tr = ctx ^. logger

-- | Wait a short delay before querying for blocks again. We also take this
-- opportunity to refresh the chain tip as it has probably increased in
-- order to refine our syncing status.
restoreSleep
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasRestorer (Block (Tx t)) ctx
        , HasLogger ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> BlockHeader
    -> IO ()
restoreSleep ctx wid localTip = do
    threadDelay twoSeconds
    runExceptT (networkTip nw) >>= \case
        Left e -> do
            logError tr $ "Failed to get network tip: " +|| e ||+ ""
            restoreSleep @ctx @s @t @k ctx wid localTip
        Right nodeTip ->
            restoreStep @ctx @s @t @k ctx wid (localTip, nodeTip)
  where
    nw = ctx ^. networkLayer @(Block (Tx t))
    tr = ctx ^. logger
    twoSeconds = 2000000 -- FIXME: Leave that to the networking layer

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
restoreBlocks
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasLogger ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> NonEmpty (Block (Tx t))
    -> (SlotId, Quantity "block" Natural) -- ^ Network tip and height
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx wid blocks (nodeTip, Quantity nodeHeight) = do
    let (slotFirst, slotLast) =
            ( view #slotId . header . NE.head $ blocks
            , view #slotId . header . NE.last $ blocks
            )
    liftIO $ logInfo tr $
        "Applying blocks ["+| slotFirst |+" ... "+| slotLast |+"]"
    -- NOTE:
    -- Not as good as a transaction, but, with the lock, nothing can make
    -- the wallet disappear within these calls, so either the wallet is
    -- there and they all succeed, or it's not and they all fail.
    DB.withLock db $ do
        (wallet, meta) <- readWallet @ctx @s @t @k ctx wid
        let bp = blockchainParameters wallet
        let (txs, cps) = NE.unzip $ applyBlocks @s @t blocks wallet
        let newTxs = fold txs

        let calculateMetadata :: SlotId -> WalletMetadata
            calculateMetadata slot = meta { status = status' }
              where
                progress' = slotRatio (bp ^. #getEpochLength) slot nodeTip
                status' =
                    if progress' == maxBound
                    then Ready
                    else Restoring progress'

        -- NOTE:
        -- We cast `k` and `nodeHeight` to 'Integer' since at a given point
        -- in time, `k` may be greater than the tip.
        let (Quantity k) = bp ^. #getEpochStability
        let bhUnstable :: Integer
            bhUnstable = fromIntegral nodeHeight - fromIntegral k
        forM_ (NE.init cps) $ \cp -> do
            let (Quantity bh) = blockHeight cp
            when (fromIntegral bh >= bhUnstable) $
                DB.putCheckpoint db (PrimaryKey wid) cp

        -- NOTE:
        -- Always store the last checkpoint from the batch and all new
        -- transactions.
        let cpLast = NE.last cps
        let Quantity bhLast = blockHeight cpLast
        let meta' = calculateMetadata (view #slotId $ currentTip cpLast)
        DB.putCheckpoint db (PrimaryKey wid) cpLast
        DB.putTxHistory db (PrimaryKey wid) newTxs
        DB.putWalletMeta db (PrimaryKey wid) meta'

        liftIO $ do
            logInfo tr $ ""
                +|| pretty (calculateMetadata slotLast)
            logInfo tr $ "number of pending transactions: "
                +|| Set.size (getPending cpLast) ||+ ""
            logInfo tr $ "number of new transactions: "
                +|| length newTxs ||+ ""
            logInfo tr $ "new block height: "
                +|| bhLast ||+ ""
            logDebug tr $ "blocks: "
                <> pretty (NE.toList blocks)
            logDebug tr $ "transactions: "
                <> pretty (blockListF (snd <$> Map.elems newTxs))
  where
    db = ctx ^. dbLayer @s @t @k
    tr = ctx ^. logger

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , IsOurs s
        , CompareDiscovery s
        , KnownAddresses s
        , DefineTx t
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
listAddresses ctx wid = do
    (s, txs) <- DB.withLock db $ (,)
        <$> (getState <$> readWalletCheckpoint @ctx @s @t @k ctx wid)
        <*> liftIO (DB.readTxHistory db
            (PrimaryKey wid) Descending wholeRange)
    let maybeIsOurs (TxOut a _) = if fst (isOurs a s)
            then Just a
            else Nothing
    let usedAddrs = Set.fromList $
            concatMap (mapMaybe maybeIsOurs . outputs' . snd) txs
          where
            outputs' (tx, _) = W.outputs @t tx
    let knownAddrs =
            L.sortBy (compareDiscovery s) (knownAddresses s)
    let withAddressState addr =
            (addr, if addr `Set.member` usedAddrs then Used else Unused)
    return $ withAddressState <$> knownAddrs
  where
    db = ctx ^. dbLayer @s @t @k

{-------------------------------------------------------------------------------
                                  Transaction
-------------------------------------------------------------------------------}

coinSelOpts
    :: TransactionLayer t k
    -> Quantity "byte" Word16
    -> CoinSelectionOptions (ErrValidateSelection t)
coinSelOpts tl txMaxSize = CoinSelectionOptions
    { maximumNumberOfInputs = estimateMaxNumberOfInputs tl txMaxSize
    , validate = validateSelection tl
    }

feeOpts
    :: TransactionLayer t k
    -> FeePolicy
    -> FeeOptions
feeOpts tl feePolicy = FeeOptions
    { estimate = computeFee feePolicy . estimateSize tl
    , dustThreshold = minBound
    }

-- | Prepare a transaction and automatically select inputs from the
-- wallet to cover the requested outputs. Note that this only runs
-- coin selection for the given outputs. In order to construct (and
-- sign) an actual transaction, use 'signTx'.
createUnsignedTx
    :: forall ctx s t k e.
        ( IsWalletCtx s t k ctx
        , HasTransactionLayer t k ctx
        , HasLogger ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
createUnsignedTx ctx wid recipients = do
    (wal, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet $
        readWallet @ctx @s @t @k ctx wid
    let bp = blockchainParameters wal
    let utxo = availableUTxO @s @t wal
    (sel, utxo') <- withExceptT ErrCreateUnsignedTxCoinSelection $ do
        let opts = coinSelOpts tl (bp ^. #getTxMaxSize)
        CoinSelection.random opts recipients utxo
    liftIO . logInfo tr $ "Coins selected for transaction: \n"+| sel |+""
    withExceptT ErrCreateUnsignedTxFee $ do
        debug tr "Coins after fee adjustment"
            =<< adjustForFee (feeOpts tl (bp ^. #getFeePolicy)) utxo' sel
  where
    tl = ctx ^. transactionLayer @t @k
    tr = ctx ^. logger

-- | Estimate a transaction fee by automatically selecting inputs from
-- the wallet to cover the requested outputs.
estimateTxFee
    :: forall ctx s t k e.
        ( IsWalletCtx s t k ctx
        , HasTransactionLayer t k ctx
        , HasDBLayer s t k ctx
        , DefineTx t
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrEstimateTxFee e) IO Fee
estimateTxFee ctx wid recipients = do
    (wal, _) <- withExceptT ErrEstimateTxFeeNoSuchWallet $
        readWallet @ctx @s @t @k ctx wid
    let bp = blockchainParameters wal
    let utxo = availableUTxO @s @t wal
    (sel, _utxo') <- withExceptT ErrEstimateTxFeeCoinSelection $ do
        let opts = coinSelOpts tl (bp ^. #getTxMaxSize)
        CoinSelection.random opts recipients utxo
    let estimateFee = computeFee (bp ^. #getFeePolicy) . estimateSize tl
    pure $ estimateFee sel
  where
    tl = ctx ^. transactionLayer @t @k

-- | Produce witnesses and construct a transaction from a given
-- selection. Requires the encryption passphrase in order to decrypt
-- the root private key. Note that this doesn't broadcast the
-- transaction to the network. In order to do so, use 'submitTx'.
signTx
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasTransactionLayer t k ctx
        , HasDBLayer s t k ctx
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
signTx ctx wid pwd (CoinSelection ins outs chgs) =
    DB.withLock db $ do
        (wal, _) <- withExceptT ErrSignTxNoSuchWallet $
            readWallet @ctx @s @t @k ctx wid
        let (changeOuts, s') = flip runState (getState wal) $
                forM chgs $ \c -> do
                    addr <- state (genChange pwd)
                    return $ TxOut addr c
        allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
        withRootKey @_ @s @t ctx wid pwd ErrSignTxWithRootKey $ \xprv -> do
            let keyFrom = isOwned (getState wal) (xprv, pwd)
            case mkStdTx tl keyFrom ins allShuffledOuts of
                Right (tx, wit) -> do
                    -- Safe because we have a lock and we already fetched the
                    -- wallet within this context.
                    liftIO . unsafeRunExceptT $
                        DB.putCheckpoint db (PrimaryKey wid) (updateState s' wal)
                    let amtChng = fromIntegral $
                            sum (getCoin <$> chgs)
                    let amtInps = fromIntegral $
                            sum (getCoin . coin . snd <$> ins)
                    let meta = TxMeta
                            { status = Pending
                            , direction = Outgoing
                            , slotId = (currentTip wal) ^. #slotId
                            , amount = Quantity (amtInps - amtChng)
                            }
                    return (tx, meta, wit)
                Left e ->
                    throwE $ ErrSignTx e
  where
    db = ctx ^. dbLayer @s @t @k
    tl = ctx ^. transactionLayer @t @k

-- | Broadcast a (signed) transaction to the network.
submitTx
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        , HasTxSubmitter (Tx t) ctx
        )
    => ctx
    -> WalletId
    -> (Tx t, TxMeta, [TxWitness])
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid (tx, meta, wit) = do
    withExceptT ErrSubmitTxNetwork $ postTx (tx, wit)
    DB.withLock db $ withExceptT ErrSubmitTxNoSuchWallet $ do
        (wal, _) <- readWallet @ctx @s @t @k ctx wid
        DB.putCheckpoint db (PrimaryKey wid) (newPending (tx, meta) wal)
  where
    db = ctx ^. dbLayer @s @t @k
    postTx = ctx ^. transactionSubmitter @(Tx t)

-- | Broadcast an externally-signed transaction to the network.
submitExternalTx
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasTransactionLayer t k ctx
        , HasTxSubmitter (Tx t) ctx
        )
    => ctx
    -> ByteString
    -> ExceptT ErrSubmitExternalTx IO (Tx t)
submitExternalTx ctx bytes = do
    txWithWit@(tx,_) <- withExceptT ErrSubmitExternalTxDecode $ except $
        decodeSignedTx tl bytes
    withExceptT ErrSubmitExternalTxNetwork $ postTx txWithWit
    return tx
  where
    postTx = ctx ^. transactionSubmitter @(Tx t)
    tl = ctx ^. transactionLayer @t @k


-- | List all transactions and metadata from history for a given wallet.
listTransactions
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
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
listTransactions ctx wid mStart mEnd order = do
    (wal, _) <- withExceptT ErrListTransactionsNoSuchWallet $
        readWallet @ctx @s @t @k ctx wid
    let tip = currentTip wal ^. #slotId
    let sp = fromBlockchainParameters (blockchainParameters wal)
    maybe (pure []) (listTransactionsWithinRange sp tip) =<< (getSlotRange sp)
  where
    db = ctx ^. dbLayer @s @t @k

    fromBlockchainParameters :: BlockchainParameters -> SlotParameters
    fromBlockchainParameters bp = SlotParameters
        (bp ^. #getEpochLength)
        (bp ^. #getSlotLength)
        (bp ^. #getGenesisBlockDate)

    -- Transforms the user-specified time range into a slot range. If the
    -- user-specified range terminates before the start of the blockchain,
    -- returns 'Nothing'.
    getSlotRange
        :: SlotParameters
        -> ExceptT ErrListTransactions IO (Maybe (Range SlotId))
    getSlotRange sp = case (mStart, mEnd) of
        (Just start, Just end) | start > end -> do
            let err = ErrStartTimeLaterThanEndTime start end
            throwE (ErrListTransactionsStartTimeLaterThanEndTime err)
        _ ->
            pure $ slotRangeFromTimeRange sp $ Range mStart mEnd

    listTransactionsWithinRange
        :: SlotParameters
        -> SlotId
        -> Range SlotId
        -> ExceptT ErrListTransactions IO [TransactionInfo]
    listTransactionsWithinRange sp tip sr = do
        liftIO $ assemble sp tip
            <$> DB.readTxHistory db (PrimaryKey wid) order sr

    -- This relies on DB.readTxHistory returning all necessary transactions
    -- to assemble coin selection information for outgoing payments.
    -- To reliably provide this information, it should be looked up when
    -- applying blocks, but that is future work (issue #573).
    assemble
        :: SlotParameters
        -> SlotId
        -> [(Hash "Tx", (Tx t, TxMeta))]
        -> [TransactionInfo]
    assemble sp tip txs = map mkTxInfo txs
      where
        mkTxInfo (txid, (tx, meta)) = TransactionInfo
            { txInfoId = txid
            , txInfoInputs =
                [(txIn, lookupOutput txIn) | txIn <- W.inputs @t tx]
            , txInfoOutputs = W.outputs @t tx
            , txInfoMeta = meta
            , txInfoDepth =
                slotDifference sp tip (meta ^. #slotId)
            , txInfoTime = txTime (meta ^. #slotId)
            }
        txOuts = Map.fromList
            [ (txid, W.outputs @t tx)
            | (txid, (tx, _)) <- txs ]
        -- Because we only track the UTxO of this wallet, we can only
        -- return this information for outgoing payments.
        lookupOutput (TxIn txid index) =
            Map.lookup txid txOuts >>= atIndex (fromIntegral index)
        atIndex i xs = if i < length xs then Just (xs !! i) else Nothing
        -- Get the approximate time of a transaction, given its 'SlotId'.
        -- We assume that the transaction "happens" at the start of the
        -- slot. This is purely arbitrary and in practice, any time between
        -- the start of a slot and its end could be a valid candidate.
        txTime = slotStartTime sp

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}

-- | Attach a given private key to a wallet. The private key is
-- necessary for some operations like signing transactions, or
-- generating new accounts.
attachPrivateKey
    :: forall ctx s t k.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey ctx wid (xprv, pwd) = do
   hpwd <- liftIO $ encryptPassphrase pwd
   DB.putPrivateKey db (PrimaryKey wid) (xprv, hpwd)
   DB.withLock db $ do
       meta <- readWalletMeta @ctx @s @t @k ctx wid
       now <- liftIO getCurrentTime
       let modify x =
               x { passphraseInfo = Just (WalletPassphraseInfo now) }
       DB.putWalletMeta db (PrimaryKey wid) (modify meta)
  where
    db = ctx ^. dbLayer @s @t @k

-- | Execute an action which requires holding a root XPrv.
withRootKey
    :: forall ctx s t k e a.
        ( IsWalletCtx s t k ctx
        , HasDBLayer s t k ctx
        )
    => ctx
    -> WalletId
    -> Passphrase "encryption"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey ctx wid pwd embed action = do
    xprv <- withExceptT embed $ do
        lift (DB.readPrivateKey db (PrimaryKey wid)) >>= \case
            Nothing ->
                throwE $ ErrWithRootKeyNoRootKey wid
            Just (xprv, hpwd) -> do
                withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                    return $ checkPassphrase pwd hpwd
                return xprv
    action xprv
  where
    db = ctx ^. dbLayer @s @t @k

{-------------------------------------------------------------------------------
                                Worker Registry
-------------------------------------------------------------------------------}

-- | A simple registry to keep track of worker threads created for wallet
-- restoration. This way, we can clean up worker threads early and don't have
-- to wait for them to fail with an error message about the wallet being gone.
newtype WorkerRegistry = WorkerRegistry (MVar (Map WalletId ThreadId))

newRegistry :: IO WorkerRegistry
newRegistry = WorkerRegistry <$> newMVar mempty

registerWorker
    :: WorkerRegistry
    -> WalletId
        -- ^ Corresponding wallet
    -> IO ()
        -- ^ Action
    -> (SomeException -> IO ())
        -- ^ On exit
    -> IO ()
registerWorker (WorkerRegistry mvar) wid io handler = do
    threadId <- forkIO $ io `catch` handler
    modifyMVar_ mvar (pure . Map.insert wid threadId)

cancelWorker :: WorkerRegistry -> WalletId -> IO ()
cancelWorker (WorkerRegistry mvar) wid =
    modifyMVar_ mvar (Map.alterF alterF wid)
  where
    alterF = \case
        Nothing -> pure Nothing
        Just threadId -> do
            -- NOTE: It is safe to kill a thread that is already dead.
            killThread threadId
            return Nothing

{-------------------------------------------------------------------------------
                                   Errors
-------------------------------------------------------------------------------}

-- | Errors that can occur when creating an unsigned transaction.
data ErrCreateUnsignedTx e
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection (ErrCoinSelection e)
    | ErrCreateUnsignedTxFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors that can occur when estimating transaction fees.
data ErrEstimateTxFee e
    = ErrEstimateTxFeeNoSuchWallet ErrNoSuchWallet
    | ErrEstimateTxFeeCoinSelection (ErrCoinSelection e)
    deriving (Show, Eq)

-- | Errors that can occur when listing UTxO statistics.
newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrSignTx
    = ErrSignTx ErrMkStdTx
    | ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTxWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors that can occur when submitting a signed transaction to the network.
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when submitting an externally-signed transaction
--   to the network.
data ErrSubmitExternalTx
    = ErrSubmitExternalTxNetwork ErrPostTx
    | ErrSubmitExternalTxDecode ErrDecodeSignedTx
    deriving (Show, Eq)

-- | Errors that can occur when trying to change a wallet's passphrase.
data ErrUpdatePassphrase
    = ErrUpdatePassphraseNoSuchWallet ErrNoSuchWallet
    | ErrUpdatePassphraseWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors that can occur when trying to perform an operation on a wallet that
-- requires a private key, but where none is attached to the wallet.
data ErrWithRootKey
    = ErrWithRootKeyNoRootKey WalletId
    | ErrWithRootKeyWrongPassphrase WalletId ErrWrongPassphrase
    deriving (Show, Eq)

-- | Errors that can occur when trying to list transactions.
data ErrListTransactions
    = ErrListTransactionsNoSuchWallet ErrNoSuchWallet
    | ErrListTransactionsStartTimeLaterThanEndTime ErrStartTimeLaterThanEndTime
    deriving (Show, Eq)

-- | Indicates that the specified start time is later than the specified end
-- time.
data ErrStartTimeLaterThanEndTime = ErrStartTimeLaterThanEndTime
    { errStartTime :: UTCTime
    , errEndTime :: UTCTime
    } deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                   Utils
-------------------------------------------------------------------------------}

debug :: (Buildable a, MonadIO m) => Trace IO Text -> Text -> a -> m a
debug t msg a =
    liftIO (logDebug t (msg <> pretty a)) $> a
