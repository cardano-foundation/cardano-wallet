{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as being
-- intermediary between the three.


module Cardano.Wallet
    (
    -- * Interface
      WalletLayer (..)
    , NewWallet(..)

    -- * Errors
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)

    -- * Construction
    , mkWalletLayer

    -- * Helpers
    , unsafeRunExceptT
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( encodeSignedTx, toByteString )
import Cardano.Wallet.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionError (..)
    , CoinSelectionOptions
    , shuffle
    )
import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (RootK)
    , Key
    , Passphrase
    , XPrv
    , deriveAccountPrivateKey
    , digest
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, SeqState (..), mkAddressPool )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlocks, availableUTxO, currentTip, getState, initWallet )
import Cardano.Wallet.Primitive.Signing
    ( SignTxError, mkStdTx )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , SignedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxOut (..)
    , TxWitness
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , slotRatio
    )
import Control.Arrow
    ( first )
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.Monad
    ( void, (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Data.Functor
    ( ($>) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Time.Clock
    ( getCurrentTime )
import Fmt
    ( (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.CoinSelection.Policy.Random as CoinSelection
import qualified Cardano.Wallet.DB as DB
import qualified Data.Text.IO as TIO

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s = WalletLayer
    { createWallet
        :: NewWallet
        -> ExceptT ErrWalletAlreadyExists IO WalletId
        -- ^ Initialise and store a new wallet, returning its ID.

    , readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s, WalletMetadata)
        -- ^ Retrieve the wallet state for the wallet with the given ID.

    , restoreWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Restore a wallet from its current tip up to a given target
        -- (typically, the network tip).
        --
        -- It returns immediately and fail if the wallet is already beyond the
        -- given tip. It starts a worker in background which will fetch and
        -- apply remaining blocks until failure or, the target slot is reached.

    , createUnsignedTx
        :: WalletId
        -> CoinSelectionOptions
        -> NonEmpty TxOut
        -> ExceptT ErrCreateUnsignedTx IO CoinSelection
        -- ^ Prepare a transaction and automatically select inputs from the
        -- wallet to cover the requested outputs. Note that this only run the
        -- coin selection for the given outputs. In order to construct (and
        -- sign) an actual transaction, have a look at 'signTx'.

    , signTx
        :: WalletId
        -> Key 'RootK XPrv
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx, [TxWitness])
        -- ^ Produce witnesses and construct a transaction from a given
        -- selection. Requires the encryption passphrase in order to decrypt
        -- the root private key. Note that this doesn't broadcast the
        -- transaction to the network. In order to do so, have a look at
        -- 'submitTx'.

    , submitTx
        :: (Tx, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
        -- ^ Broadcast a (signed) transaction to the network.
    }

data NewWallet = NewWallet
    { seed
        :: !(Passphrase "seed")
    , secondFactor
        :: !(Passphrase "generation")
    , name
        :: !WalletName
    , passphrase
        :: !(Passphrase "encryption")
    , gap
        :: !AddressPoolGap
    } deriving (Show, Generic)

-- | Errors occuring when creating an unsigned transaction
data ErrCreateUnsignedTx
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection CoinSelectionError

-- | Errors occuring when signing a transaction
data ErrSignTx
    = ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTx SignTxError

-- | Errors occuring when submitting a signed transaction to the network
data ErrSubmitTx = forall a. NetworkError a

{-------------------------------------------------------------------------------
                                 Construction
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
mkWalletLayer
    :: DBLayer IO SeqState
    -> NetworkLayer IO
    -> WalletLayer SeqState
mkWalletLayer db network = WalletLayer

    {---------------------------------------------------------------------------
                                       Wallets
    ---------------------------------------------------------------------------}

    { createWallet = \w -> do
        let rootXPrv =
                generateKeyFromSeed (seed w, secondFactor w) (passphrase w)
        let accXPrv =
                deriveAccountPrivateKey mempty rootXPrv minBound
        let extPool =
                mkAddressPool (publicKey accXPrv) (gap w) ExternalChain []
        let intPool =
                mkAddressPool (publicKey accXPrv) minBound InternalChain []
        let wid =
                WalletId (digest $ publicKey rootXPrv)
        let checkpoint = initWallet $ SeqState
                { externalPool = extPool
                , internalPool = intPool
                }
        now <- liftIO getCurrentTime
        let metadata = WalletMetadata
                { name = Cardano.Wallet.name w
                , passphraseInfo = WalletPassphraseInfo now
                , status = Restoring minBound
                , delegation = NotDelegating
                }
        DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

    , readWallet = _readWallet

    , restoreWallet = \wid -> do
        (w, _) <- _readWallet wid
        void $ liftIO $ forkIO $ do
            runExceptT (networkTip network) >>= \case
                Left e -> do
                    TIO.putStrLn $ "[ERROR] restoreSleep: " +|| e ||+ ""
                    restoreSleep wid (currentTip w)
                Right (_, tip) -> do
                    restoreStep wid (currentTip w, slotId tip)

    {---------------------------------------------------------------------------
                                    Transactions
    ---------------------------------------------------------------------------}

    , createUnsignedTx = \wid opts recipients -> do
        (w, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet
            (_readWallet wid)
        let utxo = availableUTxO w
        withExceptT ErrCreateUnsignedTxCoinSelection $
            CoinSelection.random opts utxo recipients

    , submitTx = \(tx, witnesses) -> do
        let signed = SignedTx $ toByteString $ encodeSignedTx (tx, witnesses)
        withExceptT NetworkError $ postTx network signed

    , signTx = \wid rootXPrv password (CoinSelection ins outs _chgs) -> do
        -- TODO: This is untested
        (w, _) <- withExceptT ErrSignTxNoSuchWallet $ _readWallet wid

        shuffledOuts <- liftIO $ shuffle outs
        case mkStdTx (getState w) (rootXPrv, password) ins shuffledOuts of
            Right a -> return a
            Left e -> throwE $ ErrSignTx e
    }
  where
    _readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet SeqState, WalletMetadata)
    _readWallet wid = maybeToExceptT (ErrNoSuchWallet wid) $ do
        cp <- MaybeT $ DB.readCheckpoint db (PrimaryKey wid)
        meta <- MaybeT $ DB.readWalletMeta db (PrimaryKey wid)
        return (cp, meta)

    -- | Infinite restoration loop. We drain the whole available chain and try
    -- to catch up with the node. In case of error, we log it and wait a bit
    -- before retrying.
    --
    -- The function only terminates if the wallet has disappeared from the DB.
    restoreStep :: WalletId -> (SlotId, SlotId) -> IO ()
    restoreStep wid (slot, tip) = do
        runExceptT (nextBlocks network slot) >>= \case
            Left e -> do
                TIO.putStrLn $ "[ERROR] restoreStep: " +|| e ||+ ""
                restoreSleep wid slot
            Right [] -> do
                restoreSleep wid slot
            Right blocks -> do
                let next = slotId . header . last $ blocks
                runExceptT (restoreBlocks wid blocks tip) >>= \case
                    Left (ErrNoSuchWallet _) -> do
                        TIO.putStrLn $ "[ERROR] restoreStep: wallet " +| wid |+ "is gone!"
                    Right () -> do
                        restoreStep wid (next, tip)

    -- | Wait a short delay before querying for blocks again. We do take this
    -- opportunity to also refresh the chain tip as it has probably increased
    -- in order to refine our syncing status.
    restoreSleep :: WalletId -> SlotId -> IO ()
    restoreSleep wid slot = do
        let tenSeconds = 10000000 in threadDelay tenSeconds
        runExceptT (networkTip network) >>= \case
            Left e -> do
                TIO.putStrLn $ "[ERROR] restoreSleep: " +|| e ||+ ""
                restoreSleep wid slot
            Right (_, tip) ->
                restoreStep wid (slot, slotId tip)

    -- | Apply the given blocks to the wallet and update the wallet state,
    -- transaction history and corresponding metadata.
    restoreBlocks
        :: WalletId
        -> [Block]
        -> SlotId -- ^ Network tip
        -> ExceptT ErrNoSuchWallet IO ()
    restoreBlocks wid blocks tip = do
        let (inf, sup) =
                ( slotId . header . head $ blocks
                , slotId . header . last $ blocks
                )
        liftIO $ TIO.putStrLn $
            "[INFO] Applying blocks ["+| inf |+" ... "+| sup |+"]"

        (cp, meta) <- _readWallet wid
        -- NOTE
        -- We only process non-empty blocks, though we still keep the last block
        -- of the list, even if empty, so that we correctly update the current
        -- tip of the wallet state.
        let nonEmpty = not . null . transactions
        let (h,q) = first (filter nonEmpty) $ splitAt (length blocks - 1) blocks
        let (txs, cp') = applyBlocks (h ++ q) cp
        let progress = slotRatio sup tip
        let status' = if progress == maxBound then Ready else Restoring progress
        let meta' = meta { status = status' }

        -- NOTE
        -- Not as good as a transaction, but, with the lock, nothing can make
        -- the wallet disappear within these calls, so either the wallet is
        -- there and they all succeed, or it's not and they all fail.
        DB.withLock db $ do
            DB.putCheckpoint db (PrimaryKey wid) cp'
            DB.putTxHistory db (PrimaryKey wid) txs
            DB.putWalletMeta db (PrimaryKey wid) meta'

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Run an ExcepT and throws the error if any. This makes sense only if called
-- after checking for an invariant or, after ensuring that preconditions for
-- meeting the underlying error have been discarded.
unsafeRunExceptT :: (MonadFail m, Show e) => ExceptT e m a -> m a
unsafeRunExceptT = runExceptT >=> \case
    Left e ->
        fail $ "unexpected error: " <> show e
    Right a ->
        return a
