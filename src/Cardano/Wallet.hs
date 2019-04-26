{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

    -- * Errors
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)
    , ErrSignTx(..)
    , ErrSubmitTx(..)
    , ErrCreateUnsignedTx(..)

    -- * Construction
    , mkWalletLayer

    -- * Helpers
    , unsafeRunExceptT
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( encodeSignedTx, toByteString, txId )
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
    ( ErrPostTx (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK)
    , ErrWrongPassphrase (..)
    , Key
    , Passphrase
    , XPrv
    , checkPassphrase
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressScheme (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , getState
    , initWallet
    , newPending
    , updateState
    )
import Cardano.Wallet.Primitive.Signing
    ( SignTxError, mkStdTx )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , Coin (..)
    , Direction (..)
    , IsOurs (..)
    , SignedTx (..)
    , SlotId (..)
    , Tx
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
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
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, void, (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( getCurrentTime )
import Fmt
    ( (+|), (+||), (|+), (||+) )

import qualified Cardano.Wallet.CoinSelection.Policy.Random as CoinSelection
import qualified Cardano.Wallet.DB as DB
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s = WalletLayer
    { createWallet
        :: WalletId
        -> WalletName
        -> s
        -> ExceptT ErrWalletAlreadyExists IO WalletId
        -- ^ Initialise and store a new wallet, returning its Id.

    , readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s, WalletMetadata)
        -- ^ Retrieve the wallet state for the wallet with the given ID.

    , listWallets
        :: IO [WalletId]
        -- ^ Retrieve a list of known wallets IDs.

    , removeWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Remove an existing wallet. Note that, there's no particular work to
        -- be done regarding the restoration worker as it will simply terminate
        -- on the next tick when noticing that the corresponding wallet is gone.

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
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx, TxMeta, [TxWitness])
        -- ^ Produce witnesses and construct a transaction from a given
        -- selection. Requires the encryption passphrase in order to decrypt
        -- the root private key. Note that this doesn't broadcast the
        -- transaction to the network. In order to do so, have a look at
        -- 'submitTx'.

    , submitTx
        :: WalletId
        -> (Tx, TxMeta, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
        -- ^ Broadcast a (signed) transaction to the network.

    , attachPrivateKey
        :: WalletId
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Attach a given private key to a wallet. The private key is
        -- necessary for some operations like signing transactions or,
        -- generating new accounts.
    }

-- | Errors occuring when creating an unsigned transaction
data ErrCreateUnsignedTx
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection CoinSelectionError

-- | Errors occuring when signing a transaction
data ErrSignTx
    = ErrSignTx SignTxError
    | ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTxWrongPassphrase ErrWrongPassphrase

-- | Errors occuring when submitting a signed transaction to the network
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet

{-------------------------------------------------------------------------------
                                 Construction
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
mkWalletLayer
    :: forall s. (IsOurs s, AddressScheme s, NFData s, Show s)
    => DBLayer IO s
    -> NetworkLayer IO
    -> WalletLayer s
mkWalletLayer db network = WalletLayer

    {---------------------------------------------------------------------------
                                       Wallets
    ---------------------------------------------------------------------------}

    { createWallet = \wid wname s -> do
        let checkpoint = initWallet s
        now <- liftIO getCurrentTime
        let metadata = WalletMetadata
                { name = wname
                , passphraseInfo = WalletPassphraseInfo now
                , status = Restoring minBound
                , delegation = NotDelegating
                }
        DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

    , readWallet = _readWallet

    , listWallets = fmap (\(PrimaryKey wid) -> wid) <$> DB.listWallets db

    , removeWallet = DB.removeWallet db . PrimaryKey

    , restoreWallet = \wid -> do
        (w, _) <- _readWallet wid
        void $ liftIO $ forkIO $ do
            runExceptT (networkTip network) >>= \case
                Left e -> do
                    TIO.putStrLn $ "[ERROR] restoreSleep: " +|| e ||+ ""
                    restoreSleep wid (currentTip w)
                Right (_, tip) -> do
                    restoreStep wid (currentTip w, tip ^. #slotId)

    {---------------------------------------------------------------------------
                                    Transactions
    ---------------------------------------------------------------------------}

    , createUnsignedTx = \wid opts recipients -> do
        (w, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet
            (_readWallet wid)
        let utxo = availableUTxO w
        withExceptT ErrCreateUnsignedTxCoinSelection $
            CoinSelection.random opts utxo recipients

    , signTx = \wid pwd (CoinSelection ins outs chgs) -> DB.withLock db $ do
        (w, _) <- withExceptT ErrSignTxNoSuchWallet $ _readWallet wid
        let (changeOuts, s') = flip runState (getState w) $ forM chgs $ \c -> do
                addr <- state nextChangeAddress
                return $ TxOut addr c
        allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
        withRootKey wid pwd ErrSignTxWrongPassphrase $ \xprv -> do
            case mkStdTx (getState w) (xprv, pwd) ins allShuffledOuts of
                Right (tx, wit) -> do
                    -- Safe because we have a lock and we already fetched the
                    -- wallet within this context.
                    liftIO . unsafeRunExceptT $
                        DB.putCheckpoint db (PrimaryKey wid) (updateState s' w)
                    let amtChng = fromIntegral $
                            sum (getCoin <$> chgs)
                    let amtInps = fromIntegral $
                            sum (getCoin . coin . snd <$> ins)
                    let meta = TxMeta
                            { status = Pending
                            , direction = Outgoing
                            , slotId = currentTip w
                            , amount = Quantity (amtInps - amtChng)
                            }
                    return (tx, meta, wit)
                Left e ->
                    throwE $ ErrSignTx e

    , submitTx = \wid (tx, meta, wit) -> do
        let signed = SignedTx $ toByteString $ encodeSignedTx (tx, wit)
        withExceptT ErrSubmitTxNetwork $ postTx network signed
        DB.withLock db $ withExceptT ErrSubmitTxNoSuchWallet $ do
            (w, _) <- _readWallet wid
            let history = Map.fromList [(txId tx, (tx, meta))]
            DB.putCheckpoint db (PrimaryKey wid) (newPending tx w)
            DB.putTxHistory db (PrimaryKey wid) history

    {---------------------------------------------------------------------------
                                     Keystore
    ---------------------------------------------------------------------------}

    , attachPrivateKey = \wid (xprv, pwd) -> do
        hpwd <- liftIO $ encryptPassphrase pwd
        DB.putPrivateKey db (PrimaryKey wid) (xprv, hpwd)
    }
  where
    _readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s, WalletMetadata)
    _readWallet wid = maybeToExceptT (ErrNoSuchWallet wid) $ do
        cp <- MaybeT $ DB.readCheckpoint db (PrimaryKey wid)
        meta <- MaybeT $ DB.readWalletMeta db (PrimaryKey wid)
        return (cp, meta)

    -- | Execute an action which requires holding a root XPrv
    withRootKey
        :: forall e a. ()
        => WalletId
        -> Passphrase "encryption"
        -> (ErrWrongPassphrase -> e)
        -> (Key 'RootK XPrv -> ExceptT e IO a)
        -> ExceptT e IO a
    withRootKey wid pwd embed action = do
        xprv <- withExceptT embed $ do
            (xprv, hpwd) <- liftIO $ DB.readPrivateKey db (PrimaryKey wid) >>= \case
                Nothing -> unsafeRunExceptT $ throwE $ ErrNoSuchWallet wid
                Just a  -> return a
            ExceptT $ return ((\() -> xprv) <$> checkPassphrase pwd hpwd)
        action xprv

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
                let next = view #slotId . header . last $ blocks
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
                restoreStep wid (slot, tip ^. #slotId)

    -- | Apply the given blocks to the wallet and update the wallet state,
    -- transaction history and corresponding metadata.
    restoreBlocks
        :: WalletId
        -> [Block]
        -> SlotId -- ^ Network tip
        -> ExceptT ErrNoSuchWallet IO ()
    restoreBlocks wid blocks tip = do
        let (inf, sup) =
                ( view #slotId . header . head $ blocks
                , view #slotId . header . last $ blocks
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
        let meta' = meta { status = status' } :: WalletMetadata

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
