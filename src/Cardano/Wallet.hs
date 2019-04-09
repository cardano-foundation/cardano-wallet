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
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions )
import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..), drain, listen )
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
    ( Wallet, applyBlocks, availableUTxO, getState, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
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
    )
import Control.Monad
    ( (>=>) )
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
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.DB as DB

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

    , watchWallet
        :: forall a. WalletId
        -> IO a
        -- ^ Consume blocks from the node as they arrive, and apply them to the
        -- wallet. This function never returns, but may throw an exception.

    , processWallet
        :: (SlotId -> IO ())
        -> WalletId
        -> IO ()
        -- ^ Consume the entire available chain, applying block to the given
        -- wallet, and stop when finished. This function is intended for
        -- benchmarking.
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
    | ErrSignTx

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

    , signTx = \wid rootXPrv password (CoinSelection ins outs chgs) -> do
        (w, _) <- withExceptT ErrSignTxNoSuchWallet $ _readWallet wid
        maybe
            (throwE ErrSignTx)
            return
            (mkStdTx (getState w) rootXPrv password ins outs chgs)

    {---------------------------------------------------------------------------
                                  W.I.P. / Debts
    ---------------------------------------------------------------------------}

    , watchWallet =
        liftIO . listen network . onNextblocks

    , processWallet = \logInfo wid -> drain network $ \slot blocks -> do
        logInfo slot
        onNextblocks wid blocks
    }
  where
    _readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet SeqState, WalletMetadata)
    _readWallet wid = maybeToExceptT (ErrNoSuchWallet wid) $ do
        cp <- MaybeT $ DB.readCheckpoint db (PrimaryKey wid)
        meta <- MaybeT $ DB.readWalletMeta db (PrimaryKey wid)
        return (cp, meta)

    onNextblocks :: WalletId -> [Block] -> IO ()
    onNextblocks wid blocks = do
        (txs, cp') <- DB.readCheckpoint db (PrimaryKey wid) >>= \case
            Nothing ->
                fail $ "couldn't find worker wallet: " <> show wid
            Just cp -> do
                let nonEmpty = not . null . transactions
                return $ applyBlocks (filter nonEmpty blocks) cp
        -- FIXME
        -- Note that, the two calls below are _safe_ under the assumption that
        -- the wallet existed _right before_. In theory, in a multi-threaded
        -- context, it may happen that another actor deletes the wallet between
        -- the calls. Here
        -- In practice, it isn't really _bad_ if the wallet is gone, we could
        -- simply log an error or warning and move on. This would have to be
        -- done as soon as we introduce logging.
        -- Note also that there's no transaction surrounding both calls because
        -- there's only one thread per wallet that will apply blocks. And
        -- therefore, only one thread making changes on checkpoints and/or tx
        -- history
        unsafeRunExceptT $ DB.putCheckpoint db (PrimaryKey wid) cp'
        unsafeRunExceptT $ DB.putTxHistory db (PrimaryKey wid) txs

    mkStdTx = error "TODO: mkStdTx not implemented yet"

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
