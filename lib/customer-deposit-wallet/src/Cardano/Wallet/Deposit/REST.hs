{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'IO'-based interface to the Deposit Wallet
-- where the wallet is treated as a mutable resource (~ REST).
-- This interface can be mapped one-to-one to a HTTP interface.
module Cardano.Wallet.Deposit.REST
    ( -- * Types
      WalletResource
    , WalletResourceM
    , ErrDatabase (..)
    , ErrLoadingDatabase (..)
    , ErrCreatingDatabase (..)
    , ErrWalletResource (..)

      -- * Running
    , runWalletResourceM

      -- * Operations

      -- ** Initialization
    , initXPubWallet
    , loadWallet

      -- ** Mapping between customers and addresses
    , listCustomers
    , createAddress

      -- ** Reading from the blockchain
    , getWalletTip
    , availableBalance
    , getCustomerHistory
    , getCustomerHistories

      -- ** Writing to the blockchain
    , createPayment
    , getBIP32PathsForOwnedInputs
    , signTxBody
    , walletExists
    , walletPublicIdentity
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubFromBytes
    , xpubToBytes
    )
import Cardano.Crypto.Wallet
    ( XPub (..)
    )
import Cardano.Wallet.Address.BIP32
    ( BIP32Path
    )
import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ErrResourceExists (..)
    , ErrResourceMissing (..)
    , ResourceStatus
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , Word31
    , fromXPubAndGenesis
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Codec.Serialise
    ( deserialise
    , serialise
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Control.Monad.Trans.Reader
    ( ReaderT (..)
    , ask
    )
import Control.Tracer
    ( Tracer (..)
    )
import Cryptography.Hash.Blake
    ( blake2b160
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.List
    ( isPrefixOf
    )
import Data.Store
    ( Store (..)
    , newStore
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( (</>)
    )

import qualified Cardano.Wallet.Deposit.IO as WalletIO
import qualified Cardano.Wallet.Deposit.IO.Resource as Resource
import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

-- | Error indicating that the database could not be loaded.
data ErrLoadingDatabase
    = ErrDatabaseNotFound FilePath
    | ErrDatabaseCorrupted FilePath
    | ErrMultipleDatabases [FilePath]
    deriving (Show, Eq)

-- | Error indicating that the database could not be created.
newtype ErrCreatingDatabase
    = ErrDatabaseAlreadyExists FilePath
    deriving (Show, Eq)

-- | Error indicating that the database could not be loaded or created.
data ErrDatabase
    = ErrLoadingDatabase ErrLoadingDatabase
    | ErrCreatingDatabase ErrCreatingDatabase
    deriving (Show, Eq)

-- | Mutable resource that may hold a 'WalletInstance'.
type WalletResource = Resource.Resource ErrDatabase WalletIO.WalletInstance

-- | Error indicating that the 'WalletResource' does not hold a wallet.
data ErrWalletResource
    = ErrNoWallet (Resource.ErrResourceMissing ErrDatabase)
    | ErrWalletPresent
        (Resource.ErrResourceExists ErrDatabase WalletIO.WalletInstance)

instance Show ErrWalletResource where
    show = \case
        ErrNoWallet e -> case e of
            ErrNotInitialized -> "Wallet is not initialized"
            ErrStillInitializing -> "Wallet is still initializing"
            ErrVanished e' -> "Wallet absent and vanished: " <> show e'
            ErrFailedToInitialize e' ->
                "Wallet failed to initialize (no wallet): "
                    <> show e'
        ErrWalletPresent e -> case e of
            ErrAlreadyInitializing -> "Wallet is already initializing"
            ErrAlreadyInitialized _ -> "Wallet is already initialized"
            ErrAlreadyVanished e' -> "Wallet vanished: " <> show e'
            ErrAlreadyFailedToInitialize e' ->
                "Wallet failed to initialize (wallet present): "
                    <> show e'

-- | Monad for acting on a 'WalletResource'.
type WalletResourceM = ReaderT WalletResource (ExceptT ErrWalletResource IO)

-- | Run a 'WalletResourceM' action on a 'WalletResource'.
runWalletResourceM
    :: WalletResourceM a
    -> WalletResource
    -> IO (Either ErrWalletResource a)
runWalletResourceM action resource =
    runExceptT (runReaderT action resource)

-- | Run an 'IO' function on the 'WalletInstance'.
onWalletInstance
    :: (WalletIO.WalletInstance -> IO a)
    -> WalletResourceM a
onWalletInstance action = ReaderT $ \resource ->
    ExceptT
        $ first ErrNoWallet <$> Resource.onResource action resource

{-----------------------------------------------------------------------------
    Initialization
------------------------------------------------------------------------------}

-- | Prefix for deposit wallets on disk.
depositPrefix :: String
depositPrefix = "deposit-"

-- | Scan a directory for deposit wallets.
scanDirectoryForDepositPrefix :: FilePath -> IO [FilePath]
scanDirectoryForDepositPrefix fp = do
    files <- listDirectory fp
    pure $ filter (depositPrefix `isPrefixOf`) files

-- | Try to open an existing wallet
findTheDepositWalletOnDisk
    :: FilePath
    -- ^ Path to the wallet database directory
    -> (Either ErrLoadingDatabase WalletIO.WalletStore -> IO a)
    -- ^ Action to run if the wallet is found
    -> IO a
findTheDepositWalletOnDisk fp action = do
    ds <- scanDirectoryForDepositPrefix fp
    case ds of
        [d] -> do
            (xpub, users) <- deserialise <$> BL.readFile (fp </> d)
            case xpubFromBytes xpub of
                Nothing -> action $ Left $ ErrDatabaseCorrupted (fp </> d)
                Just identity -> do
                    let state =
                            fromXPubAndGenesis
                                identity
                                (fromIntegral @Int users)
                                (error "FIXME")
                    store <- newStore
                    writeS store state
                    action $ Right store
        [] -> action $ Left $ ErrDatabaseNotFound fp
        ds' -> action $ Left $ ErrMultipleDatabases ((fp </>) <$> ds')

-- | Try to create a new wallet
createTheDepositWalletOnDisk
    :: FilePath
    -- ^ Path to the wallet database directory
    -> XPub
    -- ^ Id of the wallet
    -> Word31
    -- ^ Max number of users ?
    -> (Maybe WalletIO.WalletStore -> IO a)
    -- ^ Action to run if the wallet is created
    -> IO a
createTheDepositWalletOnDisk fp identity users action = do
    ds <- scanDirectoryForDepositPrefix fp
    case ds of
        [] -> do
            BL.writeFile (fp </> depositPrefix <> hashWalletId identity)
                $ serialise (xpubToBytes identity, fromIntegral users :: Int)
            store <- newStore
            action $ Just store
        _ -> do
            action Nothing
  where
    hashWalletId :: XPub -> String
    hashWalletId =
        B8.unpack
            . convertToBase Base64
            . blake2b160
            . xpubPublicKey

-- | Load an existing wallet from disk.
loadWallet
    :: WalletIO.WalletBootEnv IO
    -- ^ Environment for the wallet
    -> FilePath
    -- ^ Path to the wallet database directory
    -> Tracer IO (ResourceStatus ErrDatabase WalletIO.WalletInstance)
    -> WalletResourceM ()
loadWallet bootEnv fp trs = do
    let action :: (WalletIO.WalletInstance -> IO b) -> IO (Either ErrDatabase b)
        action f = findTheDepositWalletOnDisk fp $ \case
            Right wallet ->
                Right <$> do
                    WalletIO.withWalletLoad
                        (WalletIO.WalletEnv bootEnv wallet)
                        f
            Left e -> pure $ Left $ ErrLoadingDatabase e
    resource <- ask
    lift
        $ ExceptT
        $ first ErrWalletPresent
            <$> Resource.putResource action trs resource

-- | Initialize a new wallet from an 'XPub'.
initXPubWallet
    :: WalletIO.WalletBootEnv IO
    -- ^ Environment for the wallet
    -> FilePath
    -- ^ Path to the wallet database directory
    -> Tracer IO (ResourceStatus ErrDatabase WalletIO.WalletInstance)
    -> XPub
    -- ^ Id of the wallet
    -> Word31
    -- ^ Max number of users ?
    -> WalletResourceM ()
initXPubWallet bootEnv fp trs xpub users = do
    let action :: (WalletIO.WalletInstance -> IO b) -> IO (Either ErrDatabase b)
        action f = createTheDepositWalletOnDisk fp xpub users $ \case
            Just wallet -> do
                fmap Right
                    $ WalletIO.withWalletInit
                        (WalletIO.WalletEnv bootEnv wallet)
                        xpub
                        users
                        $ \i -> do
                            ls <- WalletIO.listCustomers i
                            last ls `seq` f i
            Nothing ->
                pure
                    $ Left
                    $ ErrCreatingDatabase
                    $ ErrDatabaseAlreadyExists fp
    resource <- ask
    lift
        $ ExceptT
        $ first ErrWalletPresent
            <$> Resource.putResource action trs resource

walletExists :: FilePath -> WalletResourceM Bool
walletExists fp = liftIO $ findTheDepositWalletOnDisk fp $ \case
    Right _ -> pure True
    Left _ -> pure False

walletPublicIdentity :: WalletResourceM WalletPublicIdentity
walletPublicIdentity = onWalletInstance WalletIO.walletPublicIdentity

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
listCustomers
    :: WalletResourceM [(Customer, Address)]
listCustomers = onWalletInstance WalletIO.listCustomers

createAddress
    :: Customer
    -> WalletResourceM Address
createAddress = onWalletInstance . WalletIO.createAddress

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}
getWalletTip :: WalletResourceM Read.ChainPoint
getWalletTip = onWalletInstance WalletIO.getWalletTip

availableBalance :: WalletResourceM Read.Value
availableBalance = onWalletInstance WalletIO.availableBalance

getCustomerHistory
    :: Customer
    -> WalletResourceM [Wallet.TxSummary]
getCustomerHistory = onWalletInstance . WalletIO.getCustomerHistory

getCustomerHistories
    :: (Read.ChainPoint, Read.ChainPoint)
    -> WalletResourceM (Map.Map Customer Wallet.ValueTransfer)
getCustomerHistories = onWalletInstance . WalletIO.getCustomerHistories

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment
    :: [(Address, Read.Value)]
    -> WalletResourceM (Maybe Write.TxBody)
createPayment = onWalletInstance . WalletIO.createPayment

getBIP32PathsForOwnedInputs
    :: Write.TxBody
    -> WalletResourceM [BIP32Path]
getBIP32PathsForOwnedInputs =
    onWalletInstance . WalletIO.getBIP32PathsForOwnedInputs

signTxBody
    :: Write.TxBody
    -> WalletResourceM (Maybe Write.Tx)
signTxBody = onWalletInstance . WalletIO.signTxBody
