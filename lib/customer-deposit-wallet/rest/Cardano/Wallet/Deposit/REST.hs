{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    , initWallet
    , loadWallet

      -- ** Mapping between customers and addresses
    , listCustomers
    , customerAddress
    , addressToCustomer

      -- ** Reading from the blockchain
    , getWalletTip
    , availableBalance
    , getTxHistoryByCustomer
    , getTxHistoryByTime
    , WalletIO.ResolveAddress

      -- ** Writing to the blockchain
    , createPayment
    , getBIP32PathsForOwnedInputs
    , signTx
    , walletExists
    , walletPublicIdentity
    , deleteWallet
    , deleteTheDepositWalletOnDisk

      -- * Internals
    , inspectTx
    , onWalletInstance
    , networkTag
    , resolveCurrentEraTx
    , canSign
    , submitTx
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubToBytes
    )
import Cardano.Crypto.Wallet
    ( XPrv
    , XPub (..)
    , unXPrv
    , unXPub
    , xprv
    , xpub
    )
import Cardano.Wallet.Address.BIP32
    ( BIP32Path
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    , WalletPublicIdentity
    , genesisData
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ErrResourceExists (..)
    , ErrResourceMissing (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( CanSign
    , Credentials
    , CurrentEraResolvedTx
    , Customer
    , ErrCreatePayment
    , InspectTx
    , Passphrase
    , Word31
    , fromCredentialsAndGenesis
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( ByCustomer
    , ByTime
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( accountXPubFromCredentials
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Codec.Serialise
    ( Serialise (..)
    , deserialise
    , serialise
    )
import Control.DeepSeq
    ( deepseq
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
import Data.ByteString
    ( ByteString
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
    , removeFile
    )
import System.FilePath
    ( (</>)
    )

import qualified Cardano.Wallet.Deposit.IO as WalletIO
import qualified Cardano.Wallet.Deposit.IO.Network.Type as Network
import qualified Cardano.Wallet.Deposit.IO.Resource as Resource
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

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
type WalletResource =
    Resource.Resource ErrDatabase WalletIO.WalletInstance

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
            ErrClosing -> "Wallet is closing"
        ErrWalletPresent e -> case e of
            ErrAlreadyInitializing -> "Wallet is already initializing"
            ErrAlreadyInitialized _ -> "Wallet is already initialized"
            ErrAlreadyVanished e' -> "Wallet vanished: " <> show e'
            ErrAlreadyFailedToInitialize e' ->
                "Wallet failed to initialize (wallet present): "
                    <> show e'
            ErrAlreadyClosing -> "Wallet is already closing"

-- | Monad for acting on a 'WalletResource'.
type WalletResourceM =
    ReaderT WalletResource (ExceptT ErrWalletResource IO)

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
scanDirectoryForDepositPrefix dir = do
    files <- listDirectory dir
    pure $ filter (depositPrefix `isPrefixOf`) files

deleteTheDepositWalletOnDisk :: FilePath -> IO ()
deleteTheDepositWalletOnDisk dir = do
    ds <- scanDirectoryForDepositPrefix dir
    case ds of
        [d] -> removeFile (dir </> d)
        _ -> pure ()

-- | Try to open an existing wallet
findTheDepositWalletOnDisk
    :: WalletBootEnv IO
    -> FilePath
    -- ^ Path to the wallet database directory
    -> (Either ErrLoadingDatabase WalletIO.WalletStore -> IO a)
    -- ^ Action to run if the wallet is found
    -> IO a
findTheDepositWalletOnDisk env dir action = do
    ds <- scanDirectoryForDepositPrefix dir
    case ds of
        [d] -> do
            (credentials, customers) <-
                deserialise <$> BL.readFile (dir </> d)
            let state =
                    fromCredentialsAndGenesis
                        credentials
                        (fromIntegral @Int customers)
                        (genesisData env)
            store <- newStore
            writeS store state
            action $ Right store
        [] -> action $ Left $ ErrDatabaseNotFound dir
        ds' -> action $ Left $ ErrMultipleDatabases ((dir </>) <$> ds')

instance Serialise XPub where
    encode = encode . unXPub
    decode = do
        b <- decode
        case xpub b of
            Right x -> pure x
            Left e -> fail e

instance Serialise XPrv where
    encode = encode . unXPrv
    decode = do
        b :: ByteString <- decode
        case xprv b of
            Right x -> pure x
            Left e -> fail e

instance Serialise Credentials

-- | Try to create a new wallet
createTheDepositWalletOnDisk
    :: Tracer IO String
    -- ^ Tracer for logging
    -> FilePath
    -- ^ Path to the wallet database directory
    -> Credentials
    -- ^ Id of the wallet
    -> Word31
    -- ^ Maximum customer index
    -> (Maybe WalletIO.WalletStore -> IO a)
    -- ^ Action to run if the wallet is created
    -> IO a
createTheDepositWalletOnDisk _tr dir credentials users action = do
    ds <- scanDirectoryForDepositPrefix dir
    case ds of
        [] -> do
            let fp = dir </> depositPrefix <> hashWalletId credentials
            BL.writeFile fp
                $ serialise (credentials, fromIntegral users :: Int)
            store <- newStore
            action $ Just store
        _ -> do
            action Nothing
  where
    hashWalletId :: Credentials -> String
    hashWalletId =
        B8.unpack
            . convertToBase Base16
            . blake2b160
            . xpubToBytes
            . accountXPubFromCredentials

-- | Load an existing wallet from disk.
loadWallet
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> WalletIO.WalletBootEnv IO
    -- ^ Environment for the wallet
    -> FilePath
    -- ^ Path to the wallet database directory
    -> WalletResourceM ()
loadWallet wtc bootEnv dir = do
    let action
            :: (WalletIO.WalletInstance -> IO b) -> IO (Either ErrDatabase b)
        action f = findTheDepositWalletOnDisk bootEnv dir $ \case
            Right wallet ->
                Right
                    <$> WalletIO.withWalletLoad wtc
                        (WalletIO.WalletEnv bootEnv wallet)
                        f
            Left e -> pure $ Left $ ErrLoadingDatabase e
    resource <- ask
    lift
        $ ExceptT
        $ first ErrWalletPresent
            <$> Resource.putResource action resource

-- | Initialize a new wallet from an 'XPub'.
initWallet
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -- ^ Tracer for logging
    -> WalletIO.WalletBootEnv IO
    -- ^ Environment for the wallet
    -> FilePath
    -- ^ Path to the wallet database directory
    -> Credentials
    -- ^ Id of the wallet
    -> Word31
    -- ^ Max number of users ?
    -> WalletResourceM ()
initWallet wtc tr bootEnv dir credentials users = do
    let action
            :: (WalletIO.WalletInstance -> IO b) -> IO (Either ErrDatabase b)
        action f = createTheDepositWalletOnDisk tr dir credentials users $ \case
            Just wallet -> do
                fmap Right
                    $ WalletIO.withWalletInit wtc
                        (WalletIO.WalletEnv bootEnv wallet)
                        credentials
                        users
                    $ \i -> do
                        addresses <- map snd <$> WalletIO.listCustomers i
                        addresses `deepseq` f i
            Nothing ->
                pure
                    $ Left
                    $ ErrCreatingDatabase
                    $ ErrDatabaseAlreadyExists dir
    resource <- ask
    lift
        $ ExceptT
        $ first ErrWalletPresent
            <$> Resource.putResource action resource

deleteWallet :: FilePath -> WalletResourceM ()
deleteWallet dir = do
    resource <- ask
    lift
        $ ExceptT
        $ first ErrNoWallet
            <$> Resource.closeResource resource
    liftIO $ deleteTheDepositWalletOnDisk dir

walletExists :: FilePath -> IO Bool
walletExists dir = do
    r <- scanDirectoryForDepositPrefix dir
    case r of
        [] -> pure False
        _ -> pure True

walletPublicIdentity :: WalletResourceM WalletPublicIdentity
walletPublicIdentity = onWalletInstance WalletIO.walletPublicIdentity

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}

-- | List all tracked customers addresses.
listCustomers :: WalletResourceM [(Customer, Address)]
listCustomers = onWalletInstance WalletIO.listCustomers

-- | Retrieve the address for a customer if it's tracked by the wallet.
customerAddress :: Customer -> WalletResourceM (Maybe Address)
customerAddress = onWalletInstance . WalletIO.customerAddress

addressToCustomer :: WalletResourceM WalletIO.ResolveAddress
addressToCustomer = onWalletInstance WalletIO.addressToCustomer

{-----------------------------------------------------------------------------
    Operations
    Reading from the blockchain
------------------------------------------------------------------------------}
getWalletTip :: WalletResourceM Read.ChainPoint
getWalletTip = onWalletInstance WalletIO.getWalletTip

availableBalance :: WalletResourceM Read.Value
availableBalance = onWalletInstance WalletIO.availableBalance

getTxHistoryByCustomer
    :: WalletResourceM ByCustomer
getTxHistoryByCustomer = onWalletInstance WalletIO.getTxHistoryByCustomer

getTxHistoryByTime
    :: WalletResourceM ByTime
getTxHistoryByTime = onWalletInstance WalletIO.getTxHistoryByTime

networkTag :: WalletResourceM Read.NetworkTag
networkTag = onWalletInstance WalletIO.networkTag

{-----------------------------------------------------------------------------
    Operations
    Writing to blockchain
------------------------------------------------------------------------------}

createPayment
    :: [(Address, Read.Value)]
    -> WalletResourceM (Either ErrCreatePayment CurrentEraResolvedTx)
createPayment = onWalletInstance . WalletIO.createPayment

getBIP32PathsForOwnedInputs
    :: Write.Tx
    -> WalletResourceM [BIP32Path]
getBIP32PathsForOwnedInputs =
    onWalletInstance . WalletIO.getBIP32PathsForOwnedInputs

canSign :: WalletResourceM CanSign
canSign = onWalletInstance WalletIO.canSign

signTx
    :: Write.Tx
    -> Passphrase
    -> WalletResourceM (Maybe Write.Tx)
signTx tx = onWalletInstance . WalletIO.signTx tx

inspectTx
    :: CurrentEraResolvedTx
    -> WalletResourceM InspectTx
inspectTx = onWalletInstance . WalletIO.inspectTx

resolveCurrentEraTx :: Write.Tx -> WalletResourceM CurrentEraResolvedTx
resolveCurrentEraTx = onWalletInstance . WalletIO.resolveCurrentEraTx

submitTx :: Write.Tx -> WalletResourceM (Either Network.ErrPostTx ())
submitTx = onWalletInstance . WalletIO.submitTx
