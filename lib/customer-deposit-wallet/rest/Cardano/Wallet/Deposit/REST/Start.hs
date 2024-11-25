{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.REST.Start
    ( loadDepositWalletFromDisk
    , newFakeBootEnv
    , newBootEnv
    , mockFundTheWallet
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (..)
    )
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    )
import Cardano.Wallet.Deposit.IO.Network.NodeToClient
    ( CardanoBlock
    , NetworkLayer
    , StandardCrypto
    , fromNetworkLayer
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    , mapBlock
    , postTx
    )
import Cardano.Wallet.Deposit.REST
    ( ErrWalletResource
    , WalletResource
    , availableBalance
    , customerAddress
    , getTxHistoryByCustomer
    , getTxHistoryByTime
    , listCustomers
    , loadWallet
    , runWalletResourceM
    , walletExists
    )
import Cardano.Wallet.Deposit.Write
    ( addTxOut
    , emptyTxBody
    , mkAda
    , mkTx
    , mkTxOut
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Control.Tracer
    ( Tracer
    , stdoutTracer
    , traceWith
    )
import Data.Functor.Contravariant
    ( (>$<)
    )

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Wallet.Deposit.Read as Read

lg :: (MonadIO m, Show a) => Tracer IO String -> String -> a -> m ()
lg tr p x = liftIO $ traceWith tr $ p <> ": " <> show x

loadDepositWalletFromDisk
    :: Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> IO ()
loadDepositWalletFromDisk tr dir env resource = do
    result <- runExceptT $ do
        exists <- ExceptT $ flip runWalletResourceM resource $ do
            test <- liftIO $ walletExists dir
            liftIO $ print test
            when test $ do
                lg tr "Loading wallet from" dir
                loadWallet env dir
                lg tr "Wallet loaded from" dir
            pure test
        liftIO $ threadDelay 1_000_000
        when exists $ do
            ExceptT $ mockFundTheWallet (networkEnv env) resource
            ExceptT $ flip runWalletResourceM resource $ do
                liftIO $ putStrLn "Available balance"
                availableBalance >>= liftIO . print
                liftIO $ putStrLn "Tx history by customer"
                getTxHistoryByCustomer >>= liftIO . print
                liftIO $ putStrLn "Tx history by time"
                getTxHistoryByTime >>= liftIO . print
                liftIO $ putStrLn "List customers"
                listCustomers >>= liftIO . print
                liftIO $ putStrLn "UTxO"
    case result of
        Left e -> error $ show e
        Right _ -> pure ()

mockFundTheWallet
    :: NetworkEnv IO z
    -> WalletResource
    -> IO (Either ErrWalletResource ())
mockFundTheWallet network resource = flip runWalletResourceM resource $ do
    Just address <- customerAddress 0
    let tx =
            mkTx
                $ fst
                $ addTxOut (mkTxOut address (mkAda 1_000_000)) emptyTxBody
    Right () <- liftIO $ postTx network tx
    pure ()

newFakeBootEnv :: Maybe FilePath -> IO (WalletBootEnv IO)
newFakeBootEnv genesisFile = do
    eGenesisData <- runExceptT $ case genesisFile of
        Nothing -> ExceptT $ pure $ Right Read.mockGenesisDataMainnet
        Just file -> fst <$> Byron.readGenesisData file
    print genesisFile
    print eGenesisData
    print $ Read.getNetworkId <$> eGenesisData
    case eGenesisData of
        Left e -> error $ show e
        Right genesisData' ->
            WalletBootEnv
                (show >$< stdoutTracer)
                genesisData'
                . mapBlock Read.EraValue
                <$> newNetworkEnvMock

newBootEnv
    :: Maybe FilePath
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -> IO (WalletBootEnv IO)
newBootEnv genesisFile nl = do
    eGenesisData <- runExceptT $ case genesisFile of
        Nothing -> ExceptT $ pure $ Right Read.mockGenesisDataMainnet
        Just file -> fst <$> Byron.readGenesisData file
    print genesisFile
    print eGenesisData
    print $ Read.getNetworkId <$> eGenesisData
    case eGenesisData of
        Left e -> error $ show e
        Right genesisData' ->
            return $ WalletBootEnv
                (show >$< stdoutTracer)
                genesisData'
                (fromNetworkLayer nl)
