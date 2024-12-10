{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.REST.Start
    ( loadDepositWalletFromDisk
    , newBootEnv
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (..)
    )
import Cardano.Wallet.Deposit.IO.Network.NodeToClient
    ( CardanoBlock
    , NetworkLayer
    , StandardCrypto
    , fromNetworkLayer
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , loadWallet
    , runWalletResourceM
    , walletExists
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
    :: Tracer IO ()
    -- ^ Tracer for wallet tip changes
    -> Tracer IO String
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> IO ()
loadDepositWalletFromDisk wtc tr dir env resource = do
    result <- flip runWalletResourceM resource $ do
        test <- liftIO $ walletExists dir
        when test $ do
            lg tr "Loading wallet from" dir
            loadWallet wtc env dir
            lg tr "Wallet loaded from" dir
        pure test
    case result of
        Left e -> error $ show e
        Right _ -> pure ()

newBootEnv
    :: Maybe FilePath
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -> IO (WalletBootEnv IO)
newBootEnv genesisFile nl = do
    eGenesisData <- runExceptT $ case genesisFile of
        Nothing -> ExceptT $ pure $ Right Read.mockGenesisDataMainnet
        Just file -> fst <$> Byron.readGenesisData file
    case eGenesisData of
        Left e -> error $ show e
        Right genesisData' ->
            return
                $ WalletBootEnv
                    (show >$< stdoutTracer)
                    genesisData'
                    (fromNetworkLayer nl)
