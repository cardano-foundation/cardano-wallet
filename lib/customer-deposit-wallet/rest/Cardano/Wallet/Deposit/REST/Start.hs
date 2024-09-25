module Cardano.Wallet.Deposit.REST.Start
    ( loadDepositWalletFromDisk
    , fakeBootEnv
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (..)
    , WalletInstance
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ResourceStatus
    )
import Cardano.Wallet.Deposit.REST
    ( ErrDatabase
    , WalletResource
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
import Control.Tracer
    ( Tracer
    , traceWith
    )

lg :: (MonadIO m, Show a) => Tracer IO String -> String -> a -> m ()
lg tr p x = liftIO $ traceWith tr $ p <> ": " <> show x

loadDepositWalletFromDisk
    :: Tracer IO String
    -> Tracer IO (ResourceStatus ErrDatabase WalletInstance)
    -> FilePath
    -> WalletBootEnv IO
    -> WalletResource
    -> IO ()
loadDepositWalletFromDisk tr sttr dir env resource = do
    result <- flip runWalletResourceM resource $ do
        test <- walletExists dir
        when test $ do
            lg tr "Loading wallet from" dir
            loadWallet env dir sttr
            lg tr "Wallet loaded from" dir
    case result of
        Left e -> error $ show e
        Right _ -> pure ()

fakeBootEnv :: WalletBootEnv m
fakeBootEnv =
    ( WalletBootEnv
        (error "Not defined")
        (error "Not defined")
        (error "Not defined")
    )
