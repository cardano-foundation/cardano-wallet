module Cardano.Wallet.Deposit.REST.Start
    ( loadDepositWalletFromDisk
    , newFakeBootEnv
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (..)
    )
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( mapBlock
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
import Control.Tracer
    ( Tracer
    , stdoutTracer
    , traceWith
    )
import Data.Functor.Contravariant
    ( (>$<)
    )

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
    result <- flip runWalletResourceM resource $ do
        test <- walletExists dir
        when test $ do
            lg tr "Loading wallet from" dir
            loadWallet env dir
            lg tr "Wallet loaded from" dir
    case result of
        Left e -> error $ show e
        Right _ -> pure ()

newFakeBootEnv :: IO (WalletBootEnv IO)
newFakeBootEnv =
    WalletBootEnv
        (show >$< stdoutTracer)
        Read.mockGenesisDataMainnet
        . mapBlock Read.EraValue
        <$> newNetworkEnvMock
