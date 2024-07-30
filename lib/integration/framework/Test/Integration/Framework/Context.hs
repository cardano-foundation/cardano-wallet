{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    , TxDescription (..)
    , clientEnv
    , runClientRequest
    , runPartialClientRequest
    ) where

import Prelude

import Cardano.Address
    ( Address
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra
    )
import Cardano.Wallet.Faucet
    ( Faucet
    )
import Cardano.Wallet.Launch.Cluster
    ( TestnetMagic
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo
    , NetworkParameters
    , PoolRetirementCertificate
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Transaction
    ( DelegationAction
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Data.IORef
    ( IORef
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Network.HTTP.Client
    ( Manager
    )
import Network.URI
    ( URI
    )
import Servant.Client
    ( ClientEnv
    , ClientError
    , ClientM
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )

-- | Context for integration tests.
--
data Context = Context
    { _cleanup
        :: IO ()
        -- ^ A cleanup action.
    , _manager
        :: (URI, Manager)
        -- ^ The underlying base URL and manager used by the wallet client.
    , _walletPort
        :: Port "wallet"
        -- ^ Server TCP port.
    , _faucet
        :: Faucet
        -- ^ Provides access to funded wallets.
    , _networkParameters :: NetworkParameters
        -- ^ Blockchain parameters for the underlying chain.
    , _testnetMagic :: TestnetMagic
        -- ^ The protocol magic of the underlying test network.
    , _poolGarbageCollectionEvents
        :: IORef [PoolGarbageCollectionEvent]
        -- ^ The complete list of pool garbage collection events.
        -- Most recent events are stored at the head of the list.
    , _mainEra
        :: ApiEra
        -- ^ The main era the tests are expected to run on. Allows tests to make
        -- era-specific assertions.
    , _smashUrl :: Text
        -- ^ Base URL of the mock smash server.

    , _mintSeaHorseAssets :: Int -> Int -> Coin -> [Address] -> IO ()
        -- ^ TODO: Remove once we can unify cardano-wallet-integration and
        -- cardano-wallet:integration, or when the wallet supports minting.
        --
        -- Cannot be used by several tests at a time. (!)
    }
    deriving Generic

-- | Records the parameters and return value of a single call to the
--   'removeRetiredPools' operation of 'Pool.DB.DBLayer'.
--
data PoolGarbageCollectionEvent = PoolGarbageCollectionEvent
    { poolGarbageCollectionEpochNo
        :: EpochNo
        -- ^ The epoch number parameter.
    , poolGarbageCollectionCertificates
        :: [PoolRetirementCertificate]
        -- ^ The pools that were removed from the database.
    }
    deriving (Eq, Show)

-- | Describe a transaction in terms of its inputs and outputs.
data TxDescription
    = DelegDescription DelegationAction
    | PaymentDescription
        { nInputs
            :: Int
        , nOutputs
            :: Int
        , nChanges
            :: Int
        }
    deriving Show

-- one day we will export the manager from the context
clientEnv :: Context -> ClientEnv
clientEnv ctx = case parseBaseUrl $ show (fst $ _manager ctx) of
    Left _ -> error "Invalid base URL"
    Right bu -> mkClientEnv (snd $ _manager ctx) bu

runClientRequest :: MonadIO m => Context -> ClientM a -> m (Either ClientError a)
runClientRequest ctx action = liftIO $ runClientM action (clientEnv ctx)

runPartialClientRequest :: MonadIO m => Context -> ClientM a -> m a
runPartialClientRequest ctx action = liftIO $ do
    res <- runClientRequest ctx action
    case res of
        Left e -> fail $ show e
        Right a -> return a
