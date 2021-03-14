{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    , TxDescription (..)
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( ApiEra )
import Cardano.Wallet.Primitive.Types
    ( EpochNo, NetworkParameters, PoolRetirementCertificate )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Transaction
    ( DelegationAction )
import Data.IORef
    ( IORef )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager )
import Numeric.Natural
    ( Natural )
import Test.Integration.Faucet
    ( Faucet )

-- | Context for integration tests.
--
data Context = Context
    { _cleanup
        :: IO ()
        -- ^ A cleanup action.
    , _manager
        :: (Text, Manager)
        -- ^ The underlying base URL and manager used by the wallet client.
    , _walletPort
        :: Port "wallet"
        -- ^ Server TCP port.
    , _faucet
        :: Faucet
        -- ^ Provides access to funded wallets.
    , _feeEstimator :: TxDescription -> (Natural, Natural)
        -- ^ A fee estimator.
    , _networkParameters :: NetworkParameters
        -- ^ Blockchain parameters for the underlying chain.
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

    , _mintSeaHorseAssets :: Int -> [Address] -> IO ()
        -- ^ TODO: Remove once we can unify cardano-wallet-core-integration and
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
