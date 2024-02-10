{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    , TxDescription (..)
    , rawNotice
    , notice
    ) where

import Prelude

import Cardano.Address
    ( Address
    )
import Cardano.CLI
    ( Port (..)
    )
import Cardano.Wallet.Api.Types
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
import Data.ByteString
    ( ByteString
    )
import Data.IORef
    ( IORef
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( NominalDiffTime
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
import Test.Hspec.Extra
    ( HasMetrics (..)
    )
import Test.HUnit.Lang
    ( HUnitFailure
    )
import Text.Pretty.Simple
    ( pShowNoColor
    )

import qualified Data.Text.Lazy as TL

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
    , _moveRewardsToScript
        :: (ByteString, Coin)
        -> IO ()
        -- ^ A function to inject rewards into some stake address.
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
    , _addSuccess :: String -> NominalDiffTime -> IO ()
        -- ^ Add a successful test to the metrics.
    , _addFailure :: String -> NominalDiffTime -> HUnitFailure -> IO ()
        -- ^ Add a failed test to the metrics.
    , _addTimeOut :: String -> NominalDiffTime -> IO ()
        -- ^ Add a timed out test to the metrics.
    , _testNotice :: String -> IO ()
        -- ^ A function to trace test debugging information.
    }
    deriving Generic

rawNotice
    :: (MonadIO m, Show a)
    => Context -- ^ context
    -> String -- ^ A prefix for the message
    -> a -- ^ The message
    -> m ()
rawNotice ctx pre x =
    liftIO
        $ _testNotice ctx
        $ pre <> ": " <> TL.unpack (pShowNoColor x)

notice
    :: (MonadIO m, Show a)
    => Context -- ^ context
    -> t  -- ^ Usually the wallet
    -> String -- ^ A prefix for the message
    -> (Context -> t -> m a) -- ^ A function to get the message
    -> m ()
notice ctx w pre f = do
    x <- f ctx w
    rawNotice ctx pre x

instance HasMetrics Context where
    putFailure ctx label failure time = _addFailure ctx label time failure
    putSuccess = _addSuccess
    putTimeout = _addTimeOut

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
