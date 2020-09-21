{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Integration.Framework.Context
    ( Context (..)
    , TxDescription (..)
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters )
import Cardano.Wallet.Transaction
    ( DelegationAction )
import Data.Proxy
    ( Proxy (..) )
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
data Context t = Context
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
    , _target
        :: Proxy t
    }
    deriving Generic

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
