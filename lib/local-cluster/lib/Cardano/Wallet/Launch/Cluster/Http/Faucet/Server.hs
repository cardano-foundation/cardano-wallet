{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.Server
    ( FaucetHandlers (..)
    , mkFaucetHandlers
    , newNodeConnVar
    , NodeConnVar (..)
    , mkFaucetServer
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster
    ( Config
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , runClusterM
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( sendFaucetAssetsTo
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.API
    ( FaucetAPI
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets (..)
    , WithNetwork (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Control.Exception
    ( throwIO
    )
import Data.Bifunctor
    ( first
    )
import Data.Functor
    ( ($>)
    )
import Servant
    ( Handler
    , HasServer (..)
    , NoContent (..)
    , ServerError (..)
    , err500
    )
import UnliftIO
    ( MonadIO (..)
    , atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )

import qualified Cardano.Address as Address

-- | Handlers for local-cluster application
newtype FaucetHandlers = FaucetHandlers
    { handleSendAssets :: SendFaucetAssets -> IO ()
    }

-- Handler for sending assets to some addresses
sendFaucetAssetsHandler
    :: CardanoNodeConn
    -> SendFaucetAssets
    -> ClusterM ()
sendFaucetAssetsHandler
    relayConnection
    SendFaucetAssets{..} = do
        sendFaucetAssetsTo relayConnection batchSize
            $ first mkLibAddress <$> assets

mkLibAddress :: Address -> Address.Address
mkLibAddress (Address a) = Address.unsafeMkAddress a

-- | A thread-safe variable to store the connection to the Cardano node
-- The connection could not be available at the time of creation
data NodeConnVar = NodeConnVar
    { getNodeConn :: IO (Maybe CardanoNodeConn)
    , setNodeConn :: CardanoNodeConn -> IO ()
    }

-- | Create a new, empty 'NodeConnVar'
newNodeConnVar :: IO NodeConnVar
newNodeConnVar = do
    var <- newTVarIO Nothing
    pure
        NodeConnVar
            { getNodeConn = readTVarIO var
            , setNodeConn = atomically . writeTVar var . Just
            }

-- | Create an application handlers record
mkFaucetHandlers
    :: NodeConnVar
    -> Config
    -> FaucetHandlers
mkFaucetHandlers mRelayConnection config =
    FaucetHandlers
        { handleSendAssets = \ass -> do
            mConn <- getNodeConn mRelayConnection
            case mConn of
                Just relayConnection ->
                    runClusterM config
                        $ sendFaucetAssetsHandler relayConnection ass
                Nothing ->
                    throwIO
                        err500{errBody = "The relay node is not available yet"}
        }

mkFaucetServer
    :: FaucetHandlers
    -> ServerT (FaucetAPI n) Handler
mkFaucetServer handlers (WithNetwork ass) =
    liftIO $ handleSendAssets handlers ass $> NoContent
