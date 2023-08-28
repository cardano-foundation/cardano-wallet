{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Preprod
    ( nodeWalletSetup
    ) where

import qualified Cardano.Wallet.Spec.Network.Node as Node
import qualified Cardano.Wallet.Spec.Network.Node.Cli as NodeCli
import qualified Cardano.Wallet.Spec.Network.Wallet as Wallet
import qualified Cardano.Wallet.Spec.Network.Wallet.Cli as WalletCli

import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..) )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )
import Cardano.Wallet.Spec.Network.Node
    ( NodeProcessConfig (..) )
import Cardano.Wallet.Spec.Network.Wallet
    ( WalletProcessConfig (..) )
import Control.Monad.Trans.Resource
    ( allocate, runResourceT )
import Control.Retry
    ( capDelay, fibonacciBackoff, retrying )
import Path
    ( Abs, Dir, Path, reldir, relfile, (</>) )
import Path.IO
    ( makeAbsolute )

nodeWalletSetup :: Path Abs Dir -> (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup stateDir withNetworkConfig = runResourceT do
    preprodDir <- makeAbsolute [reldir|config/cardano-node/preprod|]

    -- Start node
    let nodeDir = stateDir </> [reldir|node|]
    let nodeProcessConfig =
            NodeProcessConfig
                { nodeDir
                , nodeConfig = preprodDir </> [relfile|config.json|]
                , nodeTopology = preprodDir </> [relfile|topology.json|]
                , nodeDatabase = nodeDir </> [reldir|db|]
                }
    (_nodeReleaseKey, (_nodeInstance, nodeApi)) <-
        allocate (Node.start nodeProcessConfig) (Node.stop . fst)

    -- Start wallet
    let walletDir = stateDir </> [reldir|wallet|]
    let walletProcessConfig =
            WalletProcessConfig
                { walletDir
                , walletDatabase = walletDir </> [reldir|db|]
                , walletNodeApi = nodeApi
                , walletListenHost = Nothing
                , walletListenPort = Nothing
                , walletByronGenesis =
                    preprodDir </> [relfile|byron-genesis.json|]
                }
    (_walletReleaseKey, (_walletInstance, walletApi)) <-
        allocate (Wallet.start walletProcessConfig) (Wallet.stop . fst)

    -- Wait for the node to sync with the network
    let policy = capDelay 10_000_000 (fibonacciBackoff 1_000_000)

    _ <- retrying policy (\_rs -> pure . (< 99.99)) \_ -> liftIO do
        NodeCli.queryTip nodeApi >>= \case
            Left (NodeCli.CliErrorExitCode _code _out) ->
                0.0 <$ putTextLn "Waiting for the node socket ..."
            Left (NodeCli.CliErrorDecode e _out) -> do
                putTextLn $ "Failed to decode cardano-cli response: " <> toText e
                pure 100
            Right tip -> do
                let progress = NodeCli.syncProgress tip
                progress <$ putTextLn do
                    "Node sync progress: " <> show progress <> "%"

    -- Wait for the wallet to sync with the node
    _ <- retrying policy (\_rs -> pure . (/= Just NodeIsSynced)) \_ -> liftIO do
        WalletCli.queryNetworkInformation walletApi >>= \case
            Left err -> do
                putTextLn ("Waiting for wallet to start: " <> show err)
                pure Nothing
            Right networkInformation -> do
                let nodeStatus = WalletCli.nodeStatus networkInformation
                Just nodeStatus <$ putTextLn do
                    "Node status as reported by Wallet: " <> show nodeStatus

    liftIO
        $ withNetworkConfig
            NetworkConfig
                { networkConfigWallet = walletApi
                , ..
                }
