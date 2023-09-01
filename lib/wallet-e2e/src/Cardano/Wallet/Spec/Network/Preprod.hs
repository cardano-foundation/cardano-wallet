{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Preprod
    ( nodeWalletSetup
    ) where

import qualified Cardano.Node.Cli.Launcher as Node
import qualified Cardano.Wallet.Cli.Launcher as Wallet
import qualified Cardano.Wallet.Spec.Network.Node.Cli as NodeCli
import qualified Cardano.Wallet.Spec.Network.Wallet.Cli as WalletCli

import Cardano.Node.Cli.Launcher
    ( NodeProcessConfig (..) )
import Cardano.Wallet.Cli.Launcher
    ( WalletProcessConfig (..) )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..) )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )
import Control.Monad.Trans.Resource
    ( allocate, runResourceT )
import Control.Retry
    ( capDelay, fibonacciBackoff, retrying )
import Path
    ( Abs, Dir, Path, reldir, relfile, (</>) )

nodeWalletSetup
    :: Path Abs Dir
    -> Path Abs Dir
    -> (NetworkConfig -> IO ())
    -> IO ()
nodeWalletSetup stateDir nodeConfigDir withNetworkConfig = runResourceT do
    -- Start node
    let nodeDir = stateDir </> [reldir|node|]
    let nodeProcessConfig =
            NodeProcessConfig
                { nodeDir
                , nodeConfig = nodeConfigDir </> [relfile|config.json|]
                , nodeTopology = nodeConfigDir </> [relfile|topology.json|]
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
                    nodeConfigDir </> [relfile|byron-genesis.json|]
                }
    (_walletReleaseKey, (_walletInstance, walletApi)) <-
        allocate (Wallet.start walletProcessConfig) (Wallet.stop . fst)

    -- Wait for the node to start and open the socket
    void $ retrying
        (capDelay 10_000 (fibonacciBackoff 1_000))
        do \_retryStatus -> pure
        do
            \_retryStatus -> liftIO do
                NodeCli.queryTip nodeApi >>= \case
                    Left (NodeCli.CliErrorExitCode _code _out) ->
                        False <$ putTextLn "Waiting for the node socket ..."
                    _ -> do
                        putTextLn "Node socket is ready"
                        pure True

    -- Wait for the node to sync with the network
    waitFor (>= 99.99) do
        NodeCli.queryTip nodeApi >>= \case
            Left (NodeCli.CliErrorExitCode _code _out) ->
                0.0 <$ putTextLn "Waiting for the node socket ..."
            Left (NodeCli.CliErrorDecode (_jsonPath, e) _out) -> do
                putTextLn $ "Failed to decode cardano-cli response: " <> toText e
                pure 100
            Right tip -> do
                let progress = NodeCli.syncProgress tip
                progress <$ putTextLn do
                    "Node sync progress: " <> show progress <> "%"

    -- Wait for the wallet to sync with the node
    waitFor (== Just NodeIsSynced) do
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

waitFor :: MonadIO m => (a -> Bool) -> IO a -> m ()
waitFor condition action =
    void
        $ retrying
            (capDelay 10_000_000 (fibonacciBackoff 1_000_000))
            (\_retryStatus -> pure . not . condition)
            (\_retryStatus -> liftIO action)
