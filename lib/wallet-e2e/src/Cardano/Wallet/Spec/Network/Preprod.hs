{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Preprod
    ( nodeWalletSetup
    ) where

import qualified Cardano.Node.Cli.Launcher as Node
import qualified Cardano.Wallet.Cli.Launcher as Wallet
import qualified Cardano.Wallet.Spec.Data.Network.NodeStatus as NodeStatus
import qualified Cardano.Wallet.Spec.Network.Node.Cli as NodeCli
import qualified Cardano.Wallet.Spec.Network.Wallet.Cli as WalletCli

import Cardano.Node.Cli.Launcher
    ( NodeApi, NodeProcessConfig (..) )
import Cardano.Wallet.Cli.Launcher
    ( WalletApi, WalletProcessConfig (..) )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..) )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )
import Cardano.Wallet.Spec.Network.Node.Cli
    ( CliError, NodeTip )
import Cardano.Wallet.Spec.Network.Wallet.Cli
    ( NetworkInformation )
import Control.Monad.Trans.Resource
    ( allocate, runResourceT )
import Control.Retry
    ( RetryStatus, capDelay, fibonacciBackoff, retrying )
import Path
    ( Abs, Dir, Path, reldir, relfile, (</>) )

nodeWalletSetup
    :: Path Abs Dir
    -> Path Abs Dir
    -> (NetworkConfig -> IO ())
    -> IO ()
nodeWalletSetup stateDir nodeConfigDir withNetworkConfig = runResourceT do
    nodeApi <- startNode

    walletApi <- startWallet nodeApi

    unlessM (waitForNodeSocket nodeApi) do
        fail "Node socket is not available, giving up. Please check node logs."

    unlessM (waitUntilNodeIsSynced nodeApi) do
        fail "Node is not synced, giving up. Please check node logs."

    unlessM (waitUntilWalletIsSynced walletApi) do
        fail "Wallet is not synced, giving up. Please check wallet logs."

    liftIO do
        withNetworkConfig NetworkConfig{networkConfigWallet = walletApi, ..}
  where
    startNode = do
        let nodeDir = stateDir </> [reldir|node|]
        let nodeProcessConfig = NodeProcessConfig
                    { nodeDir
                    , nodeConfig = nodeConfigDir </> [relfile|config.json|]
                    , nodeTopology = nodeConfigDir </> [relfile|topology.json|]
                    , nodeDatabase = nodeDir </> [reldir|db|]
                    }
        (_nodeReleaseKey, (_nodeInstance, nodeApi)) <-
            allocate (Node.start nodeProcessConfig) (Node.stop . fst)
        pure nodeApi

    startWallet nodeApi = do
      let walletDir = stateDir </> [reldir|wallet|]
      let walletProcessConfig =
              WalletProcessConfig
                  { walletDir
                  , walletDatabase = walletDir </> [reldir|db|]
                  , walletNodeApi = nodeApi
                  , walletListenHost = Nothing
                  , walletListenPort = Nothing
                  , walletByronGenesisForTestnet = Just $
                      nodeConfigDir </> [relfile|byron-genesis.json|]
                  }
      (_walletReleaseKey, (_walletInstance, walletApi)) <-
          allocate (Wallet.start walletProcessConfig) (Wallet.stop . fst)
      pure walletApi

waitForNodeSocket :: forall m. MonadIO m => NodeApi -> m Bool
waitForNodeSocket nodeApi = do
    let policy = capDelay (seconds 60) (fibonacciBackoff (seconds 1))
    retrying policy shouldRepeat \_ -> liftIO do NodeCli.checkSocket nodeApi
  where
    shouldRepeat :: RetryStatus -> Bool -> m Bool
    shouldRepeat _retryStatus = pure . not

waitUntilNodeIsSynced :: forall m. MonadIO m => NodeApi -> m Bool
waitUntilNodeIsSynced nodeApi =
    either (const False) isSynced <$> retrying
        (capDelay (hours 1) (fibonacciBackoff (seconds 1)))
        shouldRepeat
        \_retryStatus -> liftIO do NodeCli.queryTip nodeApi
  where
    isSynced :: NodeTip -> Bool
    isSynced = (>= 99.99) . NodeCli.syncProgress

    shouldRepeat :: RetryStatus -> Either CliError NodeTip -> m Bool
    shouldRepeat _retryStatus = \case
        Left (NodeCli.CliErrorExitCode _code out) ->
            True <$ putStrLn ("CLI Error: " <> decodeUtf8 out)
        Left (NodeCli.CliErrorDecode (_jsonPath, e) _out) -> do
            True <$ putStrLn ("Failed to decode cardano-cli response: " <> e)
        Right tip ->
            not (isSynced tip) <$ putStrLn do
                "Node sync progress: " <> show (NodeCli.syncProgress tip) <> "%"

waitUntilWalletIsSynced :: forall m. MonadIO m => WalletApi -> m Bool
waitUntilWalletIsSynced walletApi =
    either (const False) ((== NodeIsSynced) . WalletCli.nodeStatus)
        <$> retrying
            (capDelay (hours 1) (fibonacciBackoff (seconds 1)))
            shouldRepeat
            \_retryStatus -> liftIO do
                WalletCli.queryNetworkInformation walletApi
  where
    shouldRepeat
        :: RetryStatus
        -> Either WalletCli.Error NetworkInformation
        -> m Bool
    shouldRepeat _retryStatus = \case
        Left err ->
            True <$ putTextLn ("Waiting for wallet to start: " <> show err)
        Right networkInformation -> do
            let nodeStatus = WalletCli.nodeStatus networkInformation
            (NodeIsSynced /= nodeStatus)
                <$ putStrLn
                    ( "Node status as reported by wallet: "
                        <> NodeStatus.toString nodeStatus
                    )

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

seconds :: Int -> Int
seconds = (* 1_000_000)

minutes :: Int -> Int
minutes = (* 60) . seconds

hours :: Int -> Int
hours = (* 60) . minutes
