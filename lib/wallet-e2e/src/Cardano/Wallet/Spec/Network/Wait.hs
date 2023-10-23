module Cardano.Wallet.Spec.Network.Wait where

import qualified Cardano.Wallet.Spec.Data.Network.NodeStatus as NodeStatus
import qualified Cardano.Wallet.Spec.Network.Node.Cli as NodeCli
import qualified Cardano.Wallet.Spec.Network.Wallet.Cli as WalletCli

import Cardano.Node.Cli.Launcher
    ( NodeApi
    )
import Cardano.Wallet.Cli.Launcher
    ( WalletApi
    )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..)
    )
import Cardano.Wallet.Spec.Network.Node.Cli
    ( CliError
    , NodeTip
    )
import Cardano.Wallet.Spec.Network.Wallet.Cli
    ( NetworkInformation
    )
import Control.Retry
    ( RetryStatus
    , capDelay
    , fibonacciBackoff
    , retrying
    )

forNodeSocket :: forall m. MonadIO m => NodeApi -> m Bool
forNodeSocket nodeApi = do
    let policy = capDelay (seconds 60) (fibonacciBackoff (seconds 1))
    retrying policy shouldRepeat \_ -> liftIO do NodeCli.checkSocket nodeApi
  where
    shouldRepeat :: RetryStatus -> Bool -> m Bool
    shouldRepeat _retryStatus = pure . not

untilNodeIsSynced :: forall m. MonadIO m => NodeApi -> m Bool
untilNodeIsSynced nodeApi =
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

untilWalletIsConnected :: forall m. MonadIO m => WalletApi -> m Bool
untilWalletIsConnected walletApi =
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
