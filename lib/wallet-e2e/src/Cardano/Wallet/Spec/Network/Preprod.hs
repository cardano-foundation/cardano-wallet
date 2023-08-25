{-# LANGUAGE QuasiQuotes #-}

module Cardano.Wallet.Spec.Network.Preprod
    ( nodeWalletSetup
    ) where

import qualified Cardano.Wallet.Spec.Network.Node as Node
import qualified Cardano.Wallet.Spec.Network.Node.Cli as Cli
import qualified Cardano.Wallet.Spec.Network.Wallet as Wallet

import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )
import Cardano.Wallet.Spec.Network.Node
    ( NodeProcessConfig (..), nodeApiSocket )
import Control.Monad.Trans.Resource
    ( ResIO, allocate, runResourceT )
import Control.Retry
    ( capDelay, fibonacciBackoff, retrying )
import Path
    ( Abs, Dir, Path, reldir, relfile, (</>) )
import Path.IO
    ( makeAbsolute )

nodeWalletSetup :: Path Abs Dir -> (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup stateDir withNetworkConfig = runResourceT do
    nodeProcessConfig <- makeNodeProcessConfig stateDir
    (_nodeReleaseKey, (_nodeInstance, nodeApi)) <-
        allocate (Node.start nodeProcessConfig) (Node.stop . fst)
    (_walletReleaseKey, (_walletInstance, networkConfigWallet)) <-
        allocate Wallet.start (Wallet.stop . fst)

    let expectedProgress :: Double = 99.0
        policy = capDelay 10_000_000 (fibonacciBackoff 500_000)
    _ <- retrying policy (\_rs -> pure . (< expectedProgress)) \_ -> liftIO do
        Cli.queryTip (nodeApiSocket nodeApi) >>= \case
            Left err ->
                0.0 <$ putTextLn ("Waiting for node to start: " <> show err)
            Right tip -> do
                let progress = Cli.syncProgress tip
                progress <$ putTextLn do
                    "Node sync progress: " <> show progress <> "%"

    liftIO $ withNetworkConfig NetworkConfig{..}

makeNodeProcessConfig :: Path Abs Dir -> ResIO NodeProcessConfig
makeNodeProcessConfig temp = do
    let nodeDir = temp </> [reldir|node|]
    preprodDir <- makeAbsolute [reldir|config/cardano-node/preprod|]
    pure
        NodeProcessConfig
            { nodeDir
            , nodeConfig = preprodDir </> [relfile|config.json|]
            , nodeTopology = preprodDir </> [relfile|topology.json|]
            , nodeDatabase = nodeDir </> [reldir|db|]
            }

{-
temporaryDirectory :: ResIO (Path Abs Dir)
temporaryDirectory = do
    temp <- getTempDir
    snd <$> allocate (createTempDir temp "cardano-wallet-e2e") removeDirRecur
-}
