module Cardano.Wallet.Spec.Network.Local
    ( configuredNetwork
    ) where

import qualified Cardano.Wallet.Spec.Network.Wait as Wait

import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..)
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet.Serialize
    ( saveFunds
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork (..)
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , allocate
    )
import System.IO
    ( openFile
    )
import System.IO.Extra
    ( newTempFile
    )
import System.Path
    ( absFile
    , relFile
    , (</>)
    )
import System.Process.Typed
    ( proc
    , setStderr
    , setStdout
    , startProcess
    , stopProcess
    , useHandleClose
    )

tempFile :: ResourceT IO FilePath
tempFile = fst . snd <$> allocate newTempFile snd

configuredNetwork
    :: DirOf "state"
    -> DirOf "config"
    -> Maybe FaucetFunds
    -> ResourceT IO ConfiguredNetwork
configuredNetwork (DirOf stateDir) (DirOf clusterConfigsDir) faucetFunds = do
    walletApi <- startCluster

    unlessM (Wait.untilWalletIsConnected walletApi) do
        fail "Wallet is not synced, giving up. Please check wallet logs."

    pure ConfiguredNetwork{configuredNetworkWallet = walletApi, ..}
  where
    startCluster :: ResourceT IO WalletApi = do
        socketPath <- tempFile
        let clusterLog = stateDir </> relFile "cluster.log"
        faucetFundsPath <- tempFile
        liftIO $ saveFunds (FileOf $ absFile faucetFundsPath)
            $ fromMaybe (FaucetFunds [] [] []) faucetFunds
        handle <- liftIO $ openFile (toFilePath clusterLog) AppendMode
        putStrLn $ "Writing cluster logs to " <> toFilePath clusterLog
        let randomPort = 64890
        let start = startProcess
                        $ setStderr (useHandleClose handle)
                        $ setStdout (useHandleClose handle)
                        $ proc
                            "local-cluster"
                            [ "--cluster-configs"
                            , toFilePath clusterConfigsDir
                            , "--socket-path"
                            , socketPath
                            , "--faucet-funds"
                            , faucetFundsPath
                            , "--wallet-present"
                            , "--wallet-port"
                            , show randomPort
                            ]
        void $ allocate start stopProcess
        pure
            WalletApi
                { walletInstanceApiUrl =
                    "http://localhost:" <> show randomPort <> "/v2"
                , walletInstanceApiHost = "localhost"
                , walletInstanceApiPort = randomPort
                }
