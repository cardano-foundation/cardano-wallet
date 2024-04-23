module Cardano.Wallet.Spec.Network.Local
    ( configuredNetwork
    ) where

import qualified Cardano.Wallet.Spec.Network.Wait as Wait

import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
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
import System.Path
    ( relFile
    , (</>)
    )
import System.Process.Typed
    ( Process
    , proc
    , setStderr
    , setStdout
    , startProcess
    , stopProcess
    , useHandleClose
    )

configuredNetwork
    :: DirOf "state"
    -> DirOf "config"
    -> ResourceT IO ConfiguredNetwork
configuredNetwork (DirOf stateDir) (DirOf clusterConfigsDir) = do
    walletApi <- startCluster

    unlessM (Wait.untilWalletIsConnected walletApi) do
        fail "Wallet is not synced, giving up. Please check wallet logs."

    pure ConfiguredNetwork{configuredNetworkWallet = walletApi, ..}
  where
    startCluster :: ResourceT IO WalletApi = do
      (_clusterReleaseKey, _clusterProcess) <-
          allocate startLocalClusterProcess stopProcess
      pure WalletApi
        { walletInstanceApiUrl = "http://localhost:8090/v2"
        , walletInstanceApiHost = "localhost"
        , walletInstanceApiPort = 8090
        }

    startLocalClusterProcess :: IO (Process () () ())
    startLocalClusterProcess = do
        let clusterLog = stateDir </> relFile "cluster.log"
        handle <- openFile (toFilePath clusterLog) AppendMode
        putStrLn $ "Writing cluster logs to " <> toFilePath clusterLog
        startProcess
          $ setStderr (useHandleClose handle)
          $ setStdout (useHandleClose handle)
          $ proc "local-cluster"
            [ "--cluster-configs"
            , toFilePath clusterConfigsDir
            ]
