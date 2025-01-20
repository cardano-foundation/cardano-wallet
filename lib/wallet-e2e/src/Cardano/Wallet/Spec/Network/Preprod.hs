module Cardano.Wallet.Spec.Network.Preprod
    ( configuredNetwork
    ) where

import qualified Cardano.Node.Cli.Launcher as Node
import qualified Cardano.Wallet.Cli.Launcher as Wallet

import Cardano.Node.Cli.Launcher
    ( NodeProcessConfig (..)
    )
import Cardano.Wallet.Cli.Launcher
    ( WalletProcessConfig (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (DirOf)
    , FileOf (..)
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork (..)
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , allocate
    )
import System.Path
    ( relDir
    , relFile
    , (</>)
    )

configuredNetwork
    :: DirOf "state"
    -> DirOf "config"
    -> ResourceT IO ConfiguredNetwork
configuredNetwork (DirOf stateDir) (DirOf nodeConfigDir) = do
    nodeApi <- startNode

    walletApi <- startWallet nodeApi

    pure ConfiguredNetwork
      { configuredNetworkWallet = walletApi
      }
  where
    startNode = do
        let nodeDir = stateDir </> relDir "node"
        let nodeProcessConfig =
                NodeProcessConfig
                { nodeDir = DirOf nodeDir
                , nodeConfig =
                    FileOf $  nodeConfigDir </> relFile "config.json"
                , nodeTopology =
                    FileOf $ nodeConfigDir </> relFile "topology.json"
                , nodeDatabase =
                    DirOf $ nodeDir </> relDir "db"
                }
        (_nodeReleaseKey, (_nodeInstance, nodeApi)) <-
            allocate (Node.start nodeProcessConfig) (Node.stop . fst)
        pure nodeApi

    startWallet nodeApi = do
      let walletDir = stateDir </> relDir "wallet"
      let walletProcessConfig =
              WalletProcessConfig
              { walletDir = DirOf walletDir
              , walletDatabase = DirOf $ walletDir </> relDir "db"
              , walletNodeApi = nodeApi
              , walletListenHost = Nothing
              , walletListenPort = Nothing
              , walletByronGenesisForTestnet = Just $
                  FileOf $ nodeConfigDir </> relFile "byron-genesis.json"
              }
      (_walletReleaseKey, (_walletInstance, walletApi)) <-
          allocate (Wallet.start walletProcessConfig) (Wallet.stop . fst)
      pure walletApi
