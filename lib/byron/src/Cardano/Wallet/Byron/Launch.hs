{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch cardano-node for /testing/.

module Cardano.Wallet.Byron.Launch
    ( -- * Integration Launcher
      withCardanoNode
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Byron.Compatibility
    ( NodeVersionData, fromGenesisData )
import Cardano.Wallet.Byron.Transaction
    ( fromGenesisTxOut )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockchainParameters (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( forM_, unless, when )
import Control.Monad.Fail
    ( MonadFail )
import Data.Aeson
    ( toJSON )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( getPOSIXTime )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import System.Directory
    ( copyFile, doesDirectoryExist, removeDirectoryRecursive )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

data CardanoNodeConfig = CardanoNodeConfig
    { nodeConfigFile   :: FilePath
    , nodeDatabaseDir  :: FilePath
    , nodeDlgCertFile  :: FilePath
    , nodeSignKeyFile  :: FilePath
    , nodeSocketFile   :: FilePath
    , nodeTopologyFile :: FilePath
    }

-- | Spins up a @cardano-node@ in another process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Trace IO Text
    -- ^ Some trace for logging
    -> FilePath
    -- ^ Test directory
    -> (FilePath -> Block -> (BlockchainParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with a socket description and genesis params
    -> IO a
withCardanoNode tr tdir action =
    orThrow $ withConfig tdir $ \cfg block0 (bp, vData) -> do
        nodePort <- getRandomPort
        let args = mkArgs cfg nodePort
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        withBackendProcess (trMessageText tr) cmd $ do
            action (nodeSocketFile cfg) block0 (bp, vData)
  where
    orThrow = (=<<) (either throwIO pure)
    mkArgs cfg port =
        [ "run"
        , "--config", nodeConfigFile cfg
        , "--signing-key", nodeSignKeyFile cfg
        , "--delegation-certificate", nodeDlgCertFile cfg
        , "--topology", nodeTopologyFile cfg
        , "--database-path", nodeDatabaseDir cfg
        , "--socket-path", nodeSocketFile cfg
        , "--port", show port
        ]


-- | Generate a new integration configuration based on a partial configuration
-- located in @./test/data/cardano-node@.
--
-- The 'startTime' from the partial genesis file will be overriden with a new
-- fresh recent one (resulting in a different genesis hash).
--
-- As a result, this function creates a temporary directory which is cleaned up
-- after use (unless the ENV var NO_CLEANUP is set):
--
--     $ tree /tmp/cardano-wallet-byron-2cbb1ea94edb1cea/
--       ├── genesis.json
--       ├── node.cert
--       ├── node.config
--       ├── node.key
--       └── node.topology
--
withConfig
    :: FilePath
    -- ^ Test data directory
    -> (  CardanoNodeConfig
       -> Block
       -> (BlockchainParameters, NodeVersionData)
       -> IO a
       )
    -- ^ Callback function with the node configuration and genesis params
    -> IO a
withConfig tdir action =
    bracket setupConfig teardownConfig $ \(_a,b,c,d) -> action b c d
  where
    source :: FilePath
    source = tdir </> "cardano-node"

    setupConfig
        :: IO ( FilePath
              , CardanoNodeConfig
              , Block
              , (BlockchainParameters, NodeVersionData)
              )
    setupConfig = do
        dir <- getCanonicalTemporaryDirectory
            >>= \tmpRoot -> createTempDirectory tmpRoot "cw-byron"

        let nodeConfigFile   = dir </> "node.config"
        let nodeDatabaseDir  = dir </> "node.db"
        let nodeDlgCertFile  = dir </> "node.cert"
        let nodeGenesisFile  = dir </> "genesis.json"
        let nodeSignKeyFile  = dir </> "node.key"
        let nodeSocketFile   = dir </> "node.socket"
        let nodeTopologyFile = dir </> "node.topology"

        -- we need to specify genesis file location every run in tmp
        Yaml.decodeFileThrow (source </> "node.config")
            >>= withObject (addGenesisFilePath (T.pack nodeGenesisFile))
            >>= Yaml.encodeFile (dir </> "node.config")

        Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
            >>= withObject updateStartTime
            >>= Aeson.encodeFile nodeGenesisFile
        forM_ ["node.topology", "node.key", "node.cert"] $ \f ->
            copyFile (source </> f) (dir </> f)

        (genesisData, genesisHash) <- unsafeRunExceptT $
            readGenesisData nodeGenesisFile
        let (bp, outs) = fromGenesisData (genesisData, genesisHash)

        let networkMagic = NetworkMagic $ unProtocolMagicId $ gdProtocolMagicId genesisData

        pure
            ( dir
            , CardanoNodeConfig
                { nodeConfigFile
                , nodeDatabaseDir
                , nodeDlgCertFile
                , nodeSignKeyFile
                , nodeSocketFile
                , nodeTopologyFile
                }
            , fromGenesisTxOut bp outs
            , ( bp
              , ( NodeToClientVersionData { networkMagic }
                , nodeToClientCodecCBORTerm
                )
              )
            )

    teardownConfig
        :: (FilePath, b, c, d)
        -> IO ()
    teardownConfig (dir, _, _, _) = do
        noCleanup <- maybe False (not . null) <$> lookupEnv "NO_CLEANUP"
        exists <- doesDirectoryExist dir
        unless noCleanup $ when exists $ removeDirectoryRecursive dir

-- | Add a "startTime" field in a given object with the current POSIX time as a
-- value.
updateStartTime
    :: Aeson.Object
    -> IO Aeson.Object
updateStartTime m = do
    time <- round @_ @Int <$> getPOSIXTime
    pure $ HM.insert "startTime" (toJSON time) m

-- | Add a "GenesisFile" field in a given object with the current path of
-- genesis.json in tmp dir as value.
addGenesisFilePath
    :: Text
    -> Aeson.Object
    -> IO Aeson.Object
addGenesisFilePath path = pure . HM.insert "GenesisFile" (toJSON path)

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ -> fail
        "withObject: was given an invalid JSON. Expected an Object but got \
        \something else."
