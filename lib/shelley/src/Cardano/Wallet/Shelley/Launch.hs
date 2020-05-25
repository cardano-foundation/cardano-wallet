{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch cardano-node for /testing/.

module Cardano.Wallet.Shelley.Launch
    ( -- * Integration Launcher
      withCardanoNode

    , NetworkConfiguration (..)
    , nodeSocketOption
    , networkConfigurationOption
    , parseGenesisData
    ) where

import Prelude


import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace )
import Cardano.CLI
    ( optionT )
import Cardano.Config.Shelley.Genesis
    ( ShelleyGenesis )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), GenesisBlockParameters (..), ProtocolMagic (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, fromGenesisData, testnetVersionData )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( forM_, unless, when )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Aeson
    ( eitherDecode, toJSON )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( getCurrentTime )
import GHC.TypeLits
    ( SomeNat (..), someNatVal )
import Options.Applicative
    ( Parser, help, long, metavar )
import Ouroboros.Consensus.Shelley.Node
    ( sgNetworkMagic )
import Ouroboros.Consensus.Shelley.Protocol
    ( TPraosStandardCrypto )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import System.Directory
    ( copyFile, doesDirectoryExist, removeDirectoryRecursive )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( takeFileName, (</>) )
import System.Info
    ( os )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

data NetworkConfiguration where
    TestnetConfig
        :: FilePath
        -> NetworkConfiguration

-- | Hand-written as there's no Show instance for 'NodeVersionData'
instance Show NetworkConfiguration where
    show = \case
        TestnetConfig genesisFile ->
            "TestnetConfig " <> show genesisFile

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = optionT $ mempty
    <> long "node-socket"
    <> metavar "FILE"
    <> help "Path to the node's domain socket."

-- | --testnet=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption =
    TestnetConfig <$> testnetOption
  where
    -- | --testnet=FILE
    testnetOption
        :: Parser FilePath
    testnetOption = optionT $ mempty
        <> long "testnet"
        <> metavar "FILE"
        <> help "Path to the genesis .json file."

someTestnetDiscriminant
    :: ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeVersionData)
someTestnetDiscriminant pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( SomeNetworkDiscriminant $ mapProxy proxy
            , testnetVersionData pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."
  where
    mapProxy :: forall a. Proxy a -> Proxy ('Testnet a)
    mapProxy _proxy = Proxy @('Testnet a)

parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO (SomeNetworkDiscriminant, GenesisBlockParameters, NodeVersionData, Block)
parseGenesisData = \case
    TestnetConfig genesisFile -> do
        (genesis :: ShelleyGenesis TPraosStandardCrypto)
            <- ExceptT $ eitherDecode <$> BL.readFile genesisFile
        let nm = unNetworkMagic $ sgNetworkMagic genesis
        let (discriminant, vData) = someTestnetDiscriminant $ ProtocolMagic $ fromIntegral nm

        let (gbp, block0) = fromGenesisData genesis
        pure
            ( discriminant
            , gbp
            , vData
            , block0
            )

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

data CardanoNodeConfig = CardanoNodeConfig
    { nodeConfigFile   :: FilePath
    , nodeDatabaseDir  :: FilePath
    , nodeSocketFile   :: FilePath
    , nodeTopologyFile :: FilePath
    , nodeVrfKey       :: FilePath
    , nodeKesKey       :: FilePath
    , nodeOpCert       :: FilePath
    }

-- | Spins up a @cardano-node@ in another process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoNode
    :: Trace IO Text
    -- ^ Some trace for logging
    -> FilePath
    -- ^ Test directory
    -> Severity
    -> (FilePath -> Block -> (GenesisBlockParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with a socket description and genesis params
    -> IO a
withCardanoNode tr tdir severity action =
    orThrow $ withConfig tdir severity $ \cfg block0 (gbp, vData) -> do
        nodePort <- getRandomPort
        let args = mkArgs cfg nodePort
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        withBackendProcess (trMessageText tr) cmd $ do
            action (nodeSocketFile cfg) block0 (gbp, vData)
  where
    orThrow = (=<<) (either throwIO pure)
    mkArgs cfg port =
        [ "run"
        , "--config", nodeConfigFile cfg
        , "--topology", nodeTopologyFile cfg
        , "--database-path", nodeDatabaseDir cfg
        , "--socket-path", nodeSocketFile cfg
        , "--port", show port
        , "--shelley-vrf-key", nodeVrfKey cfg
        , "--shelley-kes-key", nodeKesKey cfg
        , "--shelley-operational-certificate", nodeOpCert cfg
        ]


-- | Generate a new integration configuration based on a partial configuration
-- located in @./test/data/cardano-node-shelley@.
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
    -> Severity
    -- ^ Test data directory
    -> (  CardanoNodeConfig
       -> Block
       -> (GenesisBlockParameters, NodeVersionData)
       -> IO a
       )
    -- ^ Callback function with the node configuration and genesis params
    -> IO a
withConfig tdir severity action =
    bracket setupConfig teardownConfig $ \(_a,b,c,d) -> action b c d
  where
    source :: FilePath
    source = tdir </> "cardano-node-shelley"

    setupConfig
        :: IO ( FilePath
              , CardanoNodeConfig
              , Block
              , (GenesisBlockParameters, NodeVersionData)
              )
    setupConfig = do
        dir <- getCanonicalTemporaryDirectory
            >>= \tmpRoot -> createTempDirectory tmpRoot "cw-byron"

        let nodeConfigFile   = dir </> "node.config"
        let nodeDatabaseDir  = dir </> "node.db"
        let nodeGenesisFile  = dir </> "genesis.json"
        let nodeTopologyFile = dir </> "node.topology"
        let nodeVrfKey = dir </> "node-vrf.skey"
        let nodeKesKey = dir </> "node-kes.skey"
        let nodeOpCert = dir </> "node.opcert"
        let nodeSocketFile =
                if os == "mingw32"
                then "\\\\.\\pipe\\" ++ takeFileName dir
                else dir </> "node.socket"

        -- we need to specify genesis file location every run in tmp
        Yaml.decodeFileThrow (source </> "node.config")
            >>= withObject (addGenesisFilePath (T.pack nodeGenesisFile))
            >>= withObject (addMinSeverity (T.pack $ show severity))
            >>= Yaml.encodeFile (dir </> "node.config")

        Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
            >>= withObject updateStartTime
            >>= Aeson.encodeFile nodeGenesisFile
        forM_ ["node.topology", "node-vrf.skey", "node-kes.skey", "node.opcert"] $ \f ->
            copyFile (source </> f) (dir </> f)

        (genesisData :: ShelleyGenesis TPraosStandardCrypto)
            <- either (error . show) id . eitherDecode <$> BL.readFile nodeGenesisFile

        let (gbp, block0) = fromGenesisData genesisData

        let nm = sgNetworkMagic genesisData

        pure
            ( dir
            , CardanoNodeConfig
                { nodeConfigFile
                , nodeDatabaseDir
                , nodeSocketFile
                , nodeTopologyFile
                , nodeVrfKey
                , nodeKesKey
                , nodeOpCert
                }
            , block0
            , ( gbp
              , ( NodeToClientVersionData nm
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
    time <- getCurrentTime
    pure $ HM.insert "startTime" (toJSON time) m

-- | Add a "GenesisFile" field in a given object with the current path of
-- genesis.json in tmp dir as value.
addGenesisFilePath
    :: Text
    -> Aeson.Object
    -> IO Aeson.Object
addGenesisFilePath path = pure . HM.insert "GenesisFile" (toJSON path)

-- | Add a "minSeverity" field in a given object with the current path of
-- genesis.json in tmp dir as value.
addMinSeverity
    :: Text
    -> Aeson.Object
    -> IO Aeson.Object
addMinSeverity severity = pure . HM.insert "minSeverity" (toJSON severity)

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
