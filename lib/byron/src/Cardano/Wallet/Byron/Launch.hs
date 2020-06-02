{-# LANGUAGE DataKinds #-}
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

module Cardano.Wallet.Byron.Launch
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
import Cardano.Chain.Genesis
    ( GenesisData (..), readGenesisData )
import Cardano.CLI
    ( optionT )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Byron
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Byron.Compatibility
    ( NodeVersionData
    , emptyGenesis
    , fromGenesisData
    , fromProtocolMagicId
    , mainnetBlockchainParameters
    , mainnetVersionData
    , testnetVersionData
    )
import Cardano.Wallet.Byron.Transaction
    ( genesisBlockFromTxOuts )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( getRandomPort )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), NetworkParameters (..), ProtocolMagic (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Applicative
    ( (<|>) )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( forM_, unless, when )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT, withExceptT )
import Data.Aeson
    ( toJSON )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( getPOSIXTime )
import GHC.TypeLits
    ( SomeNat (..), someNatVal )
import Options.Applicative
    ( Parser, flag', help, long, metavar )
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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

data NetworkConfiguration where
    MainnetConfig
        :: (SomeNetworkDiscriminant, NodeVersionData)
        -> NetworkConfiguration

    TestnetConfig
        :: FilePath
        -> NetworkConfiguration

-- | Hand-written as there's no Show instance for 'NodeVersionData'
instance Show NetworkConfiguration where
    show = \case
        MainnetConfig{} ->
            "MainnetConfig"
        TestnetConfig genesisFile ->
            "TestnetConfig " <> show genesisFile

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = optionT $ mempty
    <> long "node-socket"
    <> metavar "FILE"
    <> help "Path to the node's domain socket."

-- | --mainnet | (--testnet=FILE)
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption =
    mainnetFlag <|> (TestnetConfig <$> testnetOption)
  where
    -- --mainnet
    mainnetFlag = flag'
        (MainnetConfig (SomeNetworkDiscriminant $ Proxy @'Mainnet, mainnetVersionData))
        (long "mainnet")

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
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeVersionData, Block)
parseGenesisData = \case
    MainnetConfig (discriminant, vData) -> pure
        ( discriminant
        , mainnetBlockchainParameters
        , vData
        , emptyGenesis (genesisParameters mainnetBlockchainParameters)
        )
    TestnetConfig genesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData genesisFile

        let (discriminant, vData) = genesisData
                & gdProtocolMagicId
                & fromProtocolMagicId
                & someTestnetDiscriminant

        let (np, outs) = fromGenesisData (genesisData, genesisHash)
        pure
            ( discriminant
            , np
            , vData
            , genesisBlockFromTxOuts (genesisParameters np) outs
            )

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

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
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Test directory in source tree
    -> Severity
    -- ^ Logging level for @cardano-node@
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO a
withCardanoNode tr tdir severity action =
    orThrow $ withConfig tdir severity $ \cfg block0 (np, vData) -> do
        nodePort <- getRandomPort
        let args = mkArgs cfg nodePort
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        withBackendProcess (trMessageText tr) cmd $ do
            action (nodeSocketFile cfg) block0 (np, vData)
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
-- located in @./test/data/cardano-node-byron@.
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
    -- ^ Test data directory in source tree
    -> Severity
    -- ^ Logging level for @cardano-node@
    -> (  CardanoNodeConfig
       -> Block
       -> (NetworkParameters, NodeVersionData)
       -> IO a
       )
    -- ^ Callback function with the node configuration and genesis params
    -> IO a
withConfig tdir minSeverity action =
    bracket setupConfig teardownConfig $ \(_a,b,c,d) -> action b c d
  where
    source :: FilePath
    source = tdir </> "cardano-node-byron"

    setupConfig
        :: IO ( FilePath
              , CardanoNodeConfig
              , Block
              , (NetworkParameters, NodeVersionData)
              )
    setupConfig = do
        dir <- getCanonicalTemporaryDirectory
            >>= \tmpRoot -> createTempDirectory tmpRoot "cw-byron"

        let nodeConfigFile   = dir </> "node.config"
        let nodeDatabaseDir  = dir </> "node.db"
        let nodeDlgCertFile  = dir </> "node.cert"
        let nodeGenesisFile  = dir </> "genesis.json"
        let nodeSignKeyFile  = dir </> "node.key"
        let nodeTopologyFile = dir </> "node.topology"
        let nodeSocketFile =
                if os == "mingw32"
                then "\\\\.\\pipe\\" ++ takeFileName dir
                else dir </> "node.socket"

        -- we need to specify genesis file location every run in tmp
        Yaml.decodeFileThrow (source </> "node.config")
            >>= withObject (addGenesisFilePath (T.pack nodeGenesisFile))
            >>= withObject (addMinSeverity (T.pack $ show minSeverity))
            >>= Yaml.encodeFile (dir </> "node.config")

        Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
            >>= withObject updateStartTime
            >>= Aeson.encodeFile nodeGenesisFile
        forM_ ["node.topology", "node.key", "node.cert"] $ \f ->
            copyFile (source </> f) (dir </> f)

        (genesisData, genesisHash) <- unsafeRunExceptT $
            readGenesisData nodeGenesisFile
        let (np, outs) = fromGenesisData (genesisData, genesisHash)
        let networkMagic =
                NetworkMagic $ unProtocolMagicId $ gdProtocolMagicId genesisData

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
            , genesisBlockFromTxOuts (genesisParameters np) outs
            , ( np
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
