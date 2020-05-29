{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch cardano-node for /testing/.

module Cardano.Wallet.Byron.Launch
    ( -- * Integration Launcher
      withCardanoSelfNode

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
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn (..)
    , defaultCardanoNodeConfig
    , withCardanoNode
    )
import Cardano.Wallet.Byron
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Byron.Compatibility
    ( NodeVersionData
    , emptyGenesis
    , fromGenesisData
    , fromProtocolMagicId
    , mainnetNetworkParameters
    , mainnetVersionData
    , testnetVersionData
    )
import Cardano.Wallet.Byron.Transaction
    ( genesisBlockFromTxOuts )
import Cardano.Wallet.Logging
    ( trMessageText )
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
    ( KnownNat, Nat, SomeNat (..), someNatVal )
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
    ( (</>) )
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

    StagingConfig
        :: FilePath
        -> NetworkConfiguration

-- | Hand-written as there's no Show instance for 'NodeVersionData'
instance Show NetworkConfiguration where
    show = \case
        MainnetConfig{} ->
            "MainnetConfig"
        TestnetConfig genesisFile ->
            "TestnetConfig " <> show genesisFile
        StagingConfig genesisFile ->
            "StagingConfig " <> show genesisFile

-- | --node-socket=FILE
nodeSocketOption :: Parser FilePath
nodeSocketOption = optionT $ mempty
    <> long "node-socket"
    <> metavar "FILE"
    <> help "Path to the node's domain socket."

-- | --mainnet | (--testnet=FILE)
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption =
    mainnetFlag
    <|>
    (TestnetConfig <$> customNetworkOption "testnet")
    <|>
    (StagingConfig <$> customNetworkOption "staging")
  where
    -- --mainnet
    mainnetFlag = flag'
        (MainnetConfig (SomeNetworkDiscriminant $ Proxy @'Mainnet, mainnetVersionData))
        (long "mainnet")

    customNetworkOption
        :: String
        -> Parser FilePath
    customNetworkOption network = optionT $ mempty
        <> long network
        <> metavar "FILE"
        <> help "Path to the genesis .json file."

someCustomDiscriminant
    :: (forall (pm :: Nat). KnownNat pm => Proxy pm -> SomeNetworkDiscriminant)
    -> ProtocolMagic
    -> (SomeNetworkDiscriminant, NodeVersionData)
someCustomDiscriminant mkSomeNetwork pm@(ProtocolMagic n) =
    case someNatVal (fromIntegral n) of
        Just (SomeNat proxy) ->
            ( mkSomeNetwork proxy
            , testnetVersionData pm
            )
        _ -> error "networkDiscriminantFlag: failed to convert \
            \ProtocolMagic to SomeNat."

parseGenesisData
    :: NetworkConfiguration
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeVersionData, Block)
parseGenesisData = \case
    MainnetConfig (discriminant, vData) -> pure
        ( discriminant
        , mainnetNetworkParameters
        , vData
        , emptyGenesis (genesisParameters mainnetNetworkParameters)
        )
    TestnetConfig genesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData genesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Testnet pm)

        let (discriminant, vData) = genesisData
                & gdProtocolMagicId
                & fromProtocolMagicId
                & someCustomDiscriminant mkSomeNetwork

        let (np, outs) = fromGenesisData (genesisData, genesisHash)
        pure
            ( discriminant
            , np
            , vData
            , genesisBlockFromTxOuts (genesisParameters np) outs
            )
    StagingConfig genesisFile -> do
        (genesisData, genesisHash) <-
            withExceptT show $ readGenesisData genesisFile

        let mkSomeNetwork
                :: forall (pm :: Nat). KnownNat pm
                => Proxy pm
                -> SomeNetworkDiscriminant
            mkSomeNetwork _ = SomeNetworkDiscriminant $ Proxy @('Staging pm)

        let (discriminant, vData) = genesisData
                & gdProtocolMagicId
                & fromProtocolMagicId
                & someCustomDiscriminant mkSomeNetwork

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

-- | Spins up a @cardano-node@ in another process.
--
-- IMPORTANT: @cardano-node@ must be available on the current path.
withCardanoSelfNode
    :: Trace IO Text
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Test directory in source tree
    -> Severity
    -- ^ Logging level for @cardano-node@
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO a
withCardanoSelfNode tr tdir severity action =
    orThrow $ withConfig tdir severity $ \cfg block0 (np, vData) ->
        withCardanoNode (trMessageText tr) cfg $ \cp ->
            action (nodeSocketFile cp) block0 (np, vData)
  where
    orThrow = (=<<) (either throwIO pure)


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
        let nodeDlgCertFile  = Just (dir </> "node.cert")
        let nodeSignKeyFile  = Just (dir </> "node.key")
        let nodeGenesisFile  = dir </> "genesis.json"
        let nodeTopologyFile = dir </> "node.topology"

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
            , defaultCardanoNodeConfig
                { nodeConfigFile
                , nodeDatabaseDir
                , nodeDlgCertFile
                , nodeSignKeyFile
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
