{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functions to launch cardano-nodes in a cluster for /testing/.

module Cardano.Wallet.Launch.Cluster
    ( -- * Local test cluster launcher
      withCluster
    , LocalClusterConfig (..)
    , localClusterConfigFromEnv
    , ClusterEra (..)
    , FaucetFunds (..)

      -- * Node launcher
    , NodeParams (..)
    , singleNodeParams
    , RunningNode (..)

      -- * Cluster node launcher
    , defaultPoolConfigs
    , clusterEraFromEnv
    , clusterToApiEra
    , clusterEraToString
    , withSMASH

      -- * Configuration
    , LogFileConfig (..)
    , logFileConfigFromEnv
    , minSeverityFromEnv
    , nodeMinSeverityFromEnv
    , walletMinSeverityFromEnv
    , testMinSeverityFromEnv
    , testLogDirFromEnv
    , walletListenFromEnv
    , tokenMetadataServerFromEnv

      -- * Faucets
    , Credential (..)
    , sendFaucetFundsTo
    , sendFaucetAssetsTo
    , moveInstantaneousRewardsTo
    , oneMillionAda
    , genMonetaryPolicyScript

    -- * Logging
    , ClusterLog (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api
    ( AsType (AsStakeKey, AsStakePoolKey)
    , Key (verificationKeyHash)
    , serialiseToCBOR
    )
import Cardano.Api.Shelley
    ( AsType (AsVrfKey) )
import Cardano.Binary
    ( fromCBOR )
import Cardano.BM.Data.Output
    ( ScribeDefinition (..)
    , ScribeFormat (..)
    , ScribeKind (..)
    , ScribePrivacy (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.CLI
    ( parseLoggingSeverity )
import Cardano.CLI.Byron.Commands
    ( VerificationKeyFile (VerificationKeyFile) )
import Cardano.CLI.Shelley.Key
    ( VerificationKeyOrFile (..), readVerificationKeyOrFile )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn
    , NodePort (..)
    , nodeSocketFile
    , withCardanoNode
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    , NonNegativeInterval
    , PositiveUnitInterval
    , StrictMaybe (..)
    , UnitInterval
    , boundRational
    , textToUrl
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Era (Crypto) )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..), ShelleyGenesisStaking (sgsPools) )
import Cardano.Pool.Metadata
    ( SMASHPoolId (..) )
import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Startup
    ( restrictFileMode )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , HealthStatusSMASH (..)
    )
import Cardano.Wallet.Launch
    ( TempDirLog (..), envFromText, lookupEnvNonEmpty )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , EpochNo (..)
    , NetworkParameters (..)
    , PoolCertificate
    , TokenMetadataServer (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( StandardShelley, fromGenesisData )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode, unsafeFromHex )
import Cardano.Wallet.Util
    ( mapFirst )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_, liftM2, replicateM, replicateM_, void, when, (>=>) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( object, toJSON, (.:), (.=) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bits
    ( (.|.) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Char
    ( toLower )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Product.Fields
    ( setField )
import Data.IntCast
    ( intCast )
import Data.List
    ( intercalate, nub, permutations, sort )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Directory
    ( copyFile, createDirectory, createDirectoryIfMissing, makeAbsolute )
import System.Environment
    ( getEnvironment )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( (<.>), (</>) )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process.Typed
    ( ProcessConfig, proc, readProcess, setEnv, setEnvInherit )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import UnliftIO.Async
    ( async, link, wait )
import UnliftIO.Chan
    ( newChan, readChan, writeChan )
import UnliftIO.Exception
    ( SomeException, finally, handle, throwIO, throwString )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, swapMVar )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
<<<<<<< HEAD
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
=======
import qualified Data.ListMap as ListMap
>>>>>>> c74a4ee662 (Update dependencies)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Yaml as Yaml

-- | Returns the shelley test data path, which is usually relative to the
-- package sources, but can be overridden by the @SHELLEY_TEST_DATA@ environment
-- variable.
getShelleyTestDataPath :: IO FilePath
getShelleyTestDataPath = fromMaybe source <$> lookupEnvNonEmpty var
  where
    source = $(getTestData) </> "cardano-node-shelley"
    var = "SHELLEY_TEST_DATA"

logFileConfigFromEnv
    :: Maybe String
    -- ^ Optional extra subdir for TESTS_LOGDIR. E.g. @Just "alonzo"@ and
    -- @Just "mary"@ to keep them separate.
    -> IO LogFileConfig
logFileConfigFromEnv subdir = LogFileConfig
    <$> nodeMinSeverityFromEnv
    <*> (testLogDirFromEnv subdir)
    <*> pure Info

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var = lookupEnvNonEmpty var >>= \case
    Nothing -> pure def
    Just arg -> either die pure (parseLoggingSeverity arg)

-- Allow configuring @cardano-node@ log level with the
-- @CARDANO_NODE_TRACING_MIN_SEVERITY@ environment variable.
nodeMinSeverityFromEnv :: IO Severity
nodeMinSeverityFromEnv =
    minSeverityFromEnv Info "CARDANO_NODE_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @CARDANO_WALLET_TRACING_MIN_SEVERITY@ environment variable.
walletMinSeverityFromEnv :: IO Severity
walletMinSeverityFromEnv =
    minSeverityFromEnv Warning "CARDANO_WALLET_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @TESTS_TRACING_MIN_SEVERITY@ environment variable.
testMinSeverityFromEnv :: IO Severity
testMinSeverityFromEnv =
    minSeverityFromEnv Notice "TESTS_TRACING_MIN_SEVERITY"

-- | Allow configuring which port the wallet server listen to in an integration
-- setup. Crashes if the variable is not a number.
walletListenFromEnv :: IO Listen
walletListenFromEnv = envFromText "CARDANO_WALLET_PORT" >>= \case
    Nothing -> pure ListenOnRandomPort
    Just (Right port) -> pure $ ListenOnPort port
    Just (Left e) -> die $ show e

tokenMetadataServerFromEnv :: IO (Maybe TokenMetadataServer)
tokenMetadataServerFromEnv = envFromText "TOKEN_METADATA_SERVER" >>= \case
    Nothing -> pure Nothing
    Just (Right s) -> pure (Just s)
    Just (Left e) -> die $ show e

-- | Directory for extra logging. Buildkite will set this environment variable
-- and upload logs in it automatically.
testLogDirFromEnv :: Maybe String -> IO (Maybe FilePath)
testLogDirFromEnv msubdir = do
    rel <- lookupEnvNonEmpty "TESTS_LOGDIR"
    makeAbsolute `traverse` case msubdir of
        Just subdir -> liftM2 (</>) rel (Just subdir)
        Nothing -> rel

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | Make a 'ProcessConfig' for running @cardano-cli@. The program must be on
-- the @PATH@, as normal. Sets @CARDANO_NODE_SOCKET_PATH@ for the subprocess, if
-- a 'CardanoNodeConn' is provided.
cliConfigBase
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> Maybe CardanoNodeConn -- ^ optional cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigBase tr conn args = do
    traceWith tr (MsgCLI args)
    env <- getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c):env
    let cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure $ cliEnv $ proc "cardano-cli" args

cliConfigNode
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> CardanoNodeConn -- ^ cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigNode tr conn = cliConfigBase tr (Just conn)

cliConfig
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfig tr = cliConfigBase tr Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO ()
cli tr = cliConfig tr >=> void . readProcessStdoutOrFail

cliLine :: Tracer IO ClusterLog -> [String] -> IO String
cliLine tr = cliConfig tr >=>
    fmap (BL8.unpack . getFirstLine) . readProcessStdoutOrFail

cliEraFlag :: ClusterEra -> String
cliEraFlag era = "--" ++ clusterEraToString era ++ "-era"

readProcessStdoutOrFail :: ProcessConfig () () () -> IO BL.ByteString
readProcessStdoutOrFail processConfig = do
    (st, out, err) <- readProcess processConfig
    case st of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO $ userError $ mconcat
            [ "command failed: "
            , BL8.unpack err
            ]

getFirstLine :: BL8.ByteString -> BL8.ByteString
getFirstLine = BL8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@.
cliRetry
    :: Tracer IO ClusterLog
    -> Text -- ^ message to print before running command
    -> ProcessConfig () a b
    -> IO ()
cliRetry tr msg processConfig = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwIO $ ProcessHasExited
            ("cardano-cli failed: " <> BL8.unpack err) st
  where
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- | The idea of what kind if pool we want to set up.
data PoolRecipe = PoolRecipe
    { pledgeAmt :: Integer
    , index :: Int
    , retirementEpoch :: Maybe EpochNo
      -- ^ An optional retirement epoch. If specified, then a pool retirement
      -- certificate will be published after the pool is initially registered.
    , poolMetadata :: Aeson.Value
    , operatorKeys :: (PoolId, Aeson.Value, Aeson.Value, Aeson.Value)
      -- ^ @(poolId, vk, sk, counter)@ - as long as the integration tests make
      -- use of hard-coded pool ids, we need to pre-assign the operator keys and
      -- related data already here.
    , delisted :: Bool
      -- ^ Tells @withSMASH@ whether to delist this pool or not. Aside from
      -- this, a delisted pool will operate as normal.
    }
    deriving (Eq, Show)

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a. NodeParams -> (RunningNode -> IO a) -> IO a
      -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
      -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: IO (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
    , finalizeShelleyGenesisSetup :: RunningNode -> IO ()
      -- ^ Submit any pool retirement certificate according to the 'recipe'
      -- on-chain.
    , registerViaTx :: RunningNode -> IO ()
    }

data PoolMetadataServer = PoolMetadataServer
    { registerMetadataForPoolIndex :: Int -> Aeson.Value -> IO ()
    , urlFromPoolIndex  :: Int -> String
    }

withPoolMetadataServer
    :: Tracer IO ClusterLog
    -> FilePath
    -> (PoolMetadataServer -> IO a)
    -> IO a
withPoolMetadataServer tr dir action = do
    let metadir = dir </> "pool-metadata"
    createDirectoryIfMissing False metadir
    withStaticServer metadir $ \baseURL -> do
        let _urlFromPoolIndex i = baseURL </> metadataFileName i
        action $ PoolMetadataServer
            { registerMetadataForPoolIndex = \i metadata -> do
                let metadataBytes = Aeson.encode metadata
                BL8.writeFile (metadir </> (metadataFileName i)) metadataBytes
                let hash = blake2b256 (BL.toStrict metadataBytes)
                traceWith tr $
                    MsgRegisteringPoolMetadata
                        (_urlFromPoolIndex i)
                        (B8.unpack $ hex hash)
            , urlFromPoolIndex = _urlFromPoolIndex
            }
  where

    metadataFileName :: Int -> FilePath
    metadataFileName i = show i <> ".json"

configurePools
    :: Tracer IO ClusterLog
    -> FilePath
    -> ClusterEra
    -> PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> IO (NonEmpty ConfiguredPool)
configurePools tr dir era metadataServer =
    mapM (configurePool tr dir era metadataServer)

configurePool
    :: Tracer IO ClusterLog
    -> FilePath
    -> ClusterEra
    -> PoolMetadataServer
    -> PoolRecipe
    -> IO ConfiguredPool
configurePool tr baseDir era metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    -- Use pool-specific dir
    let name = "pool-" <> show i
    let dir = baseDir </> name
    createDirectoryIfMissing False dir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair tr dir
    (kesPrv, kesPub) <- genKesKeyPair tr dir
    (opPrv, opPub, opCount) <- writeOperatorKeyPair tr dir recipe
    opCert <- issueOpCert tr dir kesPub opPrv opCount
    let ownerPub = dir </> "stake.pub"
    let ownerPrv = dir </> "stake.prv"
    genStakeAddrKeyPair tr (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata

    pure $ ConfiguredPool
        { operatePool = \nodeParams action -> do

            let NodeParams genesisFiles hardForks (port, peers) logCfg = nodeParams
            let logCfg' = setLoggingName name logCfg

            topology <- genTopology dir peers
            withStaticServer dir $ \url -> do
                traceWith tr $ MsgStartedStaticServer dir url

                (config, block0, bp, vd, genesisPools)
                    <- genNodeConfig
                        dir
                        ""
                        genesisFiles
                        hardForks
                        logCfg'

                let cfg = CardanoNodeConfig
                        { nodeDir = dir
                        , nodeConfigFile = config
                        , nodeTopologyFile = topology
                        , nodeDatabaseDir = "db"
                        , nodeDlgCertFile = Nothing
                        , nodeSignKeyFile = Nothing
                        , nodeOpCertFile = Just opCert
                        , nodeKesKeyFile = Just kesPrv
                        , nodeVrfKeyFile = Just vrfPrv
                        , nodePort = Just (NodePort port)
                        , nodeLoggingHostname = Just name
                        }

                withCardanoNodeProcess tr name cfg $ \socket -> do
                    action $ RunningNode socket block0 (bp, vd) genesisPools

        , registerViaShelleyGenesis = do
            poolId <- stakePoolIdFromOperatorVerKey opPub
            vrf <- poolVrfFromFile vrfPub
            stakePubHash <- stakingKeyHashFromFile ownerPub
            pledgeAddr <- stakingAddrFromVkFile ownerPub

            let params = Ledger.PoolParams
                  { _poolId = poolId
                  , _poolVrf = vrf
                  , _poolPledge = Ledger.Coin $ intCast pledgeAmt
                  , _poolCost = Ledger.Coin 0
                  , _poolMargin = unsafeUnitInterval 0.1
                  , _poolRAcnt = Ledger.RewardAcnt Mainnet $ Ledger.KeyHashObj stakePubHash
                  , _poolOwners = Set.fromList [stakePubHash]
                  , _poolRelays = mempty
                  , _poolMD = SJust $ Ledger.PoolMetadata
                        (fromMaybe (error "invalid url (too long)")
                            $ textToUrl
                            $ T.pack metadataURL)
                        (blake2b256 (BL.toStrict metadataBytes))
                  }

            let updateStaking sgs = sgs
                    { Ledger.sgsPools =
                        (ListMap.ListMap [(poolId, params)])
                            <> (sgsPools sgs)
                    , Ledger.sgsStake =
                        (ListMap.fromList [(stakePubHash, poolId)])
                            <> Ledger.sgsStake sgs
                    }
            let poolSpecificFunds = ListMap.fromList
                    [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]

            return $ \sg -> sg
                { sgInitialFunds = poolSpecificFunds <> sgInitialFunds sg
                , sgStaking = updateStaking (sgStaking sg)
                }

        , finalizeShelleyGenesisSetup = \(RunningNode socket _ _ _) -> do
            -- Here is our chance to respect the 'retirementEpoch' of
            -- the 'PoolRecipe'.
            --
            -- NOTE: We also submit the retirement cert in
            -- @registerViaTx@, but this seems to work regardless. (We
            -- do want to submit it here for the sake of babbage)
            let retire e = do
                    retCert <- issuePoolRetirementCert tr dir opPub e
                    (rawTx, faucetPrv)
                        <- preparePoolRetirement tr dir era [retCert]
                    tx <- signTx tr dir rawTx [faucetPrv, ownerPrv, opPrv]
                    submitTx tr socket "retirement cert" tx

            traverse_ retire mretirementEpoch
        , registerViaTx = \(RunningNode socket _ _ _) -> do
            stakeCert <- issueStakeVkCert tr dir "stake-pool" ownerPub
            let poolRegistrationCert = dir </> "pool.cert"
            cli tr
                [ "stake-pool", "registration-certificate"
                , "--cold-verification-key-file", opPub
                , "--vrf-verification-key-file", vrfPub
                , "--pool-pledge", show pledgeAmt
                , "--pool-cost", "0"
                , "--pool-margin", "0.1"
                , "--pool-reward-account-verification-key-file", ownerPub
                , "--pool-owner-stake-verification-key-file", ownerPub
                , "--metadata-url", metadataURL
                , "--metadata-hash", blake2b256S (BL.toStrict metadataBytes)
                , "--mainnet"
                , "--out-file", poolRegistrationCert
                ]


            mPoolRetirementCert <- traverse
                (issuePoolRetirementCert tr dir opPub) mretirementEpoch
            dlgCert <- issueDlgCert tr dir ownerPub opPub

            -- In order to get a working stake pool we need to.
            --
            -- 1. Register a stake key for our pool.
            -- 2. Register the stake pool
            -- 3. Delegate funds to our pool's key.
            --
            -- We cheat a bit here by delegating to our stake address right away
            -- in the transaction used to registered the stake key and the pool
            -- itself.  Thus, in a single transaction, we end up with a
            -- registered pool with some stake!

            let certificates = catMaybes
                    [ pure stakeCert
                    , pure poolRegistrationCert
                    , pure dlgCert
                    , mPoolRetirementCert
                    ]
            (rawTx, faucetPrv) <- preparePoolRegistration
                tr dir era ownerPub certificates pledgeAmt
            tx <- signTx tr dir rawTx [faucetPrv, ownerPrv, opPrv]
            submitTx tr socket name tx
        , metadataUrl = T.pack metadataURL
        , recipe = recipe
        }

defaultPoolConfigs :: NonEmpty PoolRecipe
defaultPoolConfigs = NE.zipWith (\i p -> p {index = i}) (1 :| [2..]) $
     -- This pool should never retire:
    PoolRecipe
        { pledgeAmt = 200 * millionAda
        , retirementEpoch = Nothing
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool A"
              , "ticker" .= Aeson.String "GPA"
              , "description" .= Aeson.Null
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "ec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9c2bbe6838dc6"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820d8f81c455ef786f47ad9f573e49dc417e0125dfa8db986d6c0ddc03be8634dc6"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "82005820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                ]
              )

        , index = undefined
        } :|
    -- This pool should retire almost immediately:
    [ PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Just 3
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool B"
              , "ticker" .= Aeson.String "GPB"
              , "description" .= Aeson.Null
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
    , operatorKeys =
          ( PoolId $ unsafeFromHex
              "1b3dc19c6ab89eaffc8501f375bb03c11bf8ed5d183736b1d80413d6"
          , Aeson.object
              [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
              , "description" .= Aeson.String "Stake pool operator key"
              , "cborHex" .= Aeson.String
                  "5820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
              ]
          , Aeson.object
              [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
              , "description" .= Aeson.String "Stake pool operator key"
              , "cborHex" .= Aeson.String
                  "5820fab9d94c52b3e222ed494f84020a29ef8405228d509a924106d05ed01c923547"
              ]
          , Aeson.object
              [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
              , "description" .= Aeson.String "Next certificate issue number: 0"
              , "cborHex" .= Aeson.String
                  "82005820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
              ]
          )
        , index = undefined
        }

    -- This pool should retire, but not within the duration of a test run:
    , PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Just 100_000
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool C"
              , "ticker" .= Aeson.String "GPC"
              , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = True
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820047572e48be93834d6d7ddb01bb1ad889b4de5a7a1a78112f1edd46284250869"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "82005820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                ]
            )
        , index = undefined
        }
    -- This pool should retire, but not within the duration of a test run:
    , PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Just 1_000_000
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool D"
              , "ticker" .= Aeson.String "GPD"
              , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "bb114cb37d75fa05260328c235a3dae295a33d0ba674a5eb1e3e568e"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake Pool Operator Verification Key"
                , "cborHex" .= Aeson.String
                    "58203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake Pool Operator Signing Key"
                , "cborHex" .= Aeson.String
                    "58208f50de27d74325eaf57767d70277210b31eb97cdc3033f632a9791a3677a64d2"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "820058203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                ]
            )
        , index = undefined
        }
    ]
  where
    millionAda = 1_000_000_000_000

localClusterConfigFromEnv :: IO LocalClusterConfig
localClusterConfigFromEnv = do
    era <- clusterEraFromEnv
    LocalClusterConfig defaultPoolConfigs era
        <$> logFileConfigFromEnv (Just $ clusterEraToString era)

data ClusterEra
    = ByronNoHardFork
    | ShelleyHardFork
    | AllegraHardFork
    | MaryHardFork
    | AlonzoHardFork
    | BabbageHardFork
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Convert @ClusterEra@ to a @ApiEra@.
clusterToApiEra :: ClusterEra -> ApiEra
clusterToApiEra = \case
    ByronNoHardFork -> ApiByron
    ShelleyHardFork -> ApiShelley
    AllegraHardFork -> ApiAllegra
    MaryHardFork -> ApiMary
    AlonzoHardFork -> ApiAlonzo
    BabbageHardFork -> ApiBabbage

-- | Defaults to the latest era.
clusterEraFromEnv :: IO ClusterEra
clusterEraFromEnv =
    fmap withDefault . traverse getEra =<< lookupEnvNonEmpty var
  where
    var = "LOCAL_CLUSTER_ERA"
    getEra env = case map toLower env of
        "byron" -> pure ByronNoHardFork
        "shelley" -> pure ShelleyHardFork
        "allegra" -> pure AllegraHardFork
        "mary" -> pure MaryHardFork
        "alonzo" -> pure AlonzoHardFork
        "babbage" -> pure BabbageHardFork
        _ -> die $ var ++ ": unknown era"
    withDefault = fromMaybe maxBound

clusterEraToString :: ClusterEra -> String
clusterEraToString = \case
    ByronNoHardFork -> "byron"
    ShelleyHardFork -> "shelley"
    AllegraHardFork -> "allegra"
    MaryHardFork    -> "mary"
    AlonzoHardFork  -> "alonzo"
    BabbageHardFork -> "babbage"

data LocalClusterConfig = LocalClusterConfig
    { cfgStakePools :: NonEmpty PoolRecipe
    -- ^ Stake pools to register.
    , cfgLastHardFork :: ClusterEra
    -- ^ Which era to use.
    , cfgNodeLogging :: LogFileConfig
    -- ^ Log severity for node.
    } deriving (Show)

-- | Information about a launched node.
data RunningNode = RunningNode
    CardanoNodeConn
    -- ^ Socket path
    Block
    -- ^ Genesis block
    (NetworkParameters, NodeToClientVersionData)
    [PoolCertificate]
    -- ^ Shelley genesis pools
    deriving (Show, Eq)


unsafeUnitInterval :: Rational -> UnitInterval
unsafeUnitInterval x = fromMaybe
        (error $ "unsafeUnitInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafeNonNegativeInterval :: Rational -> NonNegativeInterval
unsafeNonNegativeInterval x = fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafePositiveUnitInterval :: Rational -> PositiveUnitInterval
unsafePositiveUnitInterval x = fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

generateGenesis
    :: FilePath
    -> UTCTime
    -> [(Address, Coin)]
    -> (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
       -- ^ For adding genesis pools and staking in Babbage and later.
    -> IO GenesisFiles
generateGenesis dir systemStart initialFunds addPoolsToGenesis = do
    source <- getShelleyTestDataPath
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "alonzo-genesis.yaml")
        >>= Aeson.encodeFile (dir </> "genesis.alonzo.json")

    let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart
    let systemStart' = posixSecondsToUTCTime . fromRational . toRational $ startTime

    let pparams = Ledger.ShelleyPParams
            { _minfeeA = 100
            , _minfeeB = 100_000
            , _minUTxOValue = Ledger.Coin 1_000_000

            , _keyDeposit = Ledger.Coin 1_000_000
            , _poolDeposit = Ledger.Coin 0

            , _maxBBSize = 239_857
            , _maxBHSize = 217_569
            , _maxTxSize = 16_384

            , _minPoolCost = Ledger.Coin 0

            , _extraEntropy = Ledger.NeutralNonce

            -- There are a few smaller features/fixes which are enabled based on
            -- the protocol version rather than just the era, so we need to
            -- set it to a realisitic value.
            , _protocolVersion = Ledger.ProtVer 8 0

            -- Sensible pool & reward parameters:
            , _nOpt = 3
            , _rho = unsafeUnitInterval 0.178_650_067
            , _tau = unsafeUnitInterval 0.1
            , _a0 = unsafeNonNegativeInterval 0.1
            , _d = unsafeUnitInterval 0

            -- The epoch bound on pool retirements specifies how many epochs
            -- in advance retirements may be announced. For testing purposes,
            -- we allow retirements to be announced far into the future.
            ,  _eMax = 1_000_000
            }

    let sg = addPoolsToGenesis $ ShelleyGenesis
            { sgSystemStart = systemStart'
            , sgActiveSlotsCoeff = unsafePositiveUnitInterval 0.5
            , sgSlotLength = 0.2
            , sgSecurityParam = 10
            , sgEpochLength = 160
            , sgUpdateQuorum = 1
            , sgNetworkMagic = 764_824_073
            , sgSlotsPerKESPeriod = 86_400
            , sgMaxKESEvolutions = 5
            , sgNetworkId = Mainnet
            , sgMaxLovelaceSupply = 1_000_000_000_000_000_000
            , sgProtocolParams = pparams
            , sgInitialFunds = extraInitialFunds
            , sgStaking = Ledger.emptyGenesisStaking

            -- We need this to submit MIR certs (and probably for the BFT node
            -- pre-babbage):
            , sgGenDelegs = fromRight (error "invalid sgGenDelegs") $ Aeson.eitherDecode $ Aeson.encode [aesonQQ| {
                    "8ae01cab15f6235958b1147e979987bbdb90788f7c4e185f1632427a": {
                        "delegate": "b7bf59bb963aa785afe220f5b0d3deb826fd0bcaeeee58cb81ab443d",
                        "vrf": "4ebcf8b4c13c24d89144d72f544d1c425b4a3aa1ace30af4eb72752e75b40d3e"
                    }
                }
                |]
            }

    let shelleyGenesisFile = (dir </> "genesis.json")
    Aeson.encodeFile shelleyGenesisFile sg

    let byronGenesisFile = dir </> "genesis.byron.json"
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "byron-genesis.yaml")
        >>= withAddedKey "startTime" startTime
        >>= Aeson.encodeFile byronGenesisFile

    return $ GenesisFiles
        { byronGenesis = byronGenesisFile
        , shelleyGenesis = dir </> "genesis.json"
        , alonzoGenesis = dir </> "genesis.alonzo.json"
        }

  where
    extraInitialFunds
        :: ListMap (Ledger.Addr (Crypto StandardShelley)) Ledger.Coin
    extraInitialFunds = ListMap.fromList
        [ ( fromMaybe (error "extraFunds: invalid addr")
          $ Ledger.deserialiseAddr addrBytes
          , Ledger.Coin $ intCast c
          )
        | (Address addrBytes, Coin c) <- initialFunds
        ]

data FaucetFunds = FaucetFunds
    { pureAdaFunds :: [(Address, Coin)]
      -- ^ Pure ada funds
    , maFunds :: [(Address, (TokenBundle, [(String, String)]))]
      -- ^ Multi asset funds. Slower to setup than pure ada funds.
      --
      -- Beside the assets, there is a list of
      -- @(signing key, verification key hash)@, so that they can be minted by
      -- the faucet.
    , mirFunds :: [(Credential, Coin)]
      -- ^ "Move instantaneous rewards" - for easily funding reward accounts.
    } deriving (Eq, Show)

instance Semigroup FaucetFunds where
    FaucetFunds ada1 ma1 mir1 <> FaucetFunds ada2 ma2 mir2
        = FaucetFunds (ada1 <> ada2) (ma1 <> ma2) (mir1 <> mir2)

instance Monoid FaucetFunds where
    mempty = FaucetFunds [] [] []

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
--
-- The cluster is configured to automatically hard fork to Shelley at epoch 1
-- and then to Allegra at epoch 2. Callback actions can be provided to run
-- a little time after the hard forks are scheduled.
--
-- The callback actions are not guaranteed to use the same node.
withCluster
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging.
    -> FilePath
    -- ^ Temporary directory to create config files in.
    -> LocalClusterConfig
    -- ^ The configurations of pools to spawn.
    -> FaucetFunds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withCluster tr dir LocalClusterConfig{..} faucetFunds onClusterStart = bracketTracer' tr "withCluster" $ do
    withPoolMetadataServer tr dir $ \metadataServer -> do
        createDirectoryIfMissing True dir
        traceWith tr $ MsgStartingCluster dir
        resetGlobals

        systemStart <- addUTCTime 1 <$> getCurrentTime
        configuredPools <-
            configurePools tr dir cfgLastHardFork metadataServer cfgStakePools

        addGenesisPools <- do
            genesisDeltas <- mapM registerViaShelleyGenesis configuredPools
            pure $ foldr (.) id genesisDeltas
        let federalizeNetwork =
                let
                    adjustPParams f genesis = genesis
                        { sgProtocolParams = f (sgProtocolParams genesis) }
                in
                    adjustPParams (setField @"_d" (unsafeUnitInterval 0.25))

        genesisFiles <- generateGenesis
            dir
            systemStart
            (adaFunds <> internalFaucetFunds)
            (if postAlonzo then addGenesisPools else federalizeNetwork)

        if postAlonzo
            then do
                port0:ports <- rotate <$> randomUnusedTCPPorts nPools
                let pool0 :| otherPools = configuredPools

                let pool0Cfg = NodeParams
                        genesisFiles
                        cfgLastHardFork
                        port0
                        cfgNodeLogging
                operatePool pool0 pool0Cfg $ \runningPool0 -> do
                    extraClusterSetupUsingNode configuredPools runningPool0
                    case NE.nonEmpty otherPools of
                        Nothing -> onClusterStart runningPool0
                        Just others ->
                            launchPools
                                others
                                genesisFiles
                                ports
                                runningPool0
                                onClusterStart
            else do
                -- NOTE: We should soon be able to drop Alonzo support here
                -- after the Vasil HF, which should enable some simplifications
                -- of the logic in 'withCluster'.
                ports <- rotate <$> randomUnusedTCPPorts (1 + nPools)
                let bftCfg = NodeParams
                        genesisFiles
                        cfgLastHardFork
                        (head ports)
                        cfgNodeLogging
                withBFTNode tr dir bftCfg $ \runningBFTNode -> do
                    extraClusterSetupUsingNode configuredPools runningBFTNode

                    -- NOTE: We used to perform 'registerViaTx' as part of
                    -- 'launchPools' where we waited for the pools to become
                    -- active (e.g. be in the stake distribution) in parallel.
                    -- Just submitting the registration certs in sequence
                    -- /seems/ to work though, and the setup working 100%
                    -- correctly in alonzo will soon not be important.
                    mapM_ (`registerViaTx` runningBFTNode) configuredPools
                    launchPools
                        configuredPools
                        genesisFiles
                        (tail ports)
                        runningBFTNode
                        onClusterStart
  where
    nPools = length cfgStakePools

    postAlonzo = cfgLastHardFork >= BabbageHardFork

    FaucetFunds adaFunds maFunds mirFunds = faucetFunds

    -- Important cluster setup to run without rollbacks
    extraClusterSetupUsingNode ::
        NonEmpty (ConfiguredPool) -> RunningNode -> IO ()
    extraClusterSetupUsingNode configuredPools runningNode = do
        let RunningNode conn _ _ _ = runningNode

        -- Needs to happen in the first 20% of the epoch, so we run this
        -- first.
        moveInstantaneousRewardsTo tr conn dir cfgLastHardFork mirFunds

        -- Submit retirement certs for all pools using the connection to
        -- the only running first pool to avoid the certs being rolled
        -- back.
        --
        -- We run these second in hope that it reduces the risk that any of the
        -- txs fail to make it on-chain. If this were to happen when running the
        -- integration tests, the integration tests /will fail/ (c.f. #3440).
        -- Later setup is less sensitive. Using a wallet with retrying
        -- submission pool might also be an idea for the future.
        when postAlonzo $
            forM_ configuredPools $ \pool -> do
                finalizeShelleyGenesisSetup pool runningNode

        sendFaucetAssetsTo tr conn dir cfgLastHardFork
            20
            (encodeAddresses maFunds)

        -- Should ideally not be hard-coded in 'withCluster'
        (rawTx, faucetPrv) <- prepareKeyRegistration tr dir cfgLastHardFork
        tx <- signTx tr dir rawTx [faucetPrv]
        submitTx tr conn "pre-registered stake key" tx

    -- | Actually spin up the pools.
    launchPools
        :: NonEmpty ConfiguredPool
        -> GenesisFiles
        -> [(Int, [Int])]
        -- @(port, peers)@ pairs availible for the nodes. Can be used to e.g.
        -- add a BFT node as extra peer for all pools.
        -> RunningNode
        -- ^ Backup node to run the action with in case passed no pools.
        -> (RunningNode -> IO a)
        -- ^ Action to run once when the stake pools are setup.
        -> IO a
    launchPools configuredPools genesisFiles ports fallbackNode action = do
        waitGroup <- newChan
        doneGroup <- newChan

        let poolCount = length configuredPools

        let waitAll = do
                traceWith tr $
                    MsgDebug "waiting for stake pools to register"
                replicateM poolCount (readChan waitGroup)

        let onException :: SomeException -> IO ()
            onException e = do
                traceWith tr $
                    MsgDebug $ "exception while starting pool: " <>
                    T.pack (show e)
                writeChan waitGroup (Left e)

        let mkConfig (port, peers) =
                NodeParams
                genesisFiles
                cfgLastHardFork
                (port, peers)
                cfgNodeLogging
        asyncs <- forM (zip (NE.toList configuredPools) ports) $
            \(configuredPool, (port, peers)) -> do
                async $ handle onException $ do
                    let cfg = mkConfig (port, peers)
                    operatePool configuredPool cfg $ \runningPool -> do
                            writeChan waitGroup $ Right runningPool
                            readChan doneGroup
        mapM_ link asyncs
        let cancelAll = do
                traceWith tr $ MsgDebug "stopping all stake pools"
                replicateM_ poolCount (writeChan doneGroup ())
                mapM_ wait asyncs

        traceWith tr $ MsgRegisteringStakePools poolCount
        group <- waitAll
        if length (filter isRight group) /= poolCount
            then do
                cancelAll
                let errors = show (filter isLeft group)
                throwIO $ ProcessHasExited
                    ("cluster didn't start correctly: " <> errors)
                    (ExitFailure 1)
            else do
                -- Run the action using the connection to the first pool, or the
                -- fallback.
                let node = case group of
                        [] -> fallbackNode
                        Right firstPool : _ -> firstPool
                        Left e : _ -> error $ show e
                action node `finally` cancelAll


    -- | Get permutations of the size (n-1) for a list of n elements, alongside
    -- with the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap f . permutations
      where
       f = \case
        [] -> error "rotate: impossible"
        x : xs -> (x, sort xs)

    encodeAddresses = map (first (T.unpack . encodeAddress @'W.Mainnet))

data LogFileConfig = LogFileConfig
    { minSeverityTerminal :: Severity
      -- ^ Minimum logging severity
    , extraLogDir :: Maybe FilePath
      -- ^ Optional additional output to log file
    , minSeverityFile :: Severity
      -- ^ Minimum logging severity for 'extraLogFile'
    } deriving (Show)

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams = NodeParams
    { nodeGenesisFiles :: GenesisFiles
      -- ^ Genesis block start time
    , nodeHardForks :: ClusterEra
      -- ^ Era to hard fork into.
    , nodePeers :: (Int, [Int])
      -- ^ A list of ports used by peers and this node
    , nodeLogConfig :: LogFileConfig
      -- ^ The node will always log to "cardano-node.log" relative to the
      -- config. This option can set the minimum severity and add another output
      -- file.
    } deriving (Show)

singleNodeParams :: GenesisFiles -> Severity -> Maybe (FilePath, Severity) -> NodeParams
singleNodeParams genesisFiles severity extraLogFile =
    let
        logCfg = LogFileConfig
            { minSeverityTerminal = severity
            , extraLogDir = fmap fst extraLogFile
            , minSeverityFile = maybe severity snd extraLogFile
            }
    in
        NodeParams genesisFiles maxBound (0, []) logCfg

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr baseDir params action =
    bracketTracer' tr "withBFTNode" $ do
        createDirectoryIfMissing False dir
        source <- getShelleyTestDataPath

        let copyKeyFile f = do
                let dst = dir </> f
                copyFile (source </> f) dst
                restrictFileMode dst
                pure dst

        [bftCert, bftPrv, vrfPrv, kesPrv, opCert] <- forM
            [ "bft-leader" <> ".byron.cert"
            , "bft-leader" <> ".byron.skey"
            , "bft-leader" <> ".vrf.skey"
            , "bft-leader" <> ".kes.skey"
            , "bft-leader" <> ".opcert"
            ]
            copyKeyFile

        (config, block0, networkParams, versionData, genesisPools)
            <- genNodeConfig dir "-bft" genesisFiles hardForks (setLoggingName name logCfg)
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Just bftCert
                , nodeSignKeyFile = Just bftPrv
                , nodeOpCertFile = Just opCert
                , nodeKesKeyFile = Just kesPrv
                , nodeVrfKeyFile = Just vrfPrv
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg $ \socket ->
            action $ RunningNode socket block0 (networkParams, versionData) genesisPools
  where
    name = "bft"
    dir = baseDir </> name
    NodeParams genesisFiles hardForks (port, peers) logCfg = params

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
--
-- FIXME: Do we really need the relay node? If so we should re-add it to
-- withCluster, rather than connecting the wallet to one of the pools.
_withRelayNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with socket path
    -> IO a
_withRelayNode tr baseDir params act =
    bracketTracer' tr "withRelayNode" $ do
        createDirectory dir

        let logCfg' = setLoggingName name logCfg
        (config, block0, bp, vd, _genesisPools)
            <- genNodeConfig dir "-relay" genesisFiles hardForks logCfg'
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Nothing
                , nodeSignKeyFile = Nothing
                , nodeOpCertFile = Nothing
                , nodeKesKeyFile = Nothing
                , nodeVrfKeyFile = Nothing
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        let act' socket = act $ RunningNode socket block0 (bp, vd) []
        withCardanoNodeProcess tr name cfg act'
  where
    name = "node"
    dir = baseDir </> name
    NodeParams genesisFiles hardForks (port, peers) logCfg = params

-- | Run a SMASH stub server, serving some delisted pool IDs.
withSMASH
    :: Tracer IO ClusterLog
    -> FilePath
    -- ^ Parent directory to store static files
    -> (String -> IO a)
    -- ^ Action, taking base URL
    -> IO a
withSMASH tr parentDir action = do
    let staticDir = parentDir </> "smash"
    let baseDir = staticDir </> "api" </> "v1"


    -- write pool metadatas
    forM_ defaultPoolConfigs $ \pool -> do
        let (poolId, _, _, _) = operatorKeys pool
        let metadata = poolMetadata pool

        let bytes = Aeson.encode metadata

        let metadataDir = baseDir </> "metadata"
            poolDir = metadataDir </> T.unpack (toText poolId)
            hash = blake2b256S (BL.toStrict bytes)
            hashFile = poolDir </> hash


        traceWith tr $
            MsgRegisteringPoolMetadataInSMASH (T.unpack $ toText poolId) hash

        createDirectoryIfMissing True poolDir
        BL8.writeFile (poolDir </> hashFile) bytes

    -- Write delisted pools
    let toSmashId (PoolId bytes) = SMASHPoolId . T.pack . B8.unpack . hex $ bytes
    let poolId (PoolRecipe _ _ _ _ (pid, _, _, _) _) = toSmashId pid
    let delistedPoolIds = poolId <$> NE.filter delisted defaultPoolConfigs
    BL8.writeFile
        (baseDir </> "delisted")
        (Aeson.encode delistedPoolIds)

    -- health check
    let health = Aeson.encode (HealthStatusSMASH "OK" "1.2.0")
    BL8.writeFile (baseDir </> "status") health


    withStaticServer staticDir action

withCardanoNodeProcess
    :: Tracer IO ClusterLog
    -> String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> IO a
withCardanoNodeProcess tr name cfg = withCardanoNode tr' cfg >=> throwErrs
  where
    tr' = contramap (MsgLauncher name) tr
    throwErrs = either throwIO pure

setLoggingName :: String -> LogFileConfig -> LogFileConfig
setLoggingName name cfg = cfg { extraLogDir = filename <$> extraLogDir cfg }
    where filename = (</> (name <.> "log"))

data GenesisFiles = GenesisFiles
    { byronGenesis :: FilePath
    , shelleyGenesis :: FilePath
    , alonzoGenesis :: FilePath
    } deriving (Show, Eq)

genNodeConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> String -- Node name
    -> GenesisFiles
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> IO (FilePath, Block, NetworkParameters, NodeToClientVersionData, [PoolCertificate])
genNodeConfig dir name genesisFiles clusterEra logCfg = do
    let LogFileConfig severity mExtraLogFile extraSev = logCfg
    let GenesisFiles{byronGenesis,shelleyGenesis,alonzoGenesis} = genesisFiles

    source <- getShelleyTestDataPath

    let fileScribe (path, sev) = ScribeDefinition
                { scName = path
                , scFormat = ScText
                , scKind = FileSK
                , scMinSev = sev
                , scMaxSev = Critical
                , scPrivacy = ScPublic
                , scRotation = Nothing
                }

    let scribes = map fileScribe $ catMaybes
            [ Just ("cardano-node.log", severity)
            , (, extraSev) . T.pack <$> mExtraLogFile
            ]

    ----
    -- Configuration
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesis
        >>= withAddedKey "ByronGenesisFile" byronGenesis
        >>= withAddedKey "AlonzoGenesisFile" alonzoGenesis
        >>= withHardForks clusterEra
        >>= withAddedKey "minSeverity" Debug
        >>= withScribes scribes
        >>= withObject (addMinSeverityStdout severity)
        >>= Yaml.encodeFile (dir </> ("node" <> name <> ".config"))


    -- Parameters
    sg <- Yaml.decodeFileThrow
        @_ @(ShelleyGenesis StandardShelley) shelleyGenesis

    let (np, block0, genesisPools) = fromGenesisData sg
    let networkMagic = sgNetworkMagic sg
    let versionData = NodeToClientVersionData $ NetworkMagic networkMagic

    pure
        ( dir </> ("node" <> name <> ".config")
        , block0
        , np
        , versionData
        , genesisPools
        )
  where
    withScribes scribes =
        withAddedKey "setupScribes" scribes
        >=> withAddedKey "defaultScribes"
            (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    withHardForks era =
        withObject (pure . Aeson.union (Aeson.fromList hardForks))
      where
        hardForks =
            [ (Aeson.fromText $ "Test" <> T.pack (show hardFork) <> "AtEpoch"
              , Yaml.Number 0
              )
            | hardFork <- [ShelleyHardFork .. era]
            ]

withAddedKey
    :: (MonadFail m, Yaml.ToJSON a)
    => Aeson.Key
    -> a
    -> Aeson.Value
    -> m Aeson.Value
withAddedKey k v = withObject (pure . Aeson.insert k (toJSON v))

-- | Generate a topology file from a list of peers.
genTopology :: FilePath -> [Int] -> IO FilePath
genTopology dir peers = do
    let file = dir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object [ "Producers" .= map encodePeer peers ]
    pure file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port = Aeson.object
        [ "addr"    .= ("127.0.0.1" :: String)
        , "port"    .= port
        , "valency" .= (1 :: Int)
        ]
-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: Tracer IO ClusterLog
    -> FilePath
    -> PoolRecipe
    -> IO (FilePath, FilePath, FilePath)
writeOperatorKeyPair tr dir recipe = do
    let (_pId, pub, prv, count) = operatorKeys recipe

    traceWith tr $ MsgGenOperatorKeyPair dir

    let opPub = dir </> "op.pub"
    let opPrv = dir </> "op.prv"
    let opCount = dir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure (opPrv, opPub, opCount)

-- | Create a key pair for a node KES operational key
genKesKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genKesKeyPair tr dir = do
    let kesPub = dir </> "kes.pub"
    let kesPrv = dir </> "kes.prv"
    cli tr
        [ "node", "key-gen-KES"
        , "--verification-key-file", kesPub
        , "--signing-key-file", kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genVrfKeyPair tr dir = do
    let vrfPub = dir </> "vrf.pub"
    let vrfPrv = dir </> "vrf.prv"
    cli tr
        [ "node", "key-gen-VRF"
        , "--verification-key-file", vrfPub
        , "--signing-key-file", vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair :: Tracer IO ClusterLog -> (FilePath, FilePath) -> IO ()
genStakeAddrKeyPair tr (stakePrv, stakePub)= do
    cli tr
        [ "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]

-- | Issue a node operational certificate
issueOpCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert tr dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    cli tr
        [ "node", "issue-op-cert"
        , "--kes-verification-key-file", kesPub
        , "--cold-signing-key-file", opPrv
        , "--operational-certificate-issue-counter-file", opCount
        , "--kes-period", "0"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> String
    -> FilePath
    -> IO FilePath
issueStakeVkCert tr dir prefix stakePub = do
    let file = dir </> prefix <> "-stake.cert"
    cli tr
        [ "stake-address", "registration-certificate"
        , "--staking-verification-key-file", stakePub
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> String
    -> FilePath
    -> IO FilePath
issueStakeScriptCert tr dir prefix stakeScript = do
    let file = dir </> prefix <> "-stake.cert"
    cli tr
        [ "stake-address", "registration-certificate"
        , "--stake-script-file", stakeScript
        , "--out-file", file
        ]
    pure file


stakePoolIdFromOperatorVerKey
    :: FilePath -> IO (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakePoolKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: FilePath -> IO (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsVrfKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: FilePath -> IO (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakeKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: FilePath -> IO (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakeKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH = either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH = either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    return $ Ledger.Addr Mainnet
        (Ledger.KeyHashObj payKH)
        (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

issuePoolRetirementCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> EpochNo
    -> IO FilePath
issuePoolRetirementCert tr dir opPub retirementEpoch = do
    let file  = dir </> "pool-retirement.cert"
    cli tr
        [ "stake-pool", "deregistration-certificate"
        , "--cold-verification-key-file", opPub
        , "--epoch", show (unEpochNo retirementEpoch)
        , "--out-file", file
        ]
    pure file

-- | Create a stake address delegation certificate.
issueDlgCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> IO FilePath
issueDlgCert tr dir stakePub opPub = do
    let file = dir </> "dlg.cert"
    cli tr
        [ "stake-address", "delegation-certificate"
        , "--staking-verification-key-file", stakePub
        , "--stake-pool-verification-key-file", opPub
        , "--out-file", file
        ]
    pure file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
preparePoolRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> ClusterEra
    -> FilePath
    -> [FilePath]
    -> Integer
    -> IO (FilePath, FilePath)
preparePoolRegistration tr dir era stakePub certs pledgeAmt = do
    let file = dir </> "tx.raw"
    addr <- genSinkAddress tr dir (Just stakePub)
    (faucetInput, faucetPrv) <- takeFaucet
    cli tr $
        [ "transaction", "build-raw"
        , cliEraFlag era
        , "--tx-in", faucetInput
        , "--tx-out", addr <> "+" <> show pledgeAmt
        , "--ttl", "400"
        , "--fee", show (faucetAmt - pledgeAmt - depositAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

preparePoolRetirement
    :: Tracer IO ClusterLog
    -> FilePath
    -> ClusterEra
    -> [FilePath]
    -> IO (FilePath, FilePath)
preparePoolRetirement tr dir era certs = do
    let file = dir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet
    cli tr $
        [ "transaction", "build-raw"
        , cliEraFlag era
        , "--tx-in", faucetInput
        , "--ttl", "400"
        , "--fee", show (faucetAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory
    -> IO (String, (String, String))
genMonetaryPolicyScript tr dir = do
    let policyPub = dir </> "policy.pub"
    let policyPrv = dir </> "policy.prv"

    cli tr
        [ "address", "key-gen"
        , "--verification-key-file", policyPub
        , "--signing-key-file", policyPrv
        ]
    skey <- T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <- cliLine tr
        [ "address", "key-hash"
        , "--payment-verification-key-file", policyPub
        ]
    script <- writeMonetaryPolicyScriptFile dir vkeyHash
    policyId <- cliLine tr
        [ "transaction", "policyid"
        , "--script-file", script
        ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: FilePath -- ^ Destination directory for script file
    -> String -- ^ The script verification key hash
    -> IO FilePath -- ^ Returns the filename written
writeMonetaryPolicyScriptFile dir keyHash = do
    let scriptFile = dir </> keyHash <.> "script"
    Aeson.encodeFile scriptFile $ object
        [ "type" .= Aeson.String "sig"
        , "keyHash" .= keyHash
        ]
    pure scriptFile

writePolicySigningKey
    :: FilePath -- ^ destination directory for key file
    -> String -- ^ Name of file, keyhash perhaps.
    -> String -- ^ The cbor-encoded key material, encoded in hex
    -> IO FilePath -- ^ Returns the filename written
writePolicySigningKey dir keyHash cborHex = do
    let keyFile = dir </> keyHash <.> "skey"
    Aeson.encodeFile keyFile $ object
        [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
        , "description" .= Aeson.String "Payment Signing Key"
        , "cborHex" .= cborHex
        ]
    pure keyFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: FilePath -> IO Text
readKeyFromFile f = do
    textView <- either throwString pure =<< Aeson.eitherDecodeFileStrict' f
    either throwString pure $ Aeson.parseEither
        (Aeson.withObject "TextView" (.: "cborHex")) textView

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> ClusterEra
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn dir era targets = batch 80 targets $
    sendFaucet tr conn dir era "ada" . map coinBundle
  where
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> ClusterEra
    -> Int -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))] -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn dir era batchSize targets = do
    when (era >= MaryHardFork) $
        batch batchSize targets $ sendFaucet tr conn dir era "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> ClusterEra
    -> String -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn dir era what targets = do
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords $ [ addr, show c, "lovelace"] ++
                map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) = mconcat
            [ T.unpack (toText pid)
            , if B8.null name then "" else "."
            , B8.unpack (hex name)
            ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total = fromIntegral $ sum $
            map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM (nub $ concatMap (map snd . snd . snd) targets) $
        writeMonetaryPolicyScriptFile dir

    cli tr $
        [ "transaction", "build-raw"
        , cliEraFlag era
        , "--tx-in", faucetInput
        , "--ttl", "6000000"
            -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
        , "--fee", show (faucetAmt - total)
        , "--out-file", file
        ] ++
        concatMap (uncurry mkOutput . fmap fst) targets ++
        mkMint targetAssets ++
        (concatMap (\f -> ["--minting-script-file", f]) scripts)

    policyKeys <- forM (nub $ concatMap (snd . snd) targets) $
        \(skey, keyHash) -> writePolicySigningKey dir keyHash skey

    tx <- signTx tr dir file (faucetPrv:policyKeys)
    submitTx tr conn (what ++ " faucet tx") tx

batch :: Int -> [a] -> ([a] -> IO b) -> IO ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

data Credential
    = KeyCredential XPub
    | ScriptCredential ByteString
    deriving (Eq, Show)

moveInstantaneousRewardsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> ClusterEra
    -> [(Credential, Coin)]
    -> IO ()
moveInstantaneousRewardsTo tr conn dir era targets = do
    certs <- mapM mkCredentialCerts targets
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "mir-tx.raw"

    let total = fromIntegral $ sum $ map (unCoin . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $ error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress tr dir Nothing

    cli tr $
        [ "transaction", "build-raw"
        , cliEraFlag era
        , "--tx-in", faucetInput
        , "--ttl", "999999999"
        , "--fee", show (faucetAmt - 1_000_000 - totalDeposit)
        , "--tx-out", sink <> "+" <> "1000000"
        , "--out-file", file
        ] ++ concatMap (\x -> ["--certificate-file", x]) (mconcat certs)

    testData <- getShelleyTestDataPath
    let bftPrv = testData </> "bft-leader" <> ".skey"

    tx <- signTx tr dir file [faucetPrv, bftPrv]
    submitTx tr conn "MIR certificates" tx
  where
    mkCredentialCerts
        :: (Credential, Coin)
        -> IO [FilePath]
    mkCredentialCerts = \case
        (KeyCredential xpub, coin) -> do
            (prefix, vkFile) <- mkVerificationKey xpub
            stakeAddr <- cliLine tr
                [ "stake-address"
                , "build"
                , "--mainnet"
                , "--stake-verification-key-file" , vkFile
                ]
            stakeCert <- issueStakeVkCert tr dir prefix vkFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [stakeCert, mirCert]

        (ScriptCredential script, coin) -> do
            (prefix, scriptFile) <- mkScript script
            -- NOTE: cardano-cli does not support creating stake-address from
            -- scripts just yet... So it's a bit ugly, but we create a stake
            -- address by creating a standard address, and replacing the header.
            stakeAddr <- toStakeAddress <$> cliLine tr
                [ "address"
                , "build"
                , "--mainnet"
                , "--payment-script-file" , scriptFile
                ]
            stakeCert <- issueStakeScriptCert tr dir prefix scriptFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [stakeCert, mirCert]

      where
        toStakeAddress =
            T.unpack
            . Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
            . BL.toStrict
            . BL.pack . mapFirst (240 .|.) . BL.unpack
            . unsafeBech32Decode
            . T.pack
          where
            hrp = [humanReadablePart|stake|]

    mkVerificationKey
        :: XPub
        -> IO (String, FilePath)
    mkVerificationKey xpub = do
        let base16 = T.unpack $ T.decodeUtf8 $ hex $ xpubPublicKey xpub
        let json = Aeson.object
                [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
                , "description" .= Aeson.String "Stake Verification Key"
                , "cborHex" .= Aeson.String ("5820" <> T.pack base16)
                ]
        let file = dir </> base16 <> ".vk"
        BL8.writeFile file (Aeson.encode json)
        pure (base16, file)

    mkScript
        :: ByteString
        -> IO (String, FilePath)
    mkScript bytes = do
        let base16 = T.decodeUtf8 $ hex $ CBOR.toStrictByteString $ CBOR.encodeBytes bytes
        let json = Aeson.object
                [ "type" .= Aeson.String "PlutusScriptV1"
                , "description" .= Aeson.String ""
                , "cborHex" .= Aeson.String base16
                ]
        let prefix = take 100 (T.unpack base16)
        let file = dir </> prefix <> ".plutus"
        BL8.writeFile file (Aeson.encode json)
        pure (prefix, file)

    mkMIRCertificate
        :: (String, Coin)
        -> IO FilePath
    mkMIRCertificate (stakeAddr, Coin reward) = do
        let mirCert = dir </> stakeAddr <> ".mir"
        cli tr
            [ "governance", "create-mir-certificate"
            , "--reserves"
            , "--reward", show reward
            , "--stake-address", stakeAddr
            , "--out-file", mirCert
            ]
        pure mirCert

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> ClusterEra
    -> IO (FilePath, FilePath)
prepareKeyRegistration tr dir era = do
    let file = dir </> "tx.raw"

    let stakePub = dir </> "pre-registered-stake.pub"
    Aeson.encodeFile stakePub preRegisteredStakeKey

    (faucetInput, faucetPrv) <- takeFaucet

    cert <- issueStakeVkCert tr dir "pre-registered" stakePub
    sink <- genSinkAddress tr dir Nothing

    cli tr
        [ "transaction", "build-raw"
        , cliEraFlag era
        , "--tx-in", faucetInput
        , "--tx-out", sink <> "+" <> "1000000"
        , "--ttl", "400"
        , "--fee", show (faucetAmt - depositAmt - 1_000_000)
        , "--certificate-file", cert
        , "--out-file", file
        ]
    pure (file, faucetPrv)

genSinkAddress
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory to put keys
    -> Maybe FilePath -- ^ Stake pub
    -> IO String
genSinkAddress tr dir stakePub = do
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    cli tr
        [ "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    cliLine tr $
        [ "address", "build"
        , "--mainnet"
        , "--payment-verification-key-file", sinkPub
        ] ++ maybe [] (\key -> ["--stake-verification-key-file", key]) stakePub

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Output directory
    -> FilePath -- ^ Tx body file
    -> [FilePath] -- ^ Signing keys for witnesses
    -> IO FilePath
signTx tr dir rawTx keys = do
    let file = dir </> "tx.signed"
    cli tr $
        [ "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ]
        ++ concatMap (\key -> ["--signing-key-file", key]) keys
    pure file

-- | Submit a transaction through a running node.
submitTx :: Tracer IO ClusterLog -> CardanoNodeConn -> String -> FilePath -> IO ()
submitTx tr conn name signedTx =
    cliRetry tr ("Submitting transaction for " <> T.pack name) =<<
        cliConfigNode tr conn
        [ "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet", "--cardano-mode"
        ]

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: IO (String, String)
takeFaucet = do
    i <- modifyMVar faucetIndex (\i -> pure (i+1, i))
    source <- getShelleyTestDataPath
    let basename = source </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr = fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
            . decodeBase58 bitcoinAlphabet
            . T.encodeUtf8
            . T.strip
            $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (hex $ blake2b256 addr) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (txin, signingKey)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}

-- Funds needed by 'withCluster' itself.
--
-- FIXME: We should generate these programatically. Currently they need to match
-- the files on disk read by 'takeFaucet'.
internalFaucetFunds :: [(Address, Coin)]
internalFaucetFunds = map
    ((,Coin 1_000_000_000_000_000) . unsafeDecodeAddr . T.pack)
  [ "Ae2tdPwUPEZGc7WAmkmXxP3QJ8aiKSMGgfWV6w4A58ebjpr5ah147VvJfDH"
  , "Ae2tdPwUPEZCREUZxa3F1fTyVPMU2MLMYAkRe7DEVoyZsWKahphgdifWuc3"
  , "Ae2tdPwUPEYxL4wYjNxK8z5mCgMmnG1WkMFZaeZ6EGdV2LDZ5pgQzvzVpuo"
  , "Ae2tdPwUPEZMcoAHgC7RvCL9ewjZdj9Yrej2bHJJpvubhkSaRn5Y7dPGKRy"
  , "Ae2tdPwUPEZ7geEbqcaNfMFL8EMpeRYAQrHABau6xUmek87xeyyrmPm4ETc"
  , "Ae2tdPwUPEZNHxjww4RhosX3LMVAzbJtCj3vzoQM3wgLwhEHUp13jX8Xte8"
  , "Ae2tdPwUPEZ8cgFfwvjp9t42v3zQE8nCsjxMpDcdcJZzBocsUK2btirTHDN"
  , "Ae2tdPwUPEZK4VrjHdDpeTfSvWMzNa6qZ5erD2aVmU5S3mCeCZsoT6SJ6NW"
  , "Ae2tdPwUPEZ2pEgBhSNKiUXRfhb5p8jByYiJXAsokHdLGMVeqLjHFNaEr7b"
  , "VhLXUZmS1gXFnDcCzVHi2BqhkA1cvDUZrMvGfYotD4eEjKnkdfid7YsY"
  , "Ae2tdPwUPEYxYSimKRCvz9iqtsCEAeN6KR7SC1dWFYgCVb18ttTrJaht4qz"
  , "Ae2tdPwUPEZ16WMj3KGxQxTtm7cgY2oygWF8Pk1gWRCL9phsawFoJUQo8V4"
  , "Ae2tdPwUPEZ3S2LzBCw3v9qm7ZfADBeHa8GjC4g71bKLeS1HJiNPz58efsG"
  , "Ae2tdPwUPEZ5MEg5J9CJBuanYyoAeq8Usyeh3mTpAjFAfaMUHErZCC6VESB"
  , "Ae2tdPwUPEZKTEGqULNJggS2feij8B5DEkTgvj4pf6BX9xaNWsrk83a94op"
  , "Ae2tdPwUPEZ1x5d9EZgDis5f33LKFR4ZrGwh3uhYVYThiubgFSzSa5ZWWjn"
  , "Ae2tdPwUPEZLEiDLGWsbGYvnKQbDxJaUJ6PPx7ynjAjnLsNjsBB9qfwD8FL"
  , "Ae2tdPwUPEZEMR4QcU9rFCeTK8G6E5ABNAhiuEDzritQarbJ56GBMbPem8v"
  , "Ae2tdPwUPEZMgjLUEpnfpbaGrrBc3mcfLMgzT8JL2rsWcE8YGuwerng4JTx"
  , "Ae2tdPwUPEZCdpgB296udjjMqK4crPXjpMz9zzzk1QARbC844JqYGygKZck"
  , "Ae2tdPwUPEZC7DMJnx7xpRjG9wQXsNtCKvkB5RhDqK9zzra96ugUfMgkw6F"
  , "Ae2tdPwUPEZA2Hxg2X94qnx42UwLdnC2vfjSw1na2jcWnS2LjeoazWgcGqz"
  , "Ae2tdPwUPEYzwDXTM8VDDNG48ZVJPZT5ev3BGpLsBZqkYeP9Ay6keHQiUHN"
  , "Ae2tdPwUPEZK5jjAU6gc8o1Hxk9FGC2JXYR29eRj2zvYDVRy3oJKmzkkWXr"
  , "Ae2tdPwUPEZHRYGpLbcxzKSBFmVghBdUbMLD7Z1RP3CaWmE2MfudSCdLERE"
  , "Ae2tdPwUPEZ3YosvMkMYRuHAzGXmj9FDZiSWxZJxY2bfjtXQupV6cFufGxj"
  , "Ae2tdPwUPEZAUVNwHSzyz3RRhe9hgFNvw6ZBWgusousZEu71AUxwkjTJQXd"
  , "Ae2tdPwUPEZBWbsXKZ6Xj1hVqNrJevo1MguQErP7Ekws9Mwe3QyApRbfzuj"
  , "Ae2tdPwUPEZBwEwpyZ86qJJ5UcBs7zENaB9JmB1ccKKrjF2m8WqYvRLQTUQ"
  , "Ae2tdPwUPEZLVrvsAkoKffT5T2Ny9peTcw1pgDQZGUNuyhsShZYRGdJdg3P"
  , "Ae2tdPwUPEZMMcjnYLD8hNzD8rBuQX4Rbwh4Hrri9wo9Vd3QhWgJp82Q3Zb"
  , "Ae2tdPwUPEZNCXJnNKSoVwATYNRoehHnwhQLeg7Voeun7aKgw7pBELp9Xyx"
  , "Ae2tdPwUPEZMZgPQpYm9VNwW6o1y9gtgmmuto8XxnVzJQnQWNyfbK1ehxhG"
  , "Ae2tdPwUPEYx5Boej5GuTgWrL6yhioVeAN9KybWPCZgfbzTNfE4p134zvFr"
  , "Ae2tdPwUPEZAGMrgFKgSjDymZ6bRhcuCgK53xX5n7xcDUHC8MnijrSVU69g"
  , "Ae2tdPwUPEZL7g7DTRjBp63JMbSouTPJcjjZD6GQCiK3HseKbs2AYHLwcUk"
  , "Ae2tdPwUPEYw3nfF8ceQBJZ3zFL4jP9SFoyJ6N1qYTj6fk1SLaxUhrYFqAp"
  , "Ae2tdPwUPEZBWq2xEQD7NacM1cmTAvnRdwnLX5jGkBvvZpjBCCaTyVbQyCg"
  , "Ae2tdPwUPEZ2BJqnSoUrhVQ4Nf5XmHP6beK1LvYrZFaJqG6PLbHtEKzQCFV"
  , "Ae2tdPwUPEZLGkJsDc5t8WUgPafrvpQkTjXhc3zwZfT2RRSD2SCDwGJ2gko"
  , "Ae2tdPwUPEZG48xoQbHyjEw4sAz4KFFPC6H3RjvZoqDd7ui1hnBoCZ7hjZK"
  , "Ae2tdPwUPEZGjAkaWbCogSWVBjhUxnF2sMRq2QUu82itFU4PAcdo8NkLBGx"
  , "Ae2tdPwUPEZGUUmRGEwhKYoGtuqjubky2tQDB4b59RVsEaMedoNjkgBhz3z"
  , "Ae2tdPwUPEZD4CQHEa9YBp3FgK15dbM8wE4i6VcZczaUNix8U1rnrxrTBqe"
  , "Ae2tdPwUPEZ8uESNVsKkobHzoEZeRpmim475QdWF6CmBdJHWFSJjo9BT5s2"
  , "Ae2tdPwUPEZBhxiuQ3tnhdh5mW8PS5yAJ8jsxYbhs6PvYPx11o7eBs2Nja1"
  , "Ae2tdPwUPEZGXi9taRWo4pYMMZ9WtvvJme3yhmi61PkZEPUaE5c4GhwPVim"
  , "Ae2tdPwUPEZMCPdErTxmgUT4FbQty7tcCmHidJkTAxMpYGF6RYVNkrK1JAR"
  , "Ae2tdPwUPEZ92FRSRqV4dz49btBPRJUEhzyCN4Yh3QZmxGjkD18VxtAvjrJ"
  , "Ae2tdPwUPEZHto9s5ouv4SQha5WpwNrEERfWQDerXgxygM2exm9MSH972o2"
  , "Ae2tdPwUPEYyg77BWtM7HDR9DgtntvnjD5sANzHsXhLSrfHw2QoYnhzVkBV"
  , "Ae2tdPwUPEZ1SBb6wXc9WP5DY3PGRyh6puiaFCUG8mvwPsfijvDvE3FtYV3"
  , "Ae2tdPwUPEYw7n23qBj9dxeTk6vNjGwzHfSXx1zzG1k98smReGMGZmCdwvD"
  , "Ae2tdPwUPEZMsinkhpKJy3yYQ2f486UC1f3iLfeCntEe2AgyWkp3sMxXUZB"
  , "Ae2tdPwUPEZ8V56xa8NY8yAz6pbpyzmbnwneqmHJxoHisXyiiDSubsSDqTY"
  , "Ae2tdPwUPEZNCgK9K9CD9B6c1BcVMcJbSLhTBwNDWzhQ265zrYEjrV47eeW"
  , "Ae2tdPwUPEZ5PXtvRfwrrGa9ZGcmApTwTqvh58QTQANDX2ddLUcpTZnaHLo"
  , "Ae2tdPwUPEYzVh39uUKFBSubv4FGenCAEyV2BdKSwCADzVJYKEJVwPAUicj"
  , "Ae2tdPwUPEZCT2LnNBam5QjU6LE5VQRS7Z2JW1md69zMvu9y9WMnLwN3bX6"
  , "Ae2tdPwUPEZ8AFCshDagF6igZf2bHXixA1g5PdpRvn4KyTpG6zyMzky4ehh"
  , "Ae2tdPwUPEZ6nWqtXbKtchU3mpyRtrRZDt4obySFrrR85M4XcN74KTktXKv"
  , "Ae2tdPwUPEZMigfySnz9UFSmmMYvRUd2kPadT272pbbHotNVRp2scDyG2AK"
  , "Ae2tdPwUPEYxiwE99mBo8SkNPkzPEgrJmZpyXd9RuHWhpGKrSYaxUcKAbYQ"
  , "Ae2tdPwUPEZ9jpF2FAh8dxQ3BCWgG19ThVYPkEyMjhThvrhXx8ngBQeHhCQ"
  , "Ae2tdPwUPEZ82cmCBfjYq8iRzRWGgjMs7UkPypwp8LiSUJyMFEJGxBr2YKq"
  , "Ae2tdPwUPEZ1eMNrx76WA5JBwvxiHQWxM3tNYjpFDnJp9fgq86BHcxqSfN4"
  , "Ae2tdPwUPEZKJUFkpxqYrE32biZKQuqgWUdNKhFWbrGxJCnUNXVaxtQkErR"
  , "Ae2tdPwUPEYwAGnLtgusi3JKq4mvNqWvY9aztGtLwa22ko3HzUra3hjGXGx"
  , "Ae2tdPwUPEZ81XjXQAzpCj6QkV99kgkK46aS4J8xfppMi3R2Dpq4hhk7VNE"
  , "Ae2tdPwUPEZ7nPhRYqbcNaaif222Dp9rx998Q2YGYR2UNxw8qmNWwJ6daxo"
  , "Ae2tdPwUPEZ43xHeJbzVkx15t8qAhham5nt72JeK6XpXYvm68bfUHk6uVju"
  , "Ae2tdPwUPEZD45f87j3XvfwTWfTNgnz8QpnksffePU32ivaifqxcENuG6KK"
  , "Ae2tdPwUPEZF42GYPd3j7iw2cCUEMvirSk4vLPkTRdqqJtr4R4PsHSj4w2d"
  , "Ae2tdPwUPEYzyxBezBeDqDzfNQ3gzF27LVvAqETTsaw6kdJpTWHCgmPVEo2"
  , "Ae2tdPwUPEZGXRwDFR5VCmKCesFgBqgtrADgFo9FfjwSPEAyJvtVfh1JSmX"
  , "Ae2tdPwUPEZMYDvawa3S1DCA7eZdhrDFJMXHyh5hpxZJCQJD8c6ruBRanDJ"
  , "Ae2tdPwUPEZ8ffskBQYLzjPyqyxKsiNzYbvcJSN9JintHx6V6K1K8aEtho5"
  , "Ae2tdPwUPEZ8cmT88Unk2WD5YzUCcc8ifb3SzMQMpj5LS1QgRa7g6kez46h"
  , "Ae2tdPwUPEZGqtA4AbujDXkMH6zFZvTjUnRajLtwTCRV39EVdYtQJKrsc8u"
  , "Ae2tdPwUPEZ5oH337RvQhYkjaDjvZnK1PKD4tVsJsNKcBcGUWihgTsiVtde"
  , "Ae2tdPwUPEZAKA1vGHeZVpa3zhakExJ5utM9vwJ6auahoiCNFf6SufibHpC"
  , "Ae2tdPwUPEYxkHxX8KdWAPkfkTxa8kdNaZEo69baccQ7HpRfUUsELigZJf4"
  , "Ae2tdPwUPEZHajXavDF4CN4ExxHJUof8A2N2ugdEhv3LuPb76YmgUhxPu8R"
  , "Ae2tdPwUPEZGpXcqTCfq9KocPWYgVB234GRUdFVDhnxJ2H9stGrszkZJKTc"
  , "Ae2tdPwUPEZDVJUU3NfXH8di6D5E16djtgaFjWm8f81CEmoHUnMwMGGqbVj"
  , "Ae2tdPwUPEZAS8cHTvHVwgPoAC1dg9RdTx3nQVam8gNebLYwiy9YccQQuB1"
  , "Ae2tdPwUPEZ5hLgiaE7dzZuhqo68xZ7sMiqMGp39auHPcsE1VNNRvq7PnYN"
  , "Ae2tdPwUPEZAdY5hGCpQpxT2ReHdW8gd3A4h5CJsedt9SyQeUpHBzzcwjAt"
  , "Ae2tdPwUPEZ4afabfMLDJbX7Gaazj71zPpPrLeNywrv8uusU95bm21CBnwE"
  , "Ae2tdPwUPEZ7wwdAXP8z1hhMMWNrP9cc34eCFPbvEi5zFm6jDunvFq74WZe"
  , "Ae2tdPwUPEZMNyJAuNPb76ejraE3j3vQTup1xRxBHa5fKgzfznWbJijt5q2"
  , "Ae2tdPwUPEZHSzjcTUtJGNw5EcMtoYcEMpmdiPAMn1HVzy52WoTtRFpukws"
  , "Ae2tdPwUPEZMZLrkwBYumeF8P8eDPzRUWmW2epZRGRiGcvkhQptDFbujuQq"
  , "Ae2tdPwUPEZ56rfrz5TdFY1JHnCkTGMWRX4orh6Q1BMmTV5ATx7z4xbFfG7"
  , "Ae2tdPwUPEYyV78NYSddi6atWJgjWTpBHC3J1H2ceXzbDd5znBchmyp7sV3"
  , "Ae2tdPwUPEZ9jb4o5V26jQKbeDkppnJkgebXbWaabndYsRnXXYVb6weu2BP"
  , "Ae2tdPwUPEZHVs5JvSXmYxYvZGHZ8DHoM2zfJaiL99LkRbnvpH3oAVKuoS5"
  , "Ae2tdPwUPEZ967PQDmUALkQ7cEuuQVdCQp1iuUXnpbgE1kzamaBJ7qpqkwj"
  , "Ae2tdPwUPEZA8i4pSXDVJHTufffv59optZ9CFbfdUgJbHqUYbdx93N7ppV9"
  , "Ae2tdPwUPEYyDqAPnJ18XPaTE77vDAeuVa4Ytp7GBNe9PNvNLeLVBiM4jVL"
  , "Ae2tdPwUPEYw1wgtGgnoe2NbgfoFyxERny8qJM1vkqCXzkiXipJkJ7qvoR9"
  , "Ae2tdPwUPEZHKcKbatmsP23ACD6VVXiNa9czTngsBnHGT5dqqi233xVLcGs"
  , "Ae2tdPwUPEZEapggvTWfEx5jK1kkGVYMKeex7DcJVcTgmKxdcUnQXrDho2b"
  , "Ae2tdPwUPEZ1NPbZE91PQidZVBafLLco2YnpHdgwTxNPKgygXSwZVq4dgKB"
  , "Ae2tdPwUPEZLVnbtDRzNT1WmVfHTrkPs4JG38xNfmGkNWV9WgxYriy1qd6o"
  , "Ae2tdPwUPEZHUxRcryapNJoL8Fo6kMGFXsLQSLC3nmhbpz3M6RaT3CcfKrZ"
  , "Ae2tdPwUPEZ19YqjHnDr1yckaWEjwtZoaC3HZpVHepyzvcrVFtFoBUx4y1P"
  , "Ae2tdPwUPEYxdvmBHt6hD1ra9DwYMUed6VT3aB16DA8VZWGQvJyhd1MJSkE"
  , "Ae2tdPwUPEZ5grUgBooGGbBK9yHqdgVTdECqwS2XaeqG8boGBGqCA3nSBDi"
  , "Ae2tdPwUPEZLSj5xiNKzbZXQ2ZjKU4JLyfvf5E7dQLahcGZZg4QA7pNVZg2"
  , "Ae2tdPwUPEZHAvgfBNo8va259BSfq8nZpC7Lwp8jMJHkkUppMQnpRgPARaL"
  , "Ae2tdPwUPEZGNCsJF8xVNjHYAKDkyerXt2wCRexy7BFXcWvyiHFKSHTPJdF"
  , "Ae2tdPwUPEYzo3JzNowvs4gS69rZ3R5nT2KKZKWWxaymCufUsatVpu2kqii"
  , "Ae2tdPwUPEZFu8H46FK5q7g6ApMFAqpoYJJjmLyh8DheUL51i5dhbLcmSXG"
  , "Ae2tdPwUPEZ5fTgRDV736NaHHUAKaxj4ytyX1j7NLAtAF3x7gtUFGc2L8U3"
  , "Ae2tdPwUPEZCwt8ZP7R3wHB2Doed6neUHmhZYERTh3bsTQm6EfjFcfWmnTc"
  , "Ae2tdPwUPEZFQYXdB6V3wPfh99fDb8F3fXSvjVu7qBSjP8kVf81H2ApkaQu"
  , "Ae2tdPwUPEZEyVBVWrGSbQqrzQgNEdLexbUZJzqkF95Co3eESSVxerDdUfS"
  , "Ae2tdPwUPEYy6cvJ1mo5fBhYvP7r6RTpmxNGBgX8Cs4FC39eJr8DWYMd9vv"
  , "Ae2tdPwUPEZMQjnsmRoq1Vxb31PfLhxaBLsorC38QYj8Qbx9Afqg9DNeJhc"
  , "Ae2tdPwUPEZEpQ5obkgfFrjXk1GKnNBg7fkyjmNUhkH3vBxmZw7menySh28"
  , "Ae2tdPwUPEZ4hwGffsjLTTApiZEK1HgaVnndfJA1az5ToZNhiieXoskiixx"
  , "Ae2tdPwUPEZKzTzbEfDkNLvM3AfzMASBWmcSM9EU5aZ2iAAyuoyQd2gyNNN"
  , "Ae2tdPwUPEYyK9ph2bLu4GwopB38aUoHBDG2zDYGfdbZCEfYFXv6NDix979"
  , "Ae2tdPwUPEYy9WUnYWknL4SWq2nF8y2L7FngyhV6ftMEQYaTAtCxVjWHMjo"
  , "Ae2tdPwUPEZKgCUPxD5tSUDtgn3PiTfenMAFcTEBXsJqiESDmQnzxCVJj7B"
  , "Ae2tdPwUPEZ8uuaUYL4GD5uS5yiUTW6JYW54K258EGFyDeFK465fPXb2dsB"
  , "Ae2tdPwUPEZBhevhLwkd7maXseXHSfJMwgkNNraPnBXh1w86dChTRbDgrEr"
  , "Ae2tdPwUPEZLEdZb2Un8b2JLfRXzQi3cYbAtn4NG6SmLYiv1vxueuESNFVr"
  , "Ae2tdPwUPEYwpmuPpqUeqn2qTc3xEY6siqmTTaC6tn5S6fb45d8gz7Pdje3"
  , "Ae2tdPwUPEZCTzw5sgjL8X51m7Dg4xccizqJFRnrwyEWByTE4WTt1BnqtbA"
  , "Ae2tdPwUPEZ7tTXxGa4WfnGbN7qJu8gSRMmsjTDgNhz3qdCiuYC5N3ZMR12"
  , "Ae2tdPwUPEZ1UZJcQUs61oXayVvQVKAsry9oMMgDwSK9z2eMw8DibHsap1f"
  , "Ae2tdPwUPEYwJDXVgaPdZoFmDm2PcwqY67xBDpnj4z3UJmfR9dMD2XAfCjw"
  , "Ae2tdPwUPEZKr5rmjQY7aFHgEMAbMqtV38XtJCZtdNFKoiPVnWLnNDf4BGp"
  , "Ae2tdPwUPEYzSnRmYNX9GjEkhc1gXewiS2b3XQyMjztyiWrZiA6AdtWzpQ4"
  , "Ae2tdPwUPEZ4tThjhRaZZxAT1SNfRfB7yt9gYCysSamKkB7HUVH7NjkWxaA"
  , "Ae2tdPwUPEZ4msp1fbqK25ShSJ4BGYq6QbhBf4ALi3i17JS7KCx7gA8ksG8"
  , "Ae2tdPwUPEZGrBvM4Qr6wiWTMbJ7W46cMLWsenw3JQ9WvH7xwVnJTkL6n2Z"
  , "Ae2tdPwUPEZ9fUaqXRMUXhpwAqoGSaSXcrUGByyGyUnHokYH3dt2FBD8BLS"
  , "Ae2tdPwUPEZFbSUYiJG9oxa1U97ypoRHr7xg2PBhbXWShLRRU1Mav1tyYSw"
  , "Ae2tdPwUPEZJ6JcaDPLRZBNLyyB7QfN5sm1TGPpC8BCVF9eezeyRiPRXYHH"
  , "Ae2tdPwUPEZE5ZueRGyhkaW9qwWMiHYVM9uN8iTKYtTLoYoaEEU4djnKShk"
  , "Ae2tdPwUPEZJkqt5PS6o5myu5H15Gje6cPwJYXHN1ji4BzPiTKXzBvXjhWy"
  , "Ae2tdPwUPEZ1v2xoxVpm3pxFw5U6WuRV4Q3kdivrWF5cUhTVPgkBm8kMRvu"
  , "Ae2tdPwUPEZK1afLbsLTMb56F3MPCqqTq78ygzbZAamrExQMvSgyUT6jHPF"
  , "Ae2tdPwUPEZF2oYZxKaMntEh48gFqPKoGhjAaQwVNQMmUa695mhjQmebnkq"
  , "Ae2tdPwUPEZCsnxYXZfzXmbfuiBse9tTTimUuqEv4BRHjThCA4igaAfBmaN"
  , "Ae2tdPwUPEYw34SJK5vkreGkV9AUmMUB1pN9bcCjk8H3EVMbbw2PcjubFCq"
  , "Ae2tdPwUPEZLTWD9YuWFQTzLCZAbqnHwui8QSPPYAeNC7BobRVVajMsBgM1"
  , "Ae2tdPwUPEZ8UWnc14XpyhupmGrNk9QeguBfW8gzQ8WZ6PcUAtCgBdyCxsW"
  , "Ae2tdPwUPEYxzJRUWjG2e8FytD24VNa7FVYr4cdMmPBjoe3MCVVsvpHyh55"
  , "Ae2tdPwUPEZL14t4gybitgy6eHHogQUJS5pRH6P74fDeWuA8p76pMGnNBCR"
  , "Ae2tdPwUPEZM7EpvTXRV9ynN4mzoYFgG9xATWqEofbw2ZVK4AjALqaZxU3H"
  , "Ae2tdPwUPEZAXXviL2b9KNt6a5uHH5x6d3pzdPVCheXBRT81XrAKK2qMqtg"
  , "Ae2tdPwUPEZ3VrxgvtfBz2JXuszTPAKCLfapzcusf9zmxqWKxorW95QxEcR"
  , "Ae2tdPwUPEZ2t7h2auTtCbyoBk7uvroZQQ4ns5D6xoUAX83b72qqYJZDqgs"
  , "Ae2tdPwUPEZDpPM7EhAw1XVzRS52KHxASnkDceu6XTHuCJ3sPHFeCd6NDyZ"
  , "Ae2tdPwUPEZ73MuSt6NBpTSU4dzMpU2Lcd7jaKYnhfT4wS7udiB2ygy7znp"
  , "Ae2tdPwUPEZ3b8rdA63Qnvs6TGtmBaoNUXtf7vkYfUSf4iABUsWyFewiNav"
  , "Ae2tdPwUPEZHj8Kjyc4mbww3CRXBqjYhmKiXXyesGuCJZbffBFTyYWg54LE"
  , "Ae2tdPwUPEZMYomeS16gfhsV5UPuygbfPPRpMZiUwUmSxeHquue5VBiiXUs"
  , "Ae2tdPwUPEZ9TrvR9uzKnJZkxvPeTPMXB5EHkBhSb9odZa6z6RKKj3pSrrw"
  , "Ae2tdPwUPEZGAkywA1EDCnE5dTqKfx5Ngf6nbMbCmUWpRirKLv1Rp68eFwP"
  , "Ae2tdPwUPEZFjizwxcB6U2g5nwpkquqFQL78E7wq4mRp8JbQd3etaDyn1R3"
  , "Ae2tdPwUPEZ5Zznsim2RjRnDwo2CNQdTiQgKUWwED3v97qksmDnefKcGjwB"
  , "Ae2tdPwUPEZFAkbyARmyeFMR4c5yikc4AySUosnJWdw65FxJ6AsL7wh6XnJ"
  , "Ae2tdPwUPEYw7i4tXgdRBNAMVqTfskTUFTRYaVQoGyLnM87tXKuVodcUTmo"
  , "Ae2tdPwUPEZ7YLaEDbGKpWn6Ds5dRomUJ93aEF3Ptc6kkEq8Nxes118czAJ"
  , "Ae2tdPwUPEZ3pbYRkq3M3BDuLp5JLA5pBiT8diXZy8tec8FKtgdiQpS7eM2"
  , "Ae2tdPwUPEZ5kjhAsNtPK9sA4Kj8cLnmZV63RNGPXimMAPib3vPScuSRfFQ"
  , "Ae2tdPwUPEZAgEaoWowXz8w3K5agdtukBAYCpeR9o37e8rogzrhn8t8SDdi"
  , "Ae2tdPwUPEZMYomeS16gfhsV5UPuygbfPPRpMZiUwUmSxeHquue5VBiiXUs"
  , "Ae2tdPwUPEZ9TrvR9uzKnJZkxvPeTPMXB5EHkBhSb9odZa6z6RKKj3pSrrw"
  , "Ae2tdPwUPEZGAkywA1EDCnE5dTqKfx5Ngf6nbMbCmUWpRirKLv1Rp68eFwP"
  , "Ae2tdPwUPEZFjizwxcB6U2g5nwpkquqFQL78E7wq4mRp8JbQd3etaDyn1R3"
  , "Ae2tdPwUPEZ5Zznsim2RjRnDwo2CNQdTiQgKUWwED3v97qksmDnefKcGjwB"
  , "Ae2tdPwUPEZFAkbyARmyeFMR4c5yikc4AySUosnJWdw65FxJ6AsL7wh6XnJ"
  , "Ae2tdPwUPEYw7i4tXgdRBNAMVqTfskTUFTRYaVQoGyLnM87tXKuVodcUTmo"
  , "Ae2tdPwUPEZ7YLaEDbGKpWn6Ds5dRomUJ93aEF3Ptc6kkEq8Nxes118czAJ"
  , "Ae2tdPwUPEZ3pbYRkq3M3BDuLp5JLA5pBiT8diXZy8tec8FKtgdiQpS7eM2"
  , "Ae2tdPwUPEZ5kjhAsNtPK9sA4Kj8cLnmZV63RNGPXimMAPib3vPScuSRfFQ"
  , "Ae2tdPwUPEZAgEaoWowXz8w3K5agdtukBAYCpeR9o37e8rogzrhn8t8SDdi"
  , "Ae2tdPwUPEZGBDWYqP7EFf5xABUf48zeupxgQ5wcwyE4hnLqrWxwv4FKZ4H"
  , "Ae2tdPwUPEZHkJRxkXZw7LiwD36VbQcz6ezrh8NxMjF5YZDpk8y5T7AqkbN"
  , "Ae2tdPwUPEZLXBf4ZiyWdBnjVdJj4mq36KzW8LczBzaWysiLXqv5iEvH8a5"
  , "Ae2tdPwUPEZGfG3euqbHvWDx1amXpngGgnXeD1Xehfi6SsRvijRwmUQbVzG"
  , "Ae2tdPwUPEZ2d3hdaPhgAn4M2qQ1YwkVW1JR5fXBmZqjF67n8AEyXy699FN"
  , "Ae2tdPwUPEZNEuvLyVeVnzGqz8RZRqszCrJtkDzyFNEWYWbK1sJrkg2noyR"
  , "Ae2tdPwUPEZ3huRFSrKKUj6cxmjPdxzrE4QgL3FjMNkUyqsCp6rqg35JiZJ"
  , "Ae2tdPwUPEZKYLBpCCsCnzRRiLcJ9W3zktENcBhCPg3GDqy5vvF77RE8EQW"
  , "Ae2tdPwUPEZ8BPPnf5dgoj9RAPBqZkKD2BtLPXQs1NcaKfPJ9xpRFukcx2v"
  , "Ae2tdPwUPEZKd8dcsyY5NeW7rAgMwA7sUTDwmqieYgeZoExZvxbMPnQfVFp"
  , "Ae2tdPwUPEZLMpPv3SoyV5SPqcvE9wAdk9H5iTmksEAn2p21eXGqCFTutxX"
  , "Ae2tdPwUPEYxbWadLJR8sd9WyJGYMvk5aZ5yAprWgwbfmXEZqJNguFwzpMN"
  , "Ae2tdPwUPEZ4xsrAWyHz4nHgC5RoffZZxHApRtx815m3en8M1n7JXynwhWd"
  , "Ae2tdPwUPEZ49twXRg8MMnYeqTYbcZekaRDLEYqqzZN9zTJtvNz8n7USJc9"
  , "Ae2tdPwUPEZ1qkgyJ3RqTmdnBGrVUEq5uHcSPvz7rHM8xKfGk9ZEydny8kH"
  , "Ae2tdPwUPEZ3H5CCbDTs9hby6fE474QpHjaPFtRHtxQ3maG7fmav1b7nNjg"
  , "Ae2tdPwUPEZJ9V14gEp6fEY94RsP6DMwQAxCK31h4nFHqpJfXZ9gzdZZRGz"
  , "Ae2tdPwUPEZKaVojFd7YhtbPcgMWtUzA2xXeyww9WyfhksVw1QUFyCpR5sd"
  , "Ae2tdPwUPEZHy5iKqn68XqGAx7wx5tdHchkCS3QY7zrYmZ3EBm5hUwJSkUb"
  , "Ae2tdPwUPEZ7Wo53F3GTJ93YzeLoJMJpvXirkCQcwGQafJrpTRZ1UmgL7LR"
  , "Ae2tdPwUPEZ9YgYPcYWGxm992Rsj3HSeGi7DiKLGxUfyRuNrMKb2k5fKR56"
  , "Ae2tdPwUPEZKR5s691Hpn5TAWVxRTnHae7U6wLD9giUutRaGiXp39PbHnSV"
  , "Ae2tdPwUPEZHywzbLni3qBUV3mCfAsfgnCdK1pBTRht1Q79AzfUS4mJ161E"
  , "Ae2tdPwUPEZEUS1HZBW2WLibjrCQvSx8smr1UuQT86Wc7osVrAdkmMZwEkH"
  , "Ae2tdPwUPEZ2vwANf3pV4YX2q3JpP1jGozyToLgRJWJY7EU735uoach8iPE"
  , "Ae2tdPwUPEZM2zssBS1PM34jrJEvms6badKtKzVzUzL3p5PavuXna5jUzeu"
  , "Ae2tdPwUPEZBAwPn77EhvqdABbAeBLuknY98CHX5GqRZDxbrrYjAURjh5iA"
  , "Ae2tdPwUPEZGKHFUV3QgGyx6quKEQhjk3YacFMgZ6k39Zf6R9scN239rD7q"
  , "Ae2tdPwUPEZ9GFCNDtgbKEnbC3qBoBCFYyFLbJHNscGY5LgJMm8UMYzGkTh"
  , "Ae2tdPwUPEZN7UdsESqCofiHSJCBGzbW8hrXGtPjAdVyzDxyBMxUwKqFoYU"
  , "Ae2tdPwUPEZ4WcYSHRLwM7zPdh5z1pWYBFJAPD7NsRSPEWN12gmysETSGmX"
  , "Ae2tdPwUPEZNLpZzpi6raWCGgqxf9E5tGoYSWEpuRm4RM6bXsV3G4rUPF3G"
  , "Ae2tdPwUPEZ1J7zvE2ZC8WqCsijgQdm1ZUwkdLnRTBfXASKFou5L29NpLKs"
  , "Ae2tdPwUPEZ5L17NbihRn95WXSo4YBN7vv4FGdNA5X84mmbviGpM9Ma67aa"
  , "Ae2tdPwUPEYxPxoQL8DrcchoY2gsxeK8JX3RSYGCUBY4xZH7yAaPjXrexDt"
  , "Ae2tdPwUPEZG4V4GdZBd93TaVpQEcGNBuQAJSK2yGVQg4x4EwXZ9gU3oYQr"
  , "Ae2tdPwUPEZKxg6sc6eEjLyau3wTYnZaAmKVn9a3apPtEcrg7ibYZzQhfdt"
  , "Ae2tdPwUPEZEAQJxUj5Xkcukd5mvCwrMuicspyAiDuPkxA598NJGrpRdnG2"
  ]
  where
    unsafeDecodeAddr = either (error . show) id . decodeAddress @'W.Mainnet


-- | Allow running the test cluster a second time in the same process.
resetGlobals :: IO ()
resetGlobals = do
    void $ swapMVar faucetIndex 1

-- | A public stake key associated with a mnemonic that we pre-registered for
-- STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKey
    :: Aeson.Value
preRegisteredStakeKey = Aeson.object
    [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
    , "description" .= Aeson.String "Free form text"
    , "cborHex" .= Aeson.String
        "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
    ]

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 1_000_000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 1_000 * oneMillionAda

-- | Just one million Ada, in Lovelace.
oneMillionAda :: Integer
oneMillionAda = 1_000_000_000_000

-- | Add a @setupScribes[1].scMinSev@ field in a given config object.
-- The full lens library would be quite helpful here.
addMinSeverityStdout
    :: MonadFail m
    => Severity
    -> Aeson.Object
    -> m Aeson.Object
addMinSeverityStdout severity ob = case Aeson.lookup "setupScribes" ob of
    Just (Aeson.Array scribes) -> do
        let scribes' = Aeson.Array $ fmap setMinSev scribes
        pure $ Aeson.insert "setupScribes" scribes' ob
    _ -> fail "setupScribes logging config is missing or the wrong type"
  where
    sev = toJSON $ show severity
    setMinSev (Aeson.Object scribe)
        | Aeson.lookup "scKind" scribe == Just (Aeson.String "StdoutSK")
            = Aeson.Object (Aeson.insert "scMinSev" sev scribe)
        | otherwise = Aeson.Object scribe
    setMinSev a = a

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

-- | Hash a ByteString using blake2b_256 and encode it in base16
blake2b256S :: ByteString -> String
blake2b256S =
    T.unpack
    . T.decodeUtf8
    . convertToBase Base16
    . blake2b256

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgRegisteringStakePools Int -- ^ How many pools
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgRegisteringPoolMetadataInSMASH String String
    | MsgRegisteringPoolMetadata String String
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus Text ExitCode BL8.ByteString BL8.ByteString
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int BL8.ByteString
    | MsgSocketIsReady CardanoNodeConn
    | MsgStakeDistribution String ExitCode BL8.ByteString BL8.ByteString
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringPoolMetadata url hash -> T.pack $ unwords
            [ "Hosting metadata for pool using url"
            , url
            , "with hash"
            , hash
            ]
        MsgRegisteringPoolMetadataInSMASH pool hash -> T.pack $ unwords
            [ "Registering metadata for pool"
            , pool
            , "with SMASH with the metadata hash"
            , hash
            ]
        MsgRegisteringStakePools n -> mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for " <> T.pack fp
                <> " at " <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code -> "Failed " <> msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for " <> T.pack name <>
                ":\n" <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli":args)
      where
        indent = T.unlines . map ("  " <>) . T.lines . T.decodeUtf8With T.lenientDecode . BL8.toStrict

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Notice
        MsgRegisteringStakePools _ -> Notice
        MsgLauncher _ _ -> Info
        MsgStartedStaticServer _ _ -> Info
        MsgTempDir msg -> getSeverityAnnotation msg
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _-> Debug
        MsgCLIStatus _ (ExitFailure _) _ _-> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _-> Info
        MsgStakeDistribution _ (ExitFailure _) _ _-> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug
        MsgRegisteringPoolMetadataInSMASH{} -> Info
        MsgRegisteringPoolMetadata{} -> Info

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
