{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
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
    , Config (..)
    , ClusterEra (..)
    , FaucetFunds (..)

      -- * Node launcher
    , NodeParams (..)
    , singleNodeParams
    , RunningNode (..)

      -- * Cluster node launcher
    , defaultPoolConfigs
    , clusterEraFromEnv
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

import Cardano.Address
    ( Address (..) )
import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )
import Cardano.Api
    ( AsType (AsStakeKey, AsStakePoolKey)
    , File (..)
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
import Cardano.BM.Extra
    ( BracketLog, bracketTracer )
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
import Cardano.Ledger.Api
    ( StandardCrypto
    , ppA0L
    , ppDL
    , ppEMaxL
    , ppExtraEntropyL
    , ppKeyDepositL
    , ppMaxBBSizeL
    , ppMaxBHSizeL
    , ppMaxTxSizeL
    , ppMinFeeAL
    , ppMinFeeBL
    , ppMinPoolCostL
    , ppMinUTxOValueL
    , ppNOptL
    , ppPoolDepositL
    , ppProtocolVersionL
    , ppRhoL
    , ppTauL
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    , NonNegativeInterval
    , PositiveUnitInterval
    , StrictMaybe (..)
    , UnitInterval
    , boundRational
    , natVersion
    , textToUrl
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..), ShelleyGenesisStaking (sgsPools) )
import Cardano.Startup
    ( restrictFileMode )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode, unsafeFromHex )
import Cardano.Wallet.Util
    ( HasCallStack, mapFirst )
import Codec.Binary.Bech32
    ( dataPartFromBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Arrow
    ( first )
import Control.Lens
    ( over, set, (&), (.~), (<&>) )
import Control.Monad
    ( forM, forM_, liftM2, replicateM, replicateM_, void, when, (>=>) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Extra
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
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Char
    ( toLower )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast )
import Data.List
    ( intercalate, isSuffixOf, nub, permutations, sort )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.ListMap
    ( ListMap (..) )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Tagged
    ( Tagged (..), retag, untag )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Text.Encoding
    ( decodeUtf8 )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Word.Odd
    ( Word31 )
import GHC.Generics
    ( Generic )
import Network.URI
    ( URI, parseURI )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Directory
    ( copyFile
    , createDirectory
    , createDirectoryIfMissing
    , listDirectory
    , makeAbsolute
    )
import System.Environment
    ( getEnvironment, lookupEnv )
import System.Environment.Extended
    ( lookupEnvNonEmpty )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( (<.>), (</>) )
import System.IO.Temp.Extra
    ( TempDirLog )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process.Typed
    ( ProcessConfig, proc, readProcess, setEnv, setEnvInherit )
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

import qualified Cardano.Address as Address
import qualified Cardano.Codec.Cbor as CBOR
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Read as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.ListMap as ListMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml


newtype PoolId = PoolId {getPoolId :: ByteString}
    deriving stock (Generic, Eq, Show, Ord)

logFileConfigFromEnv
    :: Maybe String
    -- ^ Optional extra subdir for TESTS_LOGDIR. E.g. @Just "alonzo"@ and
    -- @Just "mary"@ to keep them separate.
    -> IO LogFileConfig
logFileConfigFromEnv subdir =
    LogFileConfig
        <$> nodeMinSeverityFromEnv
        <*> testLogDirFromEnv subdir
        <*> pure Info

-- | The lower-case names of all 'Severity' values.
loggingSeverities :: [(String, Severity)]
loggingSeverities = [(toLower <$> show s, s) | s <- [minBound .. maxBound]]

parseLoggingSeverity :: String -> Either String Severity
parseLoggingSeverity arg =
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> Left $ "unknown logging severity: " ++ arg

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var =
    lookupEnvNonEmpty var >>= \case
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

tokenMetadataServerFromEnv :: IO (Maybe URI)
tokenMetadataServerFromEnv =
    lookupEnv "TOKEN_METADATA_SERVER" <&> \case
        Nothing -> Nothing
        Just t -> parseURI t

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
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> Maybe CardanoNodeConn
    -- ^ optional cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigBase tr conn args = do
    traceWith tr (MsgCLI args)
    env <- getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c) : env
    let cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure $ cliEnv $ proc "cardano-cli" args

cliConfigNode
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> CardanoNodeConn
    -- ^ cardano-node socket path
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigNode tr conn = cliConfigBase tr (Just conn)

cliConfig
    :: Tracer IO ClusterLog
    -- ^ for logging the command
    -> [String]
    -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfig tr = cliConfigBase tr Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO ()
cli tr = cliConfig tr >=> void . readProcessStdoutOrFail

cliLine :: Tracer IO ClusterLog -> [String] -> IO String
cliLine tr =
    cliConfig tr
        >=> fmap (BL8.unpack . getFirstLine) . readProcessStdoutOrFail

cliEraFlag :: ClusterEra -> String
cliEraFlag era = "--" ++ clusterEraToString era ++ "-era"

readProcessStdoutOrFail :: ProcessConfig () () () -> IO BL.ByteString
readProcessStdoutOrFail processConfig = do
    (st, out, err) <- readProcess processConfig
    case st of
        ExitSuccess -> pure out
        ExitFailure _ ->
            throwIO
                $ userError
                $ mconcat
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
    -> Text
    -- ^ message to print before running command
    -> ProcessConfig () a b
    -> IO ()
cliRetry tr msg processConfig = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            throwIO
                $ ProcessHasExited
                    ("cardano-cli failed: " <> BL8.unpack err)
                    st
  where
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail :: (ExitCode, b, c) -> IO Bool
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- | The idea of what kind if pool we want to set up.
data PoolRecipe = PoolRecipe
    { pledgeAmt :: Integer
    , index :: Int
    , retirementEpoch :: Maybe Word31
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
    deriving stock (Eq, Show)

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a
         . NodeParams
        -> (RunningNode -> IO a)
        -> IO a
    -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
    -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: IO (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    , finalizeShelleyGenesisSetup :: RunningNode -> IO ()
    -- ^ Submit any pool retirement certificate according to the 'recipe'
    -- on-chain.
    , registerViaTx :: RunningNode -> IO ()
    }

data PoolMetadataServer = PoolMetadataServer
    { registerMetadataForPoolIndex :: Int -> Aeson.Value -> IO ()
    , urlFromPoolIndex :: Int -> String
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
        action
            $ PoolMetadataServer
                { registerMetadataForPoolIndex = \i metadata -> do
                    let metadataBytes = Aeson.encode metadata
                    BL8.writeFile (metadir </> (metadataFileName i)) metadataBytes
                    let hash = blake2b256 (BL.toStrict metadataBytes)
                    traceWith tr
                        $ MsgRegisteringPoolMetadata
                            (_urlFromPoolIndex i)
                            (B8.unpack $ convertToBase Base16 hash)
                , urlFromPoolIndex = _urlFromPoolIndex
                }
  where
    metadataFileName :: Int -> FilePath
    metadataFileName i = show i <> ".json"

configurePools
    :: Tracer IO ClusterLog
    -> Config
    -> PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> IO (NonEmpty ConfiguredPool)
configurePools tr config metadataServer =
    traverse (configurePool tr config metadataServer)

configurePool
    :: HasCallStack
    => Tracer IO ClusterLog
    -> Config
    -> PoolMetadataServer
    -> PoolRecipe
    -> IO ConfiguredPool
configurePool tr Config{..} metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    -- Use pool-specific dir
    let name = "pool-" <> show i
    let poolDir :: Tagged "pool" FilePath
        poolDir = Tagged $ untag cfgClusterDir </> name
    createDirectoryIfMissing False (untag poolDir)

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair tr poolDir
    (kesPrv, kesPub) <- genKesKeyPair tr poolDir
    (opPrv, opPub, opCount) <- writeOperatorKeyPair tr poolDir recipe
    opCert <- issueOpCert tr poolDir kesPub opPrv opCount
    let ownerPub = Tagged @"stake-pub" $ untag poolDir </> "stake.pub"
    let ownerPrv = Tagged @"stake-prv" $ untag poolDir </> "stake.prv"
    genStakeAddrKeyPair tr (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata

    pure
        ConfiguredPool
            { operatePool = \nodeParams action -> do
                let NodeParams genesisFiles hardForks (port, peers) logCfg =
                        nodeParams
                let logCfg' = setLoggingName name logCfg

                topology <-
                    genTopology (retag @"pool" @_ @"output" poolDir) peers
                withStaticServer (untag poolDir) $ \url -> do
                    traceWith tr $ MsgStartedStaticServer (untag poolDir) url

                    (config, genesisData, vd) <-
                        genNodeConfig
                            (retag @"pool" @_ @"output" poolDir)
                            cfgSetupDir
                            (Tagged @"node-name" mempty)
                            genesisFiles
                            hardForks
                            logCfg'

                    let cfg = CardanoNodeConfig
                            { nodeDir = untag @"pool" poolDir
                            , nodeConfigFile = untag @"node-config" config
                            , nodeTopologyFile = untag @"topology" topology
                            , nodeDatabaseDir = "db"
                            , nodeDlgCertFile = Nothing
                            , nodeSignKeyFile = Nothing
                            , nodeOpCertFile = Just opCert
                            , nodeKesKeyFile = Just $ untag @"kes-prv" kesPrv
                            , nodeVrfKeyFile = Just $ untag @"vrf-prv" vrfPrv
                            , nodePort = Just (NodePort port)
                            , nodeLoggingHostname = Just name
                            , nodeExecutable = Nothing
                            }

                    withCardanoNodeProcess tr name cfg $ \socket -> do
                        action $ RunningNode socket genesisData vd
            , registerViaShelleyGenesis = do
                poolId <- stakePoolIdFromOperatorVerKey opPub
                vrf <- poolVrfFromFile vrfPub
                stakePubHash <- stakingKeyHashFromFile ownerPub
                pledgeAddr <- stakingAddrFromVkFile ownerPub

                let params =
                        Ledger.PoolParams
                            { ppId = poolId
                            , ppVrf = vrf
                            , ppPledge = Ledger.Coin $ intCast pledgeAmt
                            , ppCost = Ledger.Coin 0
                            , ppMargin = unsafeUnitInterval 0.1
                            , ppRewardAcnt =
                                Ledger.RewardAcnt Mainnet
                                    $ Ledger.KeyHashObj stakePubHash
                            , ppOwners = Set.fromList [stakePubHash]
                            , ppRelays = mempty
                            , ppMetadata =
                                SJust
                                    $ Ledger.PoolMetadata
                                        ( fromMaybe (error "invalid url (too long)")
                                            $ textToUrl
                                            $ T.pack metadataURL
                                        )
                                        (blake2b256 (BL.toStrict metadataBytes))
                            }

                let updateStaking sgs =
                        sgs
                            { Ledger.sgsPools =
                                ListMap.ListMap [(poolId, params)] <> sgsPools sgs
                            , Ledger.sgsStake =
                                ListMap.fromList [(stakePubHash, poolId)]
                                    <> Ledger.sgsStake sgs
                            }
                let poolSpecificFunds =
                        ListMap.fromList
                            [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]

                pure
                    $ over #sgInitialFunds (poolSpecificFunds <>)
                        . over #sgStaking updateStaking
            , finalizeShelleyGenesisSetup = \(RunningNode socket _ _) -> do
                -- Here is our chance to respect the 'retirementEpoch' of
                -- the 'PoolRecipe'.
                --
                -- NOTE: We also submit the retirement cert in
                -- @registerViaTx@, but this seems to work regardless. (We
                -- do want to submit it here for the sake of babbage)
                let retire e = do
                        retCert <- issuePoolRetirementCert tr poolDir opPub e
                        (rawTx, faucetPrv) <-
                            preparePoolRetirement tr
                                poolDir cfgSetupDir cfgLastHardFork [retCert]
                        tx <- signTx tr
                            (retag @"pool" @_ @"output" poolDir)
                            (retag @"retirement-tx" @_ @"tx-body" rawTx)
                            [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
                            , retag @"stake-prv" @_ @"signing-key" ownerPrv
                            , retag @"op-prv" @_ @"signing-key" opPrv
                            ]
                        submitTx tr socket "retirement cert" tx

                traverse_ retire mretirementEpoch
            , registerViaTx = \(RunningNode socket _ _) -> do
                stakeCert <-
                    issueStakeVkCert tr
                    (retag @"cluster" @_ @"output" cfgClusterDir)
                    (Tagged @"prefix" "stake-pool")
                    ownerPub
                let poolRegistrationCert =
                        Tagged @"pool-reg-cert" $ untag poolDir </> "pool.cert"
                cli
                    tr
                    [ "stake-pool"
                    , "registration-certificate"
                    , "--cold-verification-key-file"
                    , untag opPub
                    , "--vrf-verification-key-file"
                    , untag vrfPub
                    , "--pool-pledge"
                    , show pledgeAmt
                    , "--pool-cost"
                    , "0"
                    , "--pool-margin"
                    , "0.1"
                    , "--pool-reward-account-verification-key-file"
                    , untag ownerPub
                    , "--pool-owner-stake-verification-key-file"
                    , untag ownerPub
                    , "--metadata-url"
                    , metadataURL
                    , "--metadata-hash"
                    , blake2b256S (BL.toStrict metadataBytes)
                    , "--mainnet"
                    , "--out-file"
                    , untag poolRegistrationCert
                    ]

                mPoolRetirementCert <- forM mretirementEpoch $ do
                    issuePoolRetirementCert tr poolDir opPub

                dlgCert <- issueDlgCert tr poolDir ownerPub opPub

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

                let certificates =
                        catMaybes
                            [ pure $ untag stakeCert
                            , pure $ untag poolRegistrationCert
                            , pure $ untag dlgCert
                            , untag <$> mPoolRetirementCert
                            ]
                (rawTx, faucetPrv) <-
                    preparePoolRegistration
                        tr
                        poolDir
                        cfgSetupDir
                        cfgLastHardFork
                        ownerPub
                        certificates
                        pledgeAmt
                tx <-
                    signTx tr
                        (retag @"pool" @_ @"output" poolDir)
                        (retag @"registration-tx" @_ @"tx-body" rawTx)
                        [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
                        , retag @"stake-prv" @_ @"signing-key" ownerPrv
                        , retag @"op-prv" @_ @"signing-key" opPrv
                        ]
                submitTx tr socket (Tagged @"name" name) tx
            , metadataUrl = T.pack metadataURL
            , recipe = recipe
            }

defaultPoolConfigs :: NonEmpty PoolRecipe
defaultPoolConfigs =
    NE.zipWith (\i p -> p{index = i}) (1 :| [2 ..])
        $
        -- This pool should never retire:
        PoolRecipe
            { pledgeAmt = 200 * millionAda
            , retirementEpoch = Nothing
            , poolMetadata =
                Aeson.object
                    [ "name" .= Aeson.String "Genesis Pool A"
                    , "ticker" .= Aeson.String "GPA"
                    , "description" .= Aeson.Null
                    , "homepage" .= Aeson.String "https://iohk.io"
                    ]
            , delisted = False
            , operatorKeys =
                ( PoolId
                    $ unsafeFromHex
                        "ec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9c2bbe6838dc6"
                , Aeson.object
                    [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                    , "description" .= Aeson.String "Stake pool operator key"
                    , "cborHex"
                        .= Aeson.String
                            "5820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                    ]
                , Aeson.object
                    [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                    , "description" .= Aeson.String "Stake pool operator key"
                    , "cborHex"
                        .= Aeson.String
                            "5820d8f81c455ef786f47ad9f573e49dc417e0125dfa8db986d6c0ddc03be8634dc6"
                    ]
                , Aeson.object
                    [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                    , "description" .= Aeson.String "Next certificate issue number: 0"
                    , "cborHex"
                        .= Aeson.String
                            "82005820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                    ]
                )
            , index = undefined
            }
            :|
            -- This pool should retire almost immediately:
            [ PoolRecipe
                { pledgeAmt = 100 * millionAda
                , retirementEpoch = Just 3
                , poolMetadata =
                    Aeson.object
                        [ "name" .= Aeson.String "Genesis Pool B"
                        , "ticker" .= Aeson.String "GPB"
                        , "description" .= Aeson.Null
                        , "homepage" .= Aeson.String "https://iohk.io"
                        ]
                , delisted = False
                , operatorKeys =
                    ( PoolId
                        $ unsafeFromHex
                            "1b3dc19c6ab89eaffc8501f375bb03c11bf8ed5d183736b1d80413d6"
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                        , "description" .= Aeson.String "Stake pool operator key"
                        , "cborHex"
                            .= Aeson.String
                                "5820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                        , "description" .= Aeson.String "Stake pool operator key"
                        , "cborHex"
                            .= Aeson.String
                                "5820fab9d94c52b3e222ed494f84020a29ef8405228d509a924106d05ed01c923547"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                        , "description" .= Aeson.String "Next certificate issue number: 0"
                        , "cborHex"
                            .= Aeson.String
                                "82005820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
                        ]
                    )
                , index = undefined
                }
            , -- This pool should retire, but not within the duration of a test run:
              PoolRecipe
                { pledgeAmt = 100 * millionAda
                , retirementEpoch = Just 100_000
                , poolMetadata =
                    Aeson.object
                        [ "name" .= Aeson.String "Genesis Pool C"
                        , "ticker" .= Aeson.String "GPC"
                        , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
                        , "homepage" .= Aeson.String "https://iohk.io"
                        ]
                , delisted = True
                , operatorKeys =
                    ( PoolId
                        $ unsafeFromHex
                            "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                        , "description" .= Aeson.String "Stake pool operator key"
                        , "cborHex"
                            .= Aeson.String
                                "5820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                        , "description" .= Aeson.String "Stake pool operator key"
                        , "cborHex"
                            .= Aeson.String
                                "5820047572e48be93834d6d7ddb01bb1ad889b4de5a7a1a78112f1edd46284250869"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                        , "description" .= Aeson.String "Next certificate issue number: 0"
                        , "cborHex"
                            .= Aeson.String
                                "82005820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                        ]
                    )
                , index = undefined
                }
            , -- This pool should retire, but not within the duration of a test run:
              PoolRecipe
                { pledgeAmt = 100 * millionAda
                , retirementEpoch = Just 1_000_000
                , poolMetadata =
                    Aeson.object
                        [ "name" .= Aeson.String "Genesis Pool D"
                        , "ticker" .= Aeson.String "GPD"
                        , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
                        , "homepage" .= Aeson.String "https://iohk.io"
                        ]
                , delisted = False
                , operatorKeys =
                    ( PoolId
                        $ unsafeFromHex
                            "bb114cb37d75fa05260328c235a3dae295a33d0ba674a5eb1e3e568e"
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                        , "description" .= Aeson.String "Stake Pool Operator Verification Key"
                        , "cborHex"
                            .= Aeson.String
                                "58203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                        , "description" .= Aeson.String "Stake Pool Operator Signing Key"
                        , "cborHex"
                            .= Aeson.String
                                "58208f50de27d74325eaf57767d70277210b31eb97cdc3033f632a9791a3677a64d2"
                        ]
                    , Aeson.object
                        [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                        , "description" .= Aeson.String "Next certificate issue number: 0"
                        , "cborHex"
                            .= Aeson.String
                                "820058203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                        ]
                    )
                , index = undefined
                }
            ]
  where
    millionAda = 1_000_000_000_000

data ClusterEra
    = ByronNoHardFork
    | ShelleyHardFork
    | AllegraHardFork
    | MaryHardFork
    | AlonzoHardFork
    | BabbageHardFork
    deriving stock (Show, Read, Eq, Ord, Bounded, Enum)

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
    MaryHardFork -> "mary"
    AlonzoHardFork -> "alonzo"
    BabbageHardFork -> "babbage"

data Config = Config
    { cfgStakePools :: NonEmpty PoolRecipe
    -- ^ Stake pools to register.
    , cfgLastHardFork :: ClusterEra
    -- ^ Which era to use.
    , cfgNodeLogging :: LogFileConfig
    -- ^ Log severity for node.
    , cfgClusterDir :: Tagged "cluster" FilePath
    -- ^ Root directory for cluster data.
    , cfgSetupDir :: Tagged "setup" FilePath
    -- ^ Directory containing data for cluster setup.
    } deriving stock (Show)

-- | Information about a launched node.
data RunningNode = RunningNode
    { runningNodeSocketPath :: CardanoNodeConn
    , runningNodeShelleyGenesis :: ShelleyGenesis StandardCrypto
    , runningNodeVersionData :: NodeToClientVersionData
    } deriving stock (Show, Eq)

unsafeUnitInterval :: HasCallStack => Rational -> UnitInterval
unsafeUnitInterval x =
    fromMaybe
        (error $ "unsafeUnitInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafeNonNegativeInterval :: HasCallStack => Rational -> NonNegativeInterval
unsafeNonNegativeInterval x =
    fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafePositiveUnitInterval :: HasCallStack => Rational -> PositiveUnitInterval
unsafePositiveUnitInterval x =
    fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

generateGenesis
    :: HasCallStack
    => Tagged "cluster" FilePath
    -> Tagged "setup" FilePath
    -> UTCTime
    -> [(Address, Coin)]
    -> (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    -- ^ For adding genesis pools and staking in Babbage and later.
    -> IO GenesisFiles
generateGenesis clusterDir setupDir systemStart initialFunds addPoolsToGenesis = do
    let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart
    let systemStart' = posixSecondsToUTCTime . fromRational . toRational $ startTime

    let pparams =
            Ledger.emptyPParams
                & ppMinFeeAL .~ Ledger.Coin 100
                & ppMinFeeBL .~ Ledger.Coin 100_000
                & ppMinUTxOValueL .~ Ledger.Coin 1_000_000
                & ppKeyDepositL .~ Ledger.Coin 1_000_000
                & ppPoolDepositL .~ Ledger.Coin 0
                & ppMaxBBSizeL .~ 239_857
                & ppMaxBHSizeL .~ 217_569
                & ppMaxTxSizeL .~ 16_384
                & ppMinPoolCostL .~ Ledger.Coin 0
                & ppExtraEntropyL .~ Ledger.NeutralNonce
                -- There are a few smaller features/fixes which are enabled based on
                -- the protocol version rather than just the era, so we need to
                -- set it to a realisitic value.
                & ppProtocolVersionL .~ Ledger.ProtVer (natVersion @8) 0
                -- Sensible pool & reward parameters:
                & ppNOptL .~ 3
                & ppRhoL .~ unsafeUnitInterval 0.178_650_067
                & ppTauL .~ unsafeUnitInterval 0.1
                & ppA0L .~ unsafeNonNegativeInterval 0.1
                & ppDL .~ unsafeUnitInterval 0
                -- The epoch bound on pool retirements specifies how many epochs
                -- in advance retirements may be announced. For testing purposes,
                -- we allow retirements to be announced far into the future.
                & ppEMaxL .~ 1_000_000

    let shelleyGenesisData = addPoolsToGenesis ShelleyGenesis
            { sgSystemStart = systemStart'
            , sgActiveSlotsCoeff = unsafePositiveUnitInterval 0.5
            , sgSlotLength = 0.5
            , sgSecurityParam = 10
            , sgEpochLength = 100
            , sgUpdateQuorum = 1
            , sgNetworkMagic = 764_824_073
            , sgSlotsPerKESPeriod = 86_400
            , sgMaxKESEvolutions = 5
            , sgNetworkId = Mainnet
            , sgMaxLovelaceSupply = 1_000_000_000_000_000_000
            , sgProtocolParams = pparams
            , sgInitialFunds = extraInitialFunds
            , sgStaking = Ledger.emptyGenesisStaking
            -- We need this to submit MIR certs
            -- (and probably for the BFT node pre-babbage):
            , sgGenDelegs =
                fromRight (error "invalid sgGenDelegs")
                    $ Aeson.eitherDecode
                    $ Aeson.encode
                        [aesonQQ|{
    "8ae01cab15f6235958b1147e979987bbdb90788f7c4e185f1632427a": {
        "delegate": "b7bf59bb963aa785afe220f5b0d3deb826fd0bcaeeee58cb81ab443d",
        "vrf": "4ebcf8b4c13c24d89144d72f544d1c425b4a3aa1ace30af4eb72752e75b40d3e"
    }
                        }|]
                            }

    let shelleyGenesis = untag clusterDir </> "genesis.shelley.json"
    Aeson.encodeFile shelleyGenesis shelleyGenesisData

    let yamlToAeson = Yaml.decodeFileThrow @_ @Aeson.Value

    let byronGenesis = untag clusterDir </> "genesis.byron.json"
    yamlToAeson (untag setupDir </> "byron-genesis.yaml")
        >>= withAddedKey "startTime" startTime
        >>= Aeson.encodeFile byronGenesis

    let alonzoGenesis = untag clusterDir </> "genesis.alonzo.json"
    yamlToAeson (untag setupDir </> "alonzo-genesis.yaml")
        >>= Aeson.encodeFile alonzoGenesis

    let conwayGenesis = untag clusterDir </> "genesis.conway.json"
    yamlToAeson (untag setupDir </> "conway-genesis.yaml")
        >>= Aeson.encodeFile conwayGenesis

    pure GenesisFiles{..}

  where
    extraInitialFunds ::
        HasCallStack => ListMap (Ledger.Addr StandardCrypto) Ledger.Coin
    extraInitialFunds =
        ListMap.fromList
            [ ( fromMaybe (error "extraFunds: invalid addr")
                    $ Ledger.deserialiseAddr $ unAddress address
              , Ledger.Coin $ intCast c
              )
            | (address, Coin c) <- initialFunds
            ]

data FaucetFunds = FaucetFunds
    { pureAdaFunds :: [(Address, Coin)]
    -- ^ Pure ada funds
    , maFunds :: [(Address, (TokenBundle, [(String, String)]))]
    -- ^ Multi asset funds. Slower to setup than pure ada funds.
    --
    -- Beside the assets, there is a list of
    -- @(signing key, verification key hash)@, so that they can be minted by
    -- the faucet.
    , mirFunds :: [(Credential, Coin)]
    -- ^ "Move instantaneous rewards" - for easily funding reward accounts.
    }
    deriving stock (Eq, Show)

instance Semigroup FaucetFunds where
    FaucetFunds ada1 ma1 mir1 <> FaucetFunds ada2 ma2 mir2 =
        FaucetFunds (ada1 <> ada2) (ma1 <> ma2) (mir1 <> mir2)

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
    :: HasCallStack
    => Tracer IO ClusterLog
    -> Config
    -> FaucetFunds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when all pools have started.
    -> IO a
withCluster tr config@Config{..} faucetFunds onClusterStart =
    bracketTracer' tr "withCluster" $ do
    let clusterDir = untag cfgClusterDir
    withPoolMetadataServer tr clusterDir $ \metadataServer -> do
        createDirectoryIfMissing True clusterDir
        traceWith tr $ MsgStartingCluster clusterDir
        resetGlobals

        systemStart <- getCurrentTime
        configuredPools <- configurePools tr config metadataServer cfgStakePools

        addGenesisPools <- do
            genesisDeltas <- mapM registerViaShelleyGenesis configuredPools
            pure $ foldr (.) id genesisDeltas
        let federalizeNetwork =
                over #sgProtocolParams (set ppDL (unsafeUnitInterval 0.25))

        faucetAddresses <- readFaucetAddresses cfgSetupDir

        genesisFiles <-
            generateGenesis
                cfgClusterDir
                cfgSetupDir
                systemStart
                (adaFunds <> map (,Coin 1_000_000_000_000_000) faucetAddresses)
                (if postAlonzo then addGenesisPools else federalizeNetwork)

        if postAlonzo
            then do
                port0 : ports <- rotate <$> randomUnusedTCPPorts nPools
                let pool0 :| otherPools = configuredPools

                let pool0Cfg =
                        NodeParams
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
                let bftCfg =
                        NodeParams
                            genesisFiles
                            cfgLastHardFork
                            (head ports)
                            cfgNodeLogging
                withBFTNode tr cfgClusterDir cfgSetupDir bftCfg $
                    \runningBFTNode -> do
                    extraClusterSetupUsingNode configuredPools runningBFTNode

                    -- NOTE: We used to perform 'registerViaTx' as part of
                    -- 'launchPools' where we waited for the pools to become
                    -- active (e.g. be in the stake distribution) in parallel.
                    -- Just submitting the registration certs in sequence
                    -- /seems/ to work though, and the setup working 100%
                    -- correctly in alonzo will soon not be important.
                    traverse_ (`registerViaTx` runningBFTNode) configuredPools
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
    extraClusterSetupUsingNode
        :: NonEmpty ConfiguredPool -> RunningNode -> IO ()
    extraClusterSetupUsingNode configuredPools runningNode = do
        let RunningNode conn _ _ = runningNode

        -- Needs to happen in the first 20% of the epoch, so we run this first.
        moveInstantaneousRewardsTo
            tr conn cfgClusterDir cfgSetupDir cfgLastHardFork mirFunds

        -- Submit retirement certs for all pools using the connection to
        -- the only running first pool to avoid the certs being rolled
        -- back.
        --
        -- We run these second in hope that it reduces the risk that any of the
        -- txs fail to make it on-chain. If this were to happen when running the
        -- integration tests, the integration tests /will fail/ (c.f. #3440).
        -- Later setup is less sensitive. Using a wallet with retrying
        -- submission pool might also be an idea for the future.
        when postAlonzo
            $ forM_ configuredPools
            $ \pool -> finalizeShelleyGenesisSetup pool runningNode

        let encodeMainnetShelleyAddr address =
                case CBOR.deserialiseCbor CBOR.decodeAddressPayload (unAddress address) of
                    Left _failure ->
                        Bech32.encodeLenient
                            [Bech32.humanReadablePart|addr|]
                            (dataPartFromBytes (unAddress address))
                    Right _payload ->
                        T.decodeUtf8 $ encodeBase58 bitcoinAlphabet (unAddress address)

        sendFaucetAssetsTo
            tr
            conn
            cfgClusterDir
            cfgSetupDir
            cfgLastHardFork
            20
            (first (T.unpack . encodeMainnetShelleyAddr) <$> maFunds)

        -- Should ideally not be hard-coded in 'withCluster'
        (rawTx, faucetPrv) <- prepareKeyRegistration tr config
        tx <- signTx tr
                (retag @"cluster" @_ @"output" cfgClusterDir)
                (retag @"reg-tx" @_ @"tx-body" rawTx)
                [retag @"faucet-prv" @_ @"signing-key" faucetPrv]
        submitTx tr conn "pre-registered stake key" tx

    -- \| Actually spin up the pools.
    launchPools :: HasCallStack => NonEmpty ConfiguredPool -> GenesisFiles
        -> [(Int, [Int])]
        -- @(port, peers)@ pairs availible for the nodes. Can be used to e.g.
        -- add a BFT node as extra peer for all pools.
        -> RunningNode
        -- \^ Backup node to run the action with in case passed no pools.
        -> (RunningNode -> IO a)
        -- \^ Action to run once when the stake pools are setup.
        -> IO a
    launchPools configuredPools genesisFiles ports fallbackNode action = do
        waitGroup <- newChan
        doneGroup <- newChan

        let poolCount = length configuredPools

        let waitAll = do
                traceWith tr
                    $ MsgDebug "waiting for stake pools to register"
                replicateM poolCount (readChan waitGroup)

        let onException :: SomeException -> IO ()
            onException e = do
                traceWith tr
                    $ MsgDebug
                    $ "exception while starting pool: "
                        <> T.pack (show e)
                writeChan waitGroup (Left e)

        let mkConfig (port, peers) =
                NodeParams
                    genesisFiles
                    cfgLastHardFork
                    (port, peers)
                    cfgNodeLogging
        asyncs <- forM (zip (NE.toList configuredPools) ports)
            $ \(configuredPool, (port, peers)) -> do
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
                throwIO
                    $ ProcessHasExited
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

    -- \| Get permutations of the size (n-1) for a list of n elements, alongside
    -- with the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: HasCallStack => Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap f . permutations
      where
        f = \case
            [] -> error "rotate: impossible"
            x : xs -> (x, sort xs)

data LogFileConfig = LogFileConfig
    { minSeverityTerminal :: Severity
    -- ^ Minimum logging severity
    , extraLogDir :: Maybe FilePath
    -- ^ Optional additional output to log file
    , minSeverityFile :: Severity
    -- ^ Minimum logging severity for 'extraLogFile'
    }
    deriving stock (Show)

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
    }
    deriving stock (Show)

singleNodeParams
    :: GenesisFiles
    -> Severity
    -> Maybe (FilePath, Severity)
    -> NodeParams
singleNodeParams genesisFiles severity extraLogFile =
    NodeParams genesisFiles maxBound (0, []) LogFileConfig
        { minSeverityTerminal = severity
        , extraLogDir = fmap fst extraLogFile
        , minSeverityFile = maybe severity snd extraLogFile
        }

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> Tagged "cluster" FilePath
    -- ^ Parent cluster state directory.
    -- Node data will be created in a subdirectory of this.
    -> Tagged "setup" FilePath
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr clusterDir setupDir params action = do
    let name = "bft"
    let NodeParams genesisFiles hardForks (port, peers) logCfg = params
    let nodeDir = untag clusterDir </> name
    bracketTracer' tr "withBFTNode" $ do
        createDirectoryIfMissing False nodeDir

        let copyKeyFile f = do
                let dst = nodeDir </> f
                copyFile (untag setupDir </> f) dst
                restrictFileMode dst
                pure dst

        [bftCert, bftPrv, vrfPrv, kesPrv, opCert] <-
            traverse (copyKeyFile . ("bft-leader" <>))
                [ ".byron.cert"
                , ".byron.skey"
                , ".vrf.skey"
                , ".kes.skey"
                , ".opcert"
                ]

        (config, genesisData, versionData) <-
            genNodeConfig
                (Tagged @"output" nodeDir)
                setupDir
                (Tagged @"node-name" "-bft")
                genesisFiles hardForks
                (setLoggingName name logCfg)

        topology <- genTopology (Tagged @"output" nodeDir) peers

        let nodeConfig = CardanoNodeConfig
                { nodeDir = nodeDir
                , nodeConfigFile = untag config
                , nodeTopologyFile = untag topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Just bftCert
                , nodeSignKeyFile = Just bftPrv
                , nodeOpCertFile = Just opCert
                , nodeKesKeyFile = Just kesPrv
                , nodeVrfKeyFile = Just vrfPrv
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                , nodeExecutable = Nothing
                }

        withCardanoNodeProcess tr name nodeConfig $ \socket ->
            action $ RunningNode socket genesisData versionData

-- | Launches a @cardano-node@ with the given configuration which will not forge
-- blocks, but has every other cluster node as its peer. Any transactions
-- submitted to this node will be broadcast to every node in the cluster.
--
-- FIXME: Do we really need the relay node? If so we should re-add it to
-- withCluster, rather than connecting the wallet to one of the pools.
_withRelayNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> Tagged "cluster" FilePath
    -- ^ Parent state directory.
    -- Node data will be created in a subdirectory of this.
    -> Tagged "setup" FilePath
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with socket path
    -> IO a
_withRelayNode tr clusterDir setupDir params act = do
    let name = "node"
    let nodeDir = Tagged @"output" $ untag clusterDir </> name
    let NodeParams genesisFiles hardForks (port, peers) logCfg = params
    bracketTracer' tr "withRelayNode" $ do
        createDirectory $ untag nodeDir

        let logCfg' = setLoggingName name logCfg
        (config, genesisData, vd) <-
            genNodeConfig
                nodeDir
                setupDir
                (Tagged @"node-name" "-relay")
                genesisFiles hardForks logCfg'
        topology <- genTopology nodeDir peers

        let cfg =
                CardanoNodeConfig
                    { nodeDir = untag nodeDir
                    , nodeConfigFile = untag config
                    , nodeTopologyFile = untag topology
                    , nodeDatabaseDir = "db"
                    , nodeDlgCertFile = Nothing
                    , nodeSignKeyFile = Nothing
                    , nodeOpCertFile = Nothing
                    , nodeKesKeyFile = Nothing
                    , nodeVrfKeyFile = Nothing
                    , nodePort = Just (NodePort port)
                    , nodeLoggingHostname = Just name
                    , nodeExecutable = Nothing
                    }

        let act' socket = act $ RunningNode socket genesisData vd
        withCardanoNodeProcess tr name cfg act'

toTextPoolId :: PoolId -> Text
toTextPoolId =
    decodeUtf8
        . convertToBase Base16
        . getPoolId

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
            poolDir = metadataDir </> T.unpack (toTextPoolId poolId)
            hash = blake2b256S (BL.toStrict bytes)
            hashFile = poolDir </> hash

        traceWith tr
            $ MsgRegisteringPoolMetadataInSMASH
                (T.unpack $ toTextPoolId poolId)
                hash

        createDirectoryIfMissing True poolDir
        BL8.writeFile (poolDir </> hashFile) bytes

    -- Write delisted pools
    let toSmashId = T.pack . B8.unpack . convertToBase Base16 . getPoolId
    let poolId (PoolRecipe _ _ _ _ (pid, _, _, _) _) = toSmashId pid
    let delistedPoolIds = poolId <$> NE.filter delisted defaultPoolConfigs
    BL8.writeFile
        (baseDir </> "delisted")
        ( Aeson.encode
            $ delistedPoolIds
            <&> \p -> object ["poolId" Aeson..= p]
        )

    -- health check
    let health =
            Aeson.encode
                $ object
                    [ "status" Aeson..= ("OK" :: Text)
                    , "version" Aeson..= ("1.2.0" :: Text)
                    ]
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
    throwErrs :: Either ProcessHasExited a -> IO a
    throwErrs = either throwIO pure

setLoggingName :: String -> LogFileConfig -> LogFileConfig
setLoggingName name cfg = cfg{extraLogDir = filename <$> extraLogDir cfg}
  where
    filename = (</> (name <.> "log"))

data GenesisFiles = GenesisFiles
    { byronGenesis :: FilePath
    , shelleyGenesis :: FilePath
    , alonzoGenesis :: FilePath
    , conwayGenesis :: FilePath
    }
    deriving stock (Show, Eq)

genNodeConfig
    :: Tagged "output" FilePath
    -- ^ A top-level directory where to put the configuration.
    -> Tagged "setup" FilePath
    -> Tagged "node-name" String -- Node name
    -> GenesisFiles
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> IO
        ( Tagged "node-config" FilePath
        , ShelleyGenesis StandardCrypto
        , NodeToClientVersionData
        )
genNodeConfig poolDir setupDir name genesisFiles clusterEra logCfg = do
    let LogFileConfig severity mExtraLogFile extraSev = logCfg
    let GenesisFiles
            { byronGenesis
            , shelleyGenesis
            , alonzoGenesis
            , conwayGenesis
            } = genesisFiles

    let fileScribe (path, sev) =
            ScribeDefinition
                { scName = path
                , scFormat = ScText
                , scKind = FileSK
                , scMinSev = sev
                , scMaxSev = Critical
                , scPrivacy = ScPublic
                , scRotation = Nothing
                }

    let scribes =
            map fileScribe
                $ catMaybes
                    [ Just ("cardano-node.log", severity)
                    , (,extraSev) . T.pack <$> mExtraLogFile
                    ]

    let poolNodeConfig = untag poolDir </> ("node" <> untag name <> ".config")

    Yaml.decodeFileThrow (untag setupDir </> "node.config")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesis
        >>= withAddedKey "ByronGenesisFile" byronGenesis
        >>= withAddedKey "AlonzoGenesisFile" alonzoGenesis
        >>= withAddedKey "ConwayGenesisFile" conwayGenesis
        >>= withHardForks clusterEra
        >>= withAddedKey "minSeverity" Debug
        >>= withScribes scribes
        >>= withObject (addMinSeverityStdout severity)
        >>= Yaml.encodeFile poolNodeConfig

    -- Parameters
    genesisData <- Yaml.decodeFileThrow shelleyGenesis
    let networkMagic = NetworkMagic $ sgNetworkMagic genesisData
    pure
        ( Tagged @"node-config" poolNodeConfig
        , genesisData
        , NodeToClientVersionData{networkMagic, query = False}
        )
  where
    withScribes :: [ScribeDefinition] -> Yaml.Value -> IO Yaml.Value
    withScribes scribes =
        withAddedKey "setupScribes" scribes
            >=> withAddedKey
                "defaultScribes"
                (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    withHardForks :: ClusterEra -> Yaml.Value -> IO Yaml.Value
    withHardForks era =
        withObject (pure . Aeson.union (Aeson.fromList hardForks))
      where
        hardForks =
            [ ( Aeson.fromText $ "Test" <> T.pack (show hardFork) <> "AtEpoch"
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
genTopology
    :: Tagged "output" FilePath -> [Int] -> IO (Tagged "topology" FilePath)
genTopology outputDir peers = do
    let file = untag outputDir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object ["Producers" .= map encodePeer peers]
    pure $ Tagged @"topology" file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]

-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> PoolRecipe
    -> IO
        ( Tagged "op-prv" FilePath
        , Tagged "op-pub" FilePath
        , Tagged "op-cnt" FilePath
        )
writeOperatorKeyPair tr poolDir recipe = do
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceWith tr $ MsgGenOperatorKeyPair $ untag poolDir

    let opPub = untag poolDir </> "op.pub"
    let opPrv = untag poolDir </> "op.prv"
    let opCount = untag poolDir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure
        ( Tagged @"op-prv" opPrv
        , Tagged @"op-pub" opPub
        , Tagged @"op-cnt" opCount
        )

-- | Create a key pair for a node KES operational key
genKesKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> IO (Tagged "kes-prv" FilePath, Tagged "kes-pub" FilePath)
genKesKeyPair tr poolDir = do
    let kesPrv = Tagged @"kes-prv" $ untag poolDir </> "kes.prv"
    let kesPub = Tagged @"kes-pub" $ untag poolDir </> "kes.pub"
    cli
        tr
        [ "node"
        , "key-gen-KES"
        , "--verification-key-file"
        , untag kesPub
        , "--signing-key-file"
        , untag kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> IO (Tagged "vrf-prv" FilePath, Tagged "vrf-pub" FilePath)
genVrfKeyPair tr poolDir = do
    let vrfPrv = Tagged @"vrf-prv" $ untag poolDir </> "vrf.prv"
    let vrfPub = Tagged @"vrf-pub" $ untag poolDir </> "vrf.pub"
    cli
        tr
        [ "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , untag vrfPub
        , "--signing-key-file"
        , untag vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair
    :: Tracer IO ClusterLog
    -> (Tagged "stake-prv" FilePath, Tagged "stake-pub" FilePath)
    -> IO ()
genStakeAddrKeyPair tr (stakePrv, stakePub) = do
    cli
        tr
        [ "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , untag stakePub
        , "--signing-key-file"
        , untag stakePrv
        ]

-- | Issue a node operational certificate
issueOpCert
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "kes-pub" FilePath
    -> Tagged "op-prv" FilePath
    -> Tagged "op-cnt" FilePath
    -> IO FilePath
issueOpCert tr nodeDir kesPub opPrv opCount = do
    let file = untag nodeDir </> "op.cert"
    cli
        tr
        [ "node"
        , "issue-op-cert"
        , "--kes-verification-key-file"
        , untag kesPub
        , "--cold-signing-key-file"
        , untag opPrv
        , "--operational-certificate-issue-counter-file"
        , untag opCount
        , "--kes-period"
        , "0"
        , "--out-file"
        , file
        ]
    pure file

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -> Tagged "prefix" String
    -> Tagged "stake-pub" FilePath
    -> IO (Tagged "stake-vk-cert" FilePath)
issueStakeVkCert tr outputDir prefix stakePub = do
    let file = untag outputDir </> untag prefix <> "-stake.cert"
    cli
        tr
        [ "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , untag stakePub
        , "--out-file"
        , file
        ]
    pure $ Tagged file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -> Tagged "prefix" String
    -> FilePath
    -> IO (Tagged "stake-script-cert" FilePath)
issueStakeScriptCert tr outputDir prefix stakeScript = do
    let file = untag outputDir </> untag prefix <> "-stake.cert"
    cli
        tr
        [ "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , stakeScript
        , "--out-file"
        , file
        ]
    pure $ Tagged file

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => Tagged "op-pub" FilePath
    -> IO (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakePoolKey
                (VerificationKeyFilePath $ File $ untag opPub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => Tagged "vrf-pub" FilePath
    -> IO (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsVrfKey
                (VerificationKeyFilePath $ File $ untag vrfPub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> IO (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakeKey
                (VerificationKeyFilePath $ File $ untag stakePub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> IO (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile stakePub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakeKey
                (VerificationKeyFilePath $ File $ untag stakePub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH = either (error . show) snd
            $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH = either (error . show) snd
            $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    pure
        $ Ledger.Addr
            Mainnet
            (Ledger.KeyHashObj payKH)
            (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

issuePoolRetirementCert
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "op-pub" FilePath
    -> Word31
    -> IO (Tagged "retirement-cert" FilePath)
issuePoolRetirementCert tr poolDir opPub retirementEpoch = do
    let file = untag poolDir </> "pool-retirement.cert"
    cli
        tr
        [ "stake-pool"
        , "deregistration-certificate"
        , "--cold-verification-key-file"
        , untag opPub
        , "--epoch"
        , show retirementEpoch
        , "--out-file"
        , file
        ]
    pure $ Tagged @"retirement-cert" file

-- | Create a stake address delegation certificate.
issueDlgCert
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "stake-pub" FilePath
    -> Tagged "op-pub" FilePath
    -> IO (Tagged "stake-addr-deleg-cert" FilePath)
issueDlgCert tr poolDir stakePub opPub = do
    let file = untag poolDir </> "dlg.cert"
    cli
        tr
        [ "stake-address"
        , "delegation-certificate"
        , "--staking-verification-key-file"
        , untag stakePub
        , "--stake-pool-verification-key-file"
        , untag opPub
        , "--out-file"
        , file
        ]
    pure $ Tagged file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
preparePoolRegistration
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> Tagged "stake-pub" FilePath
    -> [FilePath]
    -> Integer
    -> IO (Tagged "registration-tx" FilePath, Tagged "faucet-prv" FilePath)
preparePoolRegistration tr poolDir setupDir era stakePub certs pledgeAmt = do
    let file = untag poolDir </> "tx.raw"
    addr <-
        genSinkAddress tr (retag @"pool" @_ @"output" poolDir) (Just stakePub)
    (faucetInput, faucetPrv) <- takeFaucet setupDir
    cli tr
        $ [ "transaction"
          , "build-raw"
          , cliEraFlag era
          , "--tx-in"
          , untag faucetInput
          , "--tx-out"
          , addr <> "+" <> show pledgeAmt
          , "--ttl"
          , "400"
          , "--fee"
          , show (faucetAmt - pledgeAmt - depositAmt)
          , "--out-file"
          , file
          ]
            ++ mconcat ((\cert -> ["--certificate-file", cert]) <$> certs)

    pure (Tagged file, faucetPrv)

preparePoolRetirement
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> [Tagged "retirement-cert" FilePath]
    -> IO (Tagged "retirement-tx" FilePath, Tagged "faucet-prv" FilePath)
preparePoolRetirement tr poolDir setupDir era certs = do
    let file = untag poolDir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet setupDir
    cli tr
        $ [ "transaction"
          , "build-raw"
          , cliEraFlag era
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "400"
          , "--fee"
          , show faucetAmt
          , "--out-file"
          , file
          ]
            ++ mconcat ((\cert -> ["--certificate-file", untag cert]) <$> certs)

    pure (Tagged file, faucetPrv)

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -> IO (String, (String, String))
genMonetaryPolicyScript tr outputDir = do
    let policyPub = untag outputDir </> "policy.pub"
    let policyPrv = untag outputDir </> "policy.prv"

    cli
        tr
        [ "address"
        , "key-gen"
        , "--verification-key-file"
        , policyPub
        , "--signing-key-file"
        , policyPrv
        ]
    skey <- T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <-
        cliLine
            tr
            [ "address"
            , "key-hash"
            , "--payment-verification-key-file"
            , policyPub
            ]
    script <- writeMonetaryPolicyScriptFile outputDir vkeyHash
    policyId <-
        cliLine
            tr
            [ "transaction"
            , "policyid"
            , "--script-file"
            , untag script
            ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: Tagged "output" FilePath
    -- ^ Destination directory for script file
    -> String
    -- ^ The script verification key hash
    -> IO (Tagged "policy-script" FilePath)
    -- ^ Returns the filename written
writeMonetaryPolicyScriptFile outputDir keyHash = do
    let scriptFile = untag outputDir </> keyHash <.> "script"
    Aeson.encodeFile scriptFile
        $ object
            [ "type" .= Aeson.String "sig"
            , "keyHash" .= keyHash
            ]
    pure $ Tagged scriptFile

writePolicySigningKey
    :: Tagged "output" FilePath
    -- ^ destination directory for key file
    -> String
    -- ^ Name of file, keyhash perhaps.
    -> String
    -- ^ The cbor-encoded key material, encoded in hex
    -> IO (Tagged "policy-signing-key" FilePath)
    -- ^ Returns the filename written
writePolicySigningKey outputDir keyHash cborHex = do
    let keyFile = untag outputDir </> keyHash <.> "skey"
    Aeson.encodeFile keyFile
        $ object
            [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
            , "description" .= Aeson.String "Payment Signing Key"
            , "cborHex" .= cborHex
            ]
    pure $ Tagged keyFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: FilePath -> IO Text
readKeyFromFile f = do
    textView <- either throwString pure =<< Aeson.eitherDecodeFileStrict' f
    either throwString pure
        $ Aeson.parseEither
            (Aeson.withObject "TextView" (.: "cborHex"))
            textView

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> Tagged "cluster" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn clusterDir setupDir era targets =
    batch 80 targets
        $ sendFaucet tr conn clusterDir setupDir era "ada" . map coinBundle
  where
    coinBundle :: (String, Coin) -> (String, (TokenBundle, [a]))
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> Tagged "cluster" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> Int
    -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))]
    -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn clusterDir setupDir era batchSize targets =
    when (era >= MaryHardFork)
        $ batch batchSize targets
        $ sendFaucet tr conn clusterDir setupDir era "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: HasCallStack
    => Tracer IO ClusterLog
    -> CardanoNodeConn
    -> Tagged "cluster" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> String
    -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn clusterDir setupDir era what targets = do
    (faucetInput, faucetPrv) <- takeFaucet setupDir
    let file = untag clusterDir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords
                $ [addr, show c, "lovelace"]
                    ++ map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) =
            mconcat
                [ T.unpack (toText pid)
                , if B8.null name then "" else "."
                , B8.unpack (convertToBase Base16 name)
                ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total =
            fromIntegral
                $ sum
                $ map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets
    let outputDir = retag @"cluster" @_ @"output" clusterDir

    scripts <-
        forM (nub $ concatMap (map snd . snd . snd) targets)
            $ writeMonetaryPolicyScriptFile outputDir

    cli tr
        $ [ "transaction"
          , "build-raw"
          , cliEraFlag era
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "6000000"
          , -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
            "--fee"
          , show (faucetAmt - total)
          , "--out-file"
          , file
          ]
            ++ concatMap (uncurry mkOutput . fmap fst) targets
            ++ mkMint targetAssets
            ++ (concatMap (\f -> ["--minting-script-file", untag f]) scripts)

    policyKeys <-
        forM (nub $ concatMap (snd . snd) targets) $ \(skey, keyHash) ->
            writePolicySigningKey outputDir keyHash skey

    tx <- signTx tr outputDir
        (Tagged @"tx-body" file)
        (retag @"faucet-prv" @_ @"signing-key" faucetPrv : map retag policyKeys)
    submitTx tr conn (Tagged @"name" $ what ++ " faucet tx") tx

batch :: HasCallStack => Int -> [a] -> ([a] -> IO b) -> IO ()
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
    deriving stock (Eq, Show)

moveInstantaneousRewardsTo
    :: HasCallStack
    => Tracer IO ClusterLog
    -> CardanoNodeConn
    -> Tagged "cluster" FilePath
    -> Tagged "setup" FilePath
    -> ClusterEra
    -> [(Credential, Coin)]
    -> IO ()
moveInstantaneousRewardsTo tr conn clusterDir setupDir era targets = do
    let outputDir = retag @"cluster" @_ @"output" clusterDir
    certs <- mapM (mkCredentialCerts outputDir) targets
    (faucetInput, faucetPrv) <- takeFaucet setupDir
    let txFile = untag clusterDir </> "mir-tx.raw"

    let total = sum $ map (Coin.toInteger . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $
        error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress tr (retag @"cluster" @_ @"output" clusterDir) Nothing

    cli tr
        $ [ "transaction"
          , "build-raw"
          , cliEraFlag era
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "999999999"
          , "--fee"
          , show (faucetAmt - 1_000_000 - totalDeposit)
          , "--tx-out"
          , sink <> "+" <> "1000000"
          , "--out-file"
          , txFile
          ]
            ++ concatMap (\x -> ["--certificate-file", untag x]) (mconcat certs)

    let bftPrv = untag setupDir </> "bft-leader" <> ".skey"

    submitTx tr conn "MIR certificates"
        =<< signTx tr outputDir (Tagged @"tx-body" txFile)
            [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
            , Tagged @"signing-key" bftPrv
            ]
  where
    mkCredentialCerts
        :: Tagged "output" FilePath
        -> (Credential, Coin)
        -> IO [Tagged "cert" FilePath]
    mkCredentialCerts outputDir = \case
        (KeyCredential xpub, coin) -> do
            (prefix, vkFile) <- mkVerificationKey xpub
            stakeAddr <-
                cliLine
                    tr
                    [ "stake-address"
                    , "build"
                    , "--mainnet"
                    , "--stake-verification-key-file"
                    , vkFile
                    ]
            stakeCert <-
                issueStakeVkCert tr outputDir prefix (Tagged @"stake-pub" vkFile)
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [retag stakeCert, retag mirCert]
        (ScriptCredential script, coin) -> do
            (prefix, scriptFile) <- mkScript script
            -- NOTE: cardano-cli does not support creating stake-address from
            -- scripts just yet... So it's a bit ugly, but we create a stake
            -- address by creating a standard address, and replacing the header.
            stakeAddr <-
                toStakeAddress
                    <$> cliLine
                        tr
                        [ "address"
                        , "build"
                        , "--mainnet"
                        , "--payment-script-file"
                        , scriptFile
                        ]
            stakeCert <- issueStakeScriptCert tr outputDir prefix scriptFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [retag stakeCert, retag mirCert]
      where
        toStakeAddress =
            T.unpack
                . Bech32.encodeLenient hrp
                . Bech32.dataPartFromBytes
                . BL.toStrict
                . BL.pack
                . mapFirst (240 .|.)
                . BL.unpack
                . unsafeBech32Decode
                . T.pack
          where
            hrp = [humanReadablePart|stake|]

    mkVerificationKey :: XPub -> IO (Tagged "prefix" String, FilePath)
    mkVerificationKey xpub = do
        let base16 =
                T.unpack . T.decodeUtf8 . convertToBase Base16
                    $ xpubPublicKey xpub
        let json =
                Aeson.object
                    [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
                    , "description" .= Aeson.String "Stake Verification Key"
                    , "cborHex" .= Aeson.String ("5820" <> T.pack base16)
                    ]
        let file = untag clusterDir </> base16 <> ".vk"
        BL8.writeFile file (Aeson.encode json)
        pure (Tagged base16, file)

    mkScript :: ByteString -> IO (Tagged "prefix" String, FilePath)
    mkScript bytes = do
        let base16 =
                T.decodeUtf8 . convertToBase Base16
                    $ CBOR.toStrictByteString $ CBOR.encodeBytes bytes
        let json =
                Aeson.object
                    [ "type" .= Aeson.String "PlutusScriptV1"
                    , "description" .= Aeson.String ""
                    , "cborHex" .= Aeson.String base16
                    ]
        let prefix = take 100 (T.unpack base16)
        let file = untag clusterDir </> prefix <> ".plutus"
        BL8.writeFile file (Aeson.encode json)
        pure (Tagged prefix, file)

    mkMIRCertificate :: (String, Coin) -> IO (Tagged "mir-cert" FilePath)
    mkMIRCertificate (stakeAddr, Coin reward) = do
        let mirCert = untag clusterDir </> stakeAddr <> ".mir"
        cli
            tr
            [ "governance"
            , "create-mir-certificate"
            , "--reserves"
            , "--reward"
            , show reward
            , "--stake-address"
            , stakeAddr
            , "--out-file"
            , mirCert
            ]
        pure $ Tagged @"mir-cert" mirCert

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Tracer IO ClusterLog
    -> Config
    -> IO (Tagged "reg-tx" FilePath, Tagged "faucet-prv" FilePath)
prepareKeyRegistration tr Config{..} = do
    let outputDir = retag @"cluster" @_ @"output" cfgClusterDir
    let file = untag cfgClusterDir </> "tx.raw"
    let stakePub = Tagged @"stake-pub"
            $ untag cfgClusterDir </> "pre-registered-stake.pub"
    Aeson.encodeFile (untag stakePub) preRegisteredStakeKey
    (faucetInput, faucetPrv) <- takeFaucet cfgSetupDir
    cert <- issueStakeVkCert
        tr outputDir (Tagged @"prefix" "pre-registered") stakePub
    sink <- genSinkAddress tr outputDir Nothing
    cli
        tr
        [ "transaction"
        , "build-raw"
        , cliEraFlag cfgLastHardFork
        , "--tx-in"
        , untag faucetInput
        , "--tx-out"
        , sink <> "+" <> "1000000"
        , "--ttl"
        , "400"
        , "--fee"
        , show (faucetAmt - depositAmt - 1_000_000)
        , "--certificate-file"
        , untag cert
        , "--out-file"
        , file
        ]
    pure (Tagged @"reg-tx" file, faucetPrv)

genSinkAddress
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -- ^ Directory to put keys
    -> Maybe (Tagged "stake-pub" FilePath)
    -- ^ Stake pub
    -> IO String
genSinkAddress tr outputDir stakePub = do
    let sinkPrv = untag outputDir </> "sink.prv"
    let sinkPub = untag outputDir </> "sink.pub"
    cli
        tr
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , sinkPrv
        , "--verification-key-file"
        , sinkPub
        ]
    cliLine tr
        $ [ "address"
          , "build"
          , "--mainnet"
          , "--payment-verification-key-file"
          , sinkPub
          ] ++
          case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", untag key]

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -- ^ Output directory
    -> Tagged "tx-body" FilePath
    -- ^ Tx body file
    -> [Tagged "signing-key" FilePath]
    -- ^ Signing keys for witnesses
    -> IO (Tagged "tx-signed" FilePath)
signTx tr outputDir rawTx keys = do
    let file = untag outputDir </> "tx.signed"
    cli tr
        $ [ "transaction"
          , "sign"
          , "--tx-body-file"
          , untag rawTx
          , "--mainnet"
          , "--out-file"
          , file
          ]
            ++ concatMap (\key -> ["--signing-key-file", untag key]) keys
    pure $ Tagged @"tx-signed" file

-- | Submit a transaction through a running node.
submitTx
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> Tagged "name" String
    -> Tagged "tx-signed" FilePath
    -> IO ()
submitTx tr conn name signedTx =
    cliRetry tr ("Submitting transaction for " <> T.pack (untag name))
        =<< cliConfigNode
            tr
            conn
            [ "transaction"
            , "submit"
            , "--tx-file"
            , untag signedTx
            , "--mainnet"
            , "--cardano-mode"
            ]

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet
    :: HasCallStack
    => Tagged "setup" FilePath
    -> IO (Tagged "tx-in" String, Tagged "faucet-prv" FilePath)
takeFaucet setupDir = do
    i <- modifyMVar faucetIndex (\i -> pure (i + 1, i))
    let basename = untag setupDir </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr =
            fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
                . decodeBase58 bitcoinAlphabet
                . T.encodeUtf8
                . T.strip
                $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (convertToBase Base16 (blake2b256 addr)) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (Tagged @"tx-in" txin, Tagged @"faucet-prv" signingKey)

readFaucetAddresses :: HasCallStack => Tagged "setup" FilePath -> IO [Address]
readFaucetAddresses setupDir = do
    let faucetDataPath = untag setupDir </> "faucet-addrs"
    allFileNames <- listDirectory faucetDataPath
    let addrFileNames = filter (".addr" `isSuffixOf`) allFileNames
    forM addrFileNames $ readAddress . (faucetDataPath </>)
  where
    readAddress :: HasCallStack => FilePath -> IO Address
    readAddress addrFile = do
        rawFileContents <- TIO.readFile addrFile
        let base58EncodedAddress = T.strip rawFileContents
        case Address.fromBase58 base58EncodedAddress of
            Just address -> pure address
            Nothing -> error
                $ "Failed to base58-decode address file: " <> addrFile

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}

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
preRegisteredStakeKey =
    Aeson.object
        [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
        , "description" .= Aeson.String "Free form text"
        , "cborHex"
            .= Aeson.String
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
        | Aeson.lookup "scKind" scribe == Just (Aeson.String "StdoutSK") =
            Aeson.Object (Aeson.insert "scMinSev" sev scribe)
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
    _ ->
        fail
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
    = -- | How many pools
      MsgRegisteringStakePools Int
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
    deriving stock (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringPoolMetadata url hash ->
            T.pack
                $ unwords
                    [ "Hosting metadata for pool using url"
                    , url
                    , "with hash"
                    , hash
                    ]
        MsgRegisteringPoolMetadataInSMASH pool hash ->
            T.pack
                $ unwords
                    [ "Registering metadata for pool"
                    , pool
                    , "with SMASH with the metadata hash"
                    , hash
                    ]
        MsgRegisteringStakePools n ->
            mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for "
                <> T.pack fp
                <> " at "
                <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code ->
                "Failed "
                    <> msg
                    <> " with exit code "
                    <> T.pack (show code)
                    <> ":\n"
                    <> indent out
                    <> "\n"
                    <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed "
                <> msg
                <> " with exit code "
                <> T.pack (show code)
                <> ":\n"
                <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for "
                    <> T.pack name
                    <> ":\n"
                    <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status "
                    <> T.pack (show code)
                    <> ":\n"
                    <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli" : args)
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
        MsgCLIStatus _ ExitSuccess _ _ -> Debug
        MsgCLIStatus _ (ExitFailure _) _ _ -> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _ -> Info
        MsgStakeDistribution _ (ExitFailure _) _ _ -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug
        MsgRegisteringPoolMetadataInSMASH{} -> Info
        MsgRegisteringPoolMetadata{} -> Info

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
