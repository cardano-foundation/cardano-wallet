{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
      withCluster
    , withBFTNode
    , withStakePool

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
    ( Command (..), ProcessHasExited (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), NetworkParameters (..), ProtocolMagic (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, fromGenesisData, testnetVersionData )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, link, race )
import Control.Concurrent.Chan
    ( newChan, readChan, writeChan )
import Control.Concurrent.MVar
    ( MVar, newMVar, putMVar, takeMVar )
import Control.Exception
    ( SomeException, finally, handle, throwIO )
import Control.Monad
    ( forM, forM_, replicateM, replicateM_, unless, void, when )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.Aeson
    ( eitherDecode, toJSON, (.=) )
import Data.Either
    ( isLeft, isRight )
import Data.Functor
    ( ($>), (<&>) )
import Data.List
    ( isInfixOf, nub, permutations, sort )
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
    ( copyFile, doesPathExist )
import System.Environment
    ( getEnv, lookupEnv, setEnv )
import System.Exit
    ( ExitCode (..) )
import System.FilePath
    ( takeFileName, (</>) )
import System.Info
    ( os )
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory
    )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process
    ( readProcess, readProcessWithExitCode )
import Test.Utils.Paths
    ( getTestData )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    -> ExceptT String IO
        (SomeNetworkDiscriminant, NetworkParameters, NodeVersionData, Block)
parseGenesisData = \case
    TestnetConfig genesisFile -> do
        (genesis :: ShelleyGenesis TPraosStandardCrypto)
            <- ExceptT $ eitherDecode <$> BL.readFile genesisFile
        let nm = unNetworkMagic $ sgNetworkMagic genesis
        let (discriminant, vData) =
                someTestnetDiscriminant $ ProtocolMagic $ fromIntegral nm
        let (np, block0) = fromGenesisData genesis
        pure
            ( discriminant
            , np
            , vData
            , block0
            )

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: [String] -> IO String
cli args =
    readProcess "cardano-cli" args stdin
  where
    stdin = ""

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
withCluster
    :: Trace IO Text
    -- ^ Trace for subprocess control logging
    -> Severity
    -- ^ Minimal logging severity
    -> Int
    -- ^ How many pools should the cluster spawn.
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Action to run with the cluster up
    -> IO a
withCluster tr severity n action = do
    ports <- randomUnusedTCPPorts (n + 1)
    withBFTNode tr severity (head $ rotate ports) $ \socket block0 params -> do
        waitForSocket socket
        waitGroup <- newChan
        doneGroup <- newChan
        let waitAll   = replicateM  n (readChan waitGroup)
        let cancelAll = replicateM_ n (writeChan doneGroup ())

        let onException :: SomeException -> IO ()
            onException = writeChan waitGroup . Left

        forM_ (zip [0..] $ tail $ rotate ports) $ \(idx, (port, peers)) -> do
            link =<< async (handle onException $ do
                withStakePool tr severity idx (port, peers) $ do
                    writeChan waitGroup $ Right port
                    readChan doneGroup)

        TIO.putStrLn cartouche
        group <- waitAll
        if length (filter isRight group) /= n then do
            cancelAll
            throwIO $ ProcessHasExited
                ("cluster didn't start correctly: " <> show (filter isLeft group))
                (ExitFailure 1)
        else do
            action socket block0 params `finally` cancelAll
  where
    -- | Get permutations of the size (n-1) for a list of n elements, alongside with
    -- the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap (\(x:xs) -> (x, sort xs)) . permutations

withBFTNode
    :: Trace IO Text
    -- ^ Trace for subprocess control logging
    -> Severity
    -- ^ Minimal logging severity
    -> (Int, [Int])
    -- ^ A list of ports used by peers and this pool.
    -> (FilePath -> Block -> (NetworkParameters, NodeVersionData) -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr severity (port, peers) action =
    withTempDir "bft-node" $ \dir -> do
        [vrfPrv, kesPrv, opCert] <- forM
            ["node-vrf.skey", "node-kes.skey", "node.opcert"]
            (\f -> copyFile (source </> f) (dir </> f) $> (dir </> f))

        (config, block0, networkParams, versionData) <- genConfig dir severity
        topology <- genTopology dir peers
        let socket = genSocketPath dir

        let args =
                [ "run"
                , "--config", config
                , "--topology", topology
                , "--database-path", dir </> "db"
                , "--socket-path", socket
                , "--port", show port
                , "--shelley-kes-key", kesPrv
                , "--shelley-vrf-key", vrfPrv
                , "--shelley-operational-certificate", opCert
                ]
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        ((either throwIO pure) =<<)
            $ withBackendProcess (trMessageText tr) cmd
            $ action socket block0 (networkParams, versionData)
  where
    source :: FilePath
    source = $(getTestData) </> "cardano-node-shelley"

-- | Start a "stake pool node". The pool will register itself.
withStakePool
    :: Trace IO Text
    -- ^ Trace for subprocess control logging
    -> Severity
    -- ^ Minimal logging severity
    -> Int
    -- ^ Unique stake pool number
    -> (Int, [Int])
    -- ^ A list of ports used by peers and this pool.
    -> IO a
    -- ^ Callback function called once the pool has started.
    -> IO a
withStakePool tr severity idx (port, peers) action =
    withTempDir ("stake-pool-" ++ show idx) $ \dir -> do
        -- Node configuration
        (opPrv, opPub, opCount) <- genOperatorKeyPair dir
        (vrfPrv, vrfPub) <- genVrfKeyPair dir
        (kesPrv, kesPub) <- genKesKeyPair dir
        opCert <- issueOpCert dir kesPub opPrv opCount
        (config, _, _, _) <- genConfig dir severity
        topology <- genTopology dir peers

        -- Pool registration
        (stakePrv, stakePub) <- genStakeAddrKeyPair dir
        stakeCert <- issueStakeCert dir stakePub
        poolCert <- issuePoolCert dir opPub vrfPub stakePub
        dlgCert <- issueDlgCert dir stakePub opPub

        let args =
                [ "run"
                , "--config", config
                , "--topology", topology
                , "--database-path", dir </> "db"
                , "--socket-path", genSocketPath dir
                , "--port", show port
                , "--shelley-kes-key", kesPrv
                , "--shelley-vrf-key", vrfPrv
                , "--shelley-operational-certificate", opCert
                ]
        let cmd = Command "cardano-node" args (pure ()) Inherit Inherit
        ((either throwIO pure) =<<)
            $ withBackendProcess (trMessageText tr) cmd
            $ do
                path <- getEnv "CARDANO_NODE_SOCKET_PATH"
                doesExist <- doesPathExist path
                B8.putStrLn $ B8.pack $ unlines
                    [ "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , path
                    , "does exist? " <> if doesExist then "yes" else "no"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    , "@@@@@@@@@@@@@@@@@@@@"
                    ]

                -- In order to get a working stake pool we need to.
                --
                -- 1. Register a stake key for our pool.
                -- 2. Register the stake pool
                -- 3. Delegate funds to our pool's key.
                --
                -- We cheat a bit here by delegating to our stake address right away in
                -- the transaction used to registered the stake key and the pool itself.
                -- Thus, in a single transaction, we end up with a registered pool with
                -- some stake!
                (rawTx, faucetPrv) <- prepareTx dir stakePub [stakeCert, poolCert, dlgCert]
                signTx dir rawTx [faucetPrv, stakePrv, opPrv] >>= submitTx
                timeout 120 ("pool registration", waitUntilRegistered opPub)
                action

genConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> Severity
    -- ^ Minimum severity level for logging
    -> IO (FilePath, Block, NetworkParameters, NodeVersionData)
genConfig dir severity = do
    -- we need to specify genesis file location every run in tmp
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withObject (addGenesisFilePath (T.pack nodeGenesisFile))
        >>= withObject (addMinSeverity (T.pack $ show severity))
        >>= Yaml.encodeFile (dir </> "node.config")

    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "genesis.yaml")
        >>= withObject updateStartTime
        >>= Aeson.encodeFile nodeGenesisFile

    (genesisData :: ShelleyGenesis TPraosStandardCrypto)
        <- either (error . show) id . eitherDecode <$> BL.readFile nodeGenesisFile

    let (networkParameters, block0) = fromGenesisData genesisData
    let nm = sgNetworkMagic genesisData
    pure
        ( dir </> "node.config"
        , block0
        , networkParameters
        , (NodeToClientVersionData nm, nodeToClientCodecCBORTerm)
        )
  where
    source :: FilePath
    source = $(getTestData) </> "cardano-node-shelley"

    nodeGenesisFile :: FilePath
    nodeGenesisFile = dir </> "genesis.json"

-- | Generate a valid socket path based on the OS.
genSocketPath :: FilePath -> FilePath
genSocketPath dir =
    if os == "mingw32"
    then "\\\\.\\pipe\\" ++ takeFileName dir
    else dir </> "node.socket"

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

-- | Create a key pair for a node operator's offline key and a new certificate
-- issue counter
genOperatorKeyPair :: FilePath -> IO (FilePath, FilePath, FilePath)
genOperatorKeyPair dir = do
    let opPub = dir </> "op.pub"
    let opPrv = dir </> "op.prv"
    let opCount = dir </> "op.count"
    void $ cli
        [ "shelley", "node", "key-gen"
        , "--verification-key-file", opPub
        , "--signing-key-file", opPrv
        , "--operational-certificate-issue-counter", opCount
        ]
    pure (opPrv, opPub, opCount)

-- | Create a key pair for a node KES operational key
genKesKeyPair :: FilePath -> IO (FilePath, FilePath)
genKesKeyPair dir = do
    let kesPub = dir </> "kes.pub"
    let kesPrv = dir </> "kes.prv"
    void $ cli
        [ "shelley", "node", "key-gen-KES"
        , "--verification-key-file", kesPub
        , "--signing-key-file", kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair :: FilePath -> IO (FilePath, FilePath)
genVrfKeyPair dir = do
    let vrfPub = dir </> "vrf.pub"
    let vrfPrv = dir </> "vrf.prv"
    void $ cli
        [ "shelley", "node", "key-gen-VRF"
        , "--verification-key-file", vrfPub
        , "--signing-key-file", vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair :: FilePath -> IO (FilePath, FilePath)
genStakeAddrKeyPair dir = do
    let stakePub = dir </> "stake.pub"
    let stakePrv = dir </> "stake.prv"
    void $ cli
        [ "shelley", "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]
    pure (stakePrv, stakePub)

-- | Issue a node operational certificate
issueOpCert :: FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    void $ cli
        [ "shelley", "node", "issue-op-cert"
        , "--kes-verification-key-file", kesPub
        , "--cold-signing-key-file", opPrv
        , "--operational-certificate-issue-counter-file", opCount
        , "--kes-period", "0"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate
issueStakeCert :: FilePath -> FilePath -> IO FilePath
issueStakeCert dir stakePub = do
    let file = dir </> "stake.cert"
    void $ cli
        [ "shelley", "stake-address", "registration-certificate"
        , "--staking-verification-key-file", stakePub
        , "--out-file", file
        ]
    pure file

-- | Create a stake pool registration certificate
issuePoolCert :: FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issuePoolCert dir opPub vrfPub stakePub = do
    let file = dir </> "pool.cert"
    void $ cli
        [ "shelley", "stake-pool", "registration-certificate"
        , "--cold-verification-key-file", opPub
        , "--vrf-verification-key-file", vrfPub
        , "--pool-pledge", show pledgeAmt
        , "--pool-cost", "0"
        , "--pool-margin", "0.1"
        , "--pool-reward-account-verification-key-file", stakePub
        , "--pool-owner-stake-verification-key-file", stakePub
        , "--mainnet"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address delegation certificate.
issueDlgCert :: FilePath -> FilePath -> FilePath -> IO FilePath
issueDlgCert dir stakePub opPub = do
    let file = dir </> "dlg.cert"
    void $ cli
        [ "shelley", "stake-address", "delegation-certificate"
        , "--staking-verification-key-file", stakePub
        , "--stake-pool-verification-key-file", opPub
        , "--out-file", file
        ]
    pure file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareTx :: FilePath -> FilePath -> [FilePath] -> IO (FilePath, FilePath)
prepareTx dir stakePub certs = do
    let file = dir </> "tx.raw"

    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    void $ cli
        [ "shelley", "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    addr <- cli
        [ "shelley", "address", "build"
        , "--payment-verification-key-file", sinkPub
        , "--stake-verification-key-file", stakePub
        , "--mainnet"
        ]

    (faucetInput, faucetPrv) <- takeFaucet dir
    void $ cli $
        [ "shelley", "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", init addr <> "+" <> show pledgeAmt
        , "--ttl", "100"
        , "--fee", show (faucetAmt - pledgeAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

-- | Sign a transaction with all the necessary signatures.
signTx :: FilePath -> FilePath -> [FilePath] -> IO FilePath
signTx dir rawTx keys = do
    let file = dir </> "tx.signed"
    void $ cli $
        [ "shelley", "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ] ++ mconcat ((\key -> ["--signing-key-file", key]) <$> keys)
    pure file

-- | Submit a transaction through a running node
--
-- The node's target is assumed from an ENV var 'CARDANO_NODE_SOCKET_PATH'
submitTx :: FilePath -> IO ()
submitTx signedTx = do
    void $ cli
        [ "shelley", "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet"
        ]

-- | Wait for a command which depends on connecting to the given socket path to
-- succeed.
--
-- It retries every second, for up to 30 seconds. An exception is thrown if
-- it has waited for too long.
--
-- As a side effect, after this subroutine finishes, the environment variable
-- @CARDANO_NODE_SOCKET_PATH@ is set.
waitForSocket :: FilePath -> IO ()
waitForSocket socketPath = do
    setEnv "CARDANO_NODE_SOCKET_PATH" socketPath
    (st, err) <- retrying pol (const isFail) (const query)
    unless (st == ExitSuccess) $
       throwIO $ ProcessHasExited
           ("cluster bft node didn't start correctly: " <> err) st
  where
    -- TODO: check whether querying the tip works just as well.
    query = do
        B8.putStrLn . B8.pack $
            "Waiting checking for usable socket file " <> socketPath
        (st, _, err) <- readProcessWithExitCode
            "cardano-cli"
            ["shelley", "query", "stake-distribution", "--mainnet"]
            mempty
        unless (st == ExitSuccess) $ B8.putStrLn $ B8.pack err
        pure (st, err)
    isFail (st, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- | Wait until a stake pool shows as registered on-chain.
waitUntilRegistered :: FilePath -> IO ()
waitUntilRegistered opPub = do
    poolId <- init <$> cli
        [ "shelley", "stake-pool", "id"
        , "--verification-key-file", opPub
        ]
    (exitCode, distribution, err) <- readProcessWithExitCode
        "cardano-cli"
        [ "shelley", "query", "stake-distribution"
        , "--mainnet"
        ]
        mempty
    when (exitCode /= ExitSuccess) $ do
        putStrLn $ "query of stake-distribution " ++ show exitCode
        putStrLn err

    unless (poolId `isInfixOf` distribution) $ do
        threadDelay 5000000
        waitUntilRegistered opPub

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: FilePath -> IO (String, FilePath)
takeFaucet dir = takeMVar faucets >>= \case
    []    -> fail "takeFaucet: Awe crap! No more faucet available!"
    ((input,prv):q) -> do
        putMVar faucets q
        let file = dir </> "faucet.prv"
        writeFile  file prv
        pure (input, file)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucets :: MVar [(String, String)]
faucets = unsafePerformIO $ newMVar
    [ ( "ca2cada0a7c518cecddcdad2ea9b320d7a2d197565c64c37b3638a3428c9988a#0"
      , unlines
          [ "type: Genesis UTxO signing key"
          , "title: Genesis initial UTxO key"
          , "cbor-hex:"
          , "  582089574dc85f0359010458a6160836776fe2c8073ad460d3fba6d08ace684a90a3"
          ]
      )
    , ( "cf28494def86c9917c8bef9faa9b3891e9e0a9f30a76f4576921a39b049d9398#0"
      , unlines
          [ "type: Genesis UTxO signing key"
          , "title: Genesis initial UTxO key"
          , "cbor-hex:"
          , "  5820cd42926a6a0d1651dbb08aa393322b0f9a0037f3579b5f8062a57c2ff6c025e4"
          ]
      )
    , ( "9b659bf3d026cf49e0387eb19473dad4b96db8480b7e26a3e20f5c63551303a7#0"
      , unlines
          [ "type: Genesis UTxO signing key"
          , "title: Genesis initial UTxO key"
          , "cbor-hex:"
          , "  5820863d7c0d65d34bebea180435b13e1c9bb885f2a18d3ff2b47435b91fad0293c0"
          ]
      )
    , ( "3c015335bd3a676a62e3d3c61828fa1ff0eb3dd950331aba93eec68e9e024411#0"
      , unlines
          [ "type: Genesis UTxO signing key"
          , "title: Genesis initial UTxO key"
          , "cbor-hex:"
          , "  5820c92896ab4c0af89459acf786315a6b13b23f037e4d9747c8414a0d48f7418075"
          ]
      )
    , ( "82a8981af11569eef547e012232f5306b85016d27aff4c1bbd5801800f83baab#0"
      , unlines
          [ "type: Genesis UTxO signing key"
          , "title: Genesis initial UTxO key"
          , "cbor-hex:"
          , "  58207b9de02388f78146cb53e4fb4a4d8917ac367cdfe0ff1d4008cc9da52af6ac74"
          ]
      )
    ]
{-# NOINLINE faucets #-}

-- | Pledge amount used for each pool.
pledgeAmt :: Integer
pledgeAmt = oneMillionAda

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 10 * oneMillionAda

-- | Just one million Ada, in Lovelace.
oneMillionAda :: Integer
oneMillionAda = 1_000_000_000_000

-- | Add a "systemStart" field in a given object with the current POSIX time as a
-- value.
updateStartTime
    :: Aeson.Object
    -> IO Aeson.Object
updateStartTime m = do
    time <- getCurrentTime
    pure $ HM.insert "systemStart" (toJSON time) m

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

-- | Little helper to run an action within a certain delay. Fails if the action
-- takes too long.
timeout :: Int -> (String, IO a) -> IO a
timeout t (title, action) = do
    race (threadDelay $ t * 1000000) action >>= \case
        Left _  -> fail ("Waited too long for: " <> title)
        Right a -> pure a

-- | A little disclaimer shown in the logs when setting up the cluster.
cartouche :: Text
cartouche = T.unlines
    [ ""
    , "################################################################################"
    , "#                                                                              #"
    , "#  ⚠                           DISCLAIMER                                   ⚠  #"
    , "#                                                                              #"
    , "#        Cluster is booting. Stake pools are being registered on chain.        #"
    , "#                                                                              #"
    , "#        This may take roughly 30s, after what pools will become active        #"
    , "#        and will start producing blocks. Please be patient...                 #"
    , "#                                                                              #"
    , "#  ⚠                           DISCLAIMER                                   ⚠  #"
    , "#                                                                              #"
    , "################################################################################"
    ]

-- | Create a temporary directory and remove it after the given IO action has
-- finished -- unless the @NO_CLEANUP@ environment variable has been set.
withTempDir
    :: String   -- ^ Directory name template
    -> (FilePath -> IO a) -- ^ Callback that can use the directory
    -> IO a
withTempDir name action = isEnvSet "NO_CLEANUP" >>= \case
    True -> do
        parent <- getCanonicalTemporaryDirectory
        dir <- createTempDirectory parent name
        res <- action dir
        putStrLn $ "NO_CLEANUP of temporary directory " ++ dir
        pure res
    False -> withSystemTempDirectory name action
  where
    isEnvSet ev = lookupEnv ev <&> \case
        Nothing -> False
        Just "" -> False
        Just _ -> True
