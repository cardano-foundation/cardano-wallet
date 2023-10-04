{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant pure" -}

-- | Benchmark measuring how long restoration takes for different wallets.
--
-- Easiest run using
-- @
--     $ export NODE_DB="node-db-testnet"
--     $ nix build .#benchmarks/cardano-wallet/restore -o restore && ./restore/bin/restore testnet
-- @
--
-- or
-- @
--     $ ./.buildkite/bench-restore.sh shelley testnet
-- @
--
-- since it relies on lots of configuration most most easily retrieved with nix.
--
module Main where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Extra
    ( trMessageText )
import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.Launcher.Node
    ( CardanoNodeConn )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToMnemonic )
import Cardano.Wallet
    ( WalletLayer (..), WalletWorkerLog (..), dummyChangeAddressGen )
import Cardano.Wallet.Address.Book
    ( AddressBookIso )
import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery
    ( GenChange (..), IsOurs )
import Cardano.Wallet.Address.Discovery.RandomAny
    ( RndAnyState, mkRndAnyState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap, mkAddressPoolGap, purposeCIP1852 )
import Cardano.Wallet.Address.Discovery.SequentialAny
    ( SeqAnyState )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqAnyState )
import Cardano.Wallet.Address.Keys.WalletKey
    ( digest, publicKey )
import Cardano.Wallet.Address.MaybeLight
    ( MaybeLight )
import Cardano.Wallet.Api.Types
    ( toApiUtxoStatistics )
import Cardano.Wallet.BenchShared
    ( RestoreBenchArgs (..)
    , Time
    , argsNetworkDir
    , bench
    , execBenchWithNode
    , initBenchmarkLogging
    , runBenchmarks
    , withTempSqliteFile
    )
import Cardano.Wallet.DB
    ( DBFresh )
import Cardano.Wallet.DB.Layer
    ( PersistAddressBook, withDBFresh )
import Cardano.Wallet.Flavor
    ( CredFromOf
    , Excluding
    , KeyFlavorS (..)
    , KeyOf
    , WalletFlavor (..)
    , keyFlavorFromState
    )
import Cardano.Wallet.Network
    ( ChainFollowLog (..)
    , ChainFollower (..)
    , ChainSyncLog (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Network.Config
    ( NetworkConfiguration (..), parseGenesisData )
import Cardano.Wallet.Primitive.Model
    ( Wallet, availableUTxO, currentTip, getState, totalUTxO )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (sNetworkId)
    , NetworkDiscriminant (..)
    , SNetworkId (..)
    , networkDiscriminantVal
    , networkIdVal
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, neverFails )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), SyncTolerance, mkSyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , SlotNo (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletName (..)
    , chainPointFromBlockHeader
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials, RootCredentials (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( UTxOStatistics (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( AnyCardanoEra (..)
    , CardanoBlock
    , NodeToClientVersionData
    , StandardCrypto
    , emptyGenesis
    , fromCardanoBlock
    , numberOfTransactionsInBlock
    )
import Cardano.Wallet.Shelley.Network.Node
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Transaction
    ( PreSelection (..), defaultTransactionCtx )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy, unsafeMkPercentage, unsafeRunExceptT )
import Cardano.Write.Tx
    ( AnyRecentEra (..) )
import Control.Arrow
    ( first )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( unless, void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Control.Tracer
    ( Tracer (..), traceWith )
import Crypto.Hash.Extra
    ( blake2b256 )
import Data.Aeson
    ( ToJSON (..), genericToJSON, (.=) )
import Data.Functor.Contravariant
    ( contramap )
import Data.List
    ( foldl' )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time
    ( UTCTime, diffUTCTime )
import Data.Time.Clock.POSIX
    ( POSIXTime, getCurrentTime, utcTimeToPOSIXSeconds )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable
    , blockListF'
    , build
    , genericF
    , nameF
    , pretty
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Conc
    ( retry )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Numeric
    ( fromRat, showFFloat )
import Ouroboros.Network.Client.Wallet
    ( PipeliningStrategy, tunedForMainnetPipeliningStrategy )
import Say
    ( sayErr, sayShow )
import System.Exit
    ( exitWith )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hFlush, withFile )
import UnliftIO
    ( async, atomically, cancel, newTVarIO, readTVar, readTVarIO, writeTVar )
import UnliftIO.Concurrent
    ( forkIO, threadDelay )
import UnliftIO.Exception
    ( evaluate, throwString )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Checkpoints.Policy as CP
import qualified Cardano.Wallet.DB.Sqlite.Migration.Old as Sqlite
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Cardano.Wallet.Shelley.Compatibility as Cardano
import qualified Cardano.Write.Tx as Write
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = execBenchWithNode argsNetworkConfig cardanoRestoreBench >>= exitWith

{-------------------------------------------------------------------------------
                                Shelley benchmarks
-------------------------------------------------------------------------------}

argsNetworkConfig :: RestoreBenchArgs -> NetworkConfiguration
argsNetworkConfig args = case argNetworkName args of
    "mainnet" ->
        MainnetConfig
    _ ->
        TestnetConfig (argsNetworkDir args </> "genesis-byron.json")

-- | Run all available benchmarks.
cardanoRestoreBench
    :: Trace IO Text
    -> NetworkConfiguration
    -> CardanoNodeConn
    -> IO ()
cardanoRestoreBench tr c socketFile = do
    (networkId, np, vData, _b) <- unsafeRunExceptT $ parseGenesisData c
    (_, walletTr) <- initBenchmarkLogging "wallet" Notice

    withSNetworkId networkId $ \(sNetwork :: SNetworkId n)-> do
        let network =  networkDiscriminantVal sNetwork

        sayErr $ "Network: " <> network
        prepareNode (trMessageText tr) sNetwork socketFile np vData

        let benchRestoreRndWithOwnership p pipelinings = do
                let benchname = showPercentFromPermyriad p <> "-percent-rnd"
                bench_restoration
                    pipelinings
                    sNetwork
                    (trMessageText tr)
                    walletTr
                    socketFile
                    np
                    vData
                    benchname
                    (walletRnd benchname $ mkRndAnyState' p sNetwork)
                    True -- Write progress to .timelog file
                    (unsafeMkPercentage 1)
                    benchmarksRnd
        let benchRestoreSeqWithOwnership p pipelinings = do
                let benchname = showPercentFromPermyriad p <> "-percent-seq"
                bench_restoration
                    pipelinings
                    sNetwork
                    (trMessageText tr)
                    walletTr
                    socketFile
                    np
                    vData
                    benchname
                    (walletSeq benchname $ mkSeqAnyState' p sNetwork)
                    True -- Write progress to .timelog file
                    (unsafeMkPercentage 1)
                    benchmarksSeq
        let benchRestoreBaseline pipelinings = do
                let benchname = "baseline"
                bench_baseline_restoration
                    pipelinings
                    sNetwork
                    (trMessageText tr)
                    walletTr
                    socketFile
                    np
                    vData
                    benchname
                    True
                    (unsafeMkPercentage 1)

        runBenchmarks [
            benchRestoreBaseline tunedForMainnetPipeliningStrategy
            -- -- We restore /to/ a percentage that is low enough to be fast,
            -- -- but high enough to give an accurate enough indication of the
            -- -- to-100% time.
            , benchRestoreSeqWithOwnership (Proxy @0)
                tunedForMainnetPipeliningStrategy
            , benchRestoreSeqWithOwnership (Proxy @1)
                tunedForMainnetPipeliningStrategy
            , benchRestoreRndWithOwnership (Proxy @10)
                tunedForMainnetPipeliningStrategy
            ]
  where
    walletRnd
        :: Text
        -> (ByronKey 'RootK XPrv -> Int -> s)
        -> (WalletId, WalletName, s)
    walletRnd wname mkState =
        let
            seed = dummySeedFromName wname
            xprv = Byron.generateKeyFromSeed seed mempty
            wid = WalletId $ digest ByronKeyS $ publicKey ByronKeyS xprv
            rngSeed = 0
            s = mkState xprv rngSeed
        in
            (wid, WalletName wname, s)

    walletSeq
        :: Text
        -> (ClearCredentials ShelleyKey -> AddressPoolGap -> s)
        -> (WalletId, WalletName, s)
    walletSeq wname mkState =
        let
            seed = dummySeedFromName wname
            xprv = Shelley.generateKeyFromSeed (seed, Nothing) mempty
            wid = WalletId $ digest ShelleyKeyS $ publicKey ShelleyKeyS xprv
            Right gap = mkAddressPoolGap 20
            s = mkState (RootCredentials xprv mempty) gap
        in
            (wid, WalletName wname, s)

    mkSeqAnyState'
        :: forall (p :: Nat) n. HasSNetworkId n
        => Proxy p
        -> SNetworkId n
        -> ClearCredentials ShelleyKey
        -> AddressPoolGap
        -> SeqAnyState n ShelleyKey p
    mkSeqAnyState' _ _ credentials =
        mkSeqAnyState @p @n ShelleyKeyS credentials purposeCIP1852


    mkRndAnyState'
        :: forall (p :: Nat) n. ()
        => Proxy p
        -> SNetworkId n
        -> ByronKey 'RootK XPrv
        -> Int
        -> RndAnyState n p
    mkRndAnyState' _ _ = mkRndAnyState

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

data SomeBenchmarkResults where
    SomeBenchmarkResults
        :: forall result. Buildable result
        => result
        -> SomeBenchmarkResults

instance Buildable SomeBenchmarkResults where
    build (SomeBenchmarkResults results) = build results

data WalletOverview = WalletOverview
    { utxo :: UTxOStatistics
    , addresses :: Word
    , transactions :: Word
    } deriving (Show, Generic)

instance Buildable WalletOverview where
    build WalletOverview{utxo,addresses,transactions} =
        blockListF' "" id
            [ nameF "number of addresses" (build addresses)
            , nameF "number of transactions" (build transactions)
            , build utxo
            ]

instance ToJSON WalletOverview where
    toJSON WalletOverview{utxo,addresses,transactions} = Aeson.object
        [ "utxo" .= toApiUtxoStatistics utxo
        , "addresses" .= addresses
        , "transactions" .= transactions
        ]

data BenchRndResults = BenchRndResults
    { benchName :: Text
    , restoreTime :: Time
    , readWalletTime :: Time
    , listAddressesTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , importOneAddressTime :: Time
    , importManyAddressesTime :: Time
    , estimateFeesTime :: Either CannotEstimateFeeForWalletWithEmptyUTxO Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchRndResults where
    build = genericF

instance ToJSON BenchRndResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksRnd
    :: forall n s p.
        ( s ~ RndAnyState n p
        , HasSNetworkId n
        , KnownNat p
        )
    => SNetworkId n
    -> WalletLayer IO s
    -> WalletName
    -> Text
    -> Time
    -> IO BenchRndResults
benchmarksRnd network w wname
    benchname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- W.readWallet w
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fromIntegral . length
        <$> W.listAddresses w (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions w Nothing Nothing Nothing Descending
            Nothing Nothing

    -- To aid with debugging, write the current wallet state to the log:
    let walletOverview = WalletOverview{utxo,addresses,transactions}
    sayShow $ unlines
        [ "Wallet overview (rnd):"
        , show benchname
        , show walletOverview
        ]

    (_, listTransactionsLimitedTime) <- bench "list transactions (max_count = 100)" $ do
        unsafeRunExceptT
        $ W.listTransactions w Nothing Nothing Nothing Descending
            (Just 100) Nothing

    estimateFeesTime <-
        withNonEmptyUTxO cp pending CannotEstimateFeeForWalletWithEmptyUTxO $
            benchEstimateTxFee network w

    oneAddress <- genAddresses 1 cp
    (_, importOneAddressTime) <- bench "import one addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses w oneAddress

    manyAddresses <- genAddresses 1000 cp
    (_, importManyAddressesTime) <- bench "import many addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses w manyAddresses

    pure BenchRndResults
        { benchName = benchname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
        , listTransactionsLimitedTime
        , estimateFeesTime
        , importOneAddressTime
        , importManyAddressesTime
        , walletOverview
        }
  where
    genAddresses :: Int -> Wallet s -> IO [Address]
    genAddresses n cp = evaluate $ fst $ foldl'
        (\(xs, s) _ -> first (:xs) $ genChange (xprv, mempty) s)
        ([], getState cp)
        [1..n]
      where
        seed = dummySeedFromName $ getWalletName wname
        xprv = Byron.generateKeyFromSeed seed mempty

data BenchSeqResults = BenchSeqResults
    { benchName :: Text
    , restoreTime :: Time
    , readWalletTime :: Time
    , listAddressesTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , estimateFeesTime :: Either CannotEstimateFeeForWalletWithEmptyUTxO Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchSeqResults where
    build = genericF

instance ToJSON BenchSeqResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksSeq
    :: forall n s k p.
        ( s ~ SeqAnyState n k p
        , k ~ ShelleyKey
        , HasSNetworkId n
        , KnownNat p
        )
    => SNetworkId n
    -> WalletLayer IO s
    -> WalletName
    -> Text -- ^ Bench name
    -> Time
    -> IO BenchSeqResults
benchmarksSeq network w _wname
    benchname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- W.readWallet w
        pure (cp, pending)
    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fromIntegral . length <$> W.listAddresses w (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions w Nothing Nothing Nothing Descending
            Nothing Nothing

    -- To aid with debugging, write the current wallet state to the log:
    let walletOverview = WalletOverview{utxo,addresses,transactions}
    sayShow $ unlines
        [ "Wallet overview (seq):"
        , show benchname
        , show walletOverview
        ]

    (_, listTransactionsLimitedTime) <- bench "list transactions (max_count = 100)" $ do
        unsafeRunExceptT
        $ W.listTransactions w Nothing Nothing Nothing Descending
            (Just 100) Nothing

    estimateFeesTime <-
        withNonEmptyUTxO cp pending CannotEstimateFeeForWalletWithEmptyUTxO $
            benchEstimateTxFee network w

    pure BenchSeqResults
        { benchName = benchname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
        , listTransactionsLimitedTime
        , estimateFeesTime
        , walletOverview
        }

data BenchBaselineResults = BenchBaselineResults
    { benchName :: Text
    , restorationTime :: Time
    } deriving (Show, Generic)

instance Buildable BenchBaselineResults where
    build = genericF

instance ToJSON BenchBaselineResults where
    toJSON = genericToJSON Aeson.defaultOptions

{- HLINT ignore bench_baseline_restoration "Use camelCase" -}
bench_baseline_restoration
    :: forall n
     . HasSNetworkId n
    => PipeliningStrategy (CardanoBlock StandardCrypto)
    -> SNetworkId n
    -> Tracer IO (BenchmarkLog n)
    -> Trace IO Text
    -- ^ For wallet tracing
    -> CardanoNodeConn
    -- ^ Socket path
    -> NetworkParameters
    -> NodeToClientVersionData
    -> Text
    -- ^ Benchmark name (used for naming resulting files)
    -> Bool
     -- ^ if to write to disk
    -> Percentage
    -- ^ Target sync progress
    -> IO SomeBenchmarkResults
bench_baseline_restoration
    pipeliningStrat _proxy tr wlTr socket np vData benchName
        traceToDisk targetSync = do
            putStrLn $ "*** " ++ T.unpack benchName
            withRestoreEnvironment doRestore
  where
    withRestoreEnvironment action =
        withWalletLayerTracer benchName pipeliningStrat traceToDisk $
            \progressTrace -> withNetworkLayer
                networkTrace pipeliningStrat np socket vData sTol $ \nw ->
                    action progressTrace nw
      where
        networkId = networkIdVal (sNetworkId @n)
        networkTrace = trMessageText wlTr
    doRestore
        :: Tracer IO (Maybe (Quantity "block" Word32))
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> IO SomeBenchmarkResults
    doRestore progressTrace nw = do
        wait <- newTVarIO Nothing
        chainPointT <- newTVarIO []
        synchronizer <- async
            $ chainSync nw nullTracer
            $ ChainFollower
            { checkpointPolicy = const CP.atTip
            , readChainPoints  = readTVarIO chainPointT
            , rollForward = \blocks ntip -> do
                atomically $ writeTVar chainPointT
                    [chainPointFromBlockHeader ntip]
                let (ntxs, hss) = NE.unzip $
                        numberOfTransactionsInBlock <$> blocks
                    (heights, slots) = NE.unzip hss
                    tip = NE.last heights
                traceWith progressTrace $ Just tip
                seq (sum ntxs)
                    . atomically
                    . writeTVar wait
                    . Just
                    $ NE.last slots
            , rollBackward = pure
            }
        (_, restorationTime) <- bench "restoration" $
            reportProgress nw tr targetSync
                (atomically $ readTVar wait >>= maybe retry pure)
        saveBenchmarkPoints benchName restorationTime
        cancel synchronizer
        pure $ SomeBenchmarkResults
            BenchBaselineResults {benchName, restorationTime}

{- HLINT ignore bench_restoration "Use camelCase" -}
bench_restoration
    :: forall n s results.
        ( IsOurs s RewardAccount
        , MaybeLight s
        , PersistAddressBook s
        , WalletFlavor s
        , HasSNetworkId n
        , Buildable results
        , ToJSON results
        , IsOurs s Address
        )
    => PipeliningStrategy (CardanoBlock StandardCrypto)
    -> SNetworkId n
    -> Tracer IO (BenchmarkLog n)
    -> Trace IO Text -- ^ For wallet tracing
    -> CardanoNodeConn  -- ^ Socket path
    -> NetworkParameters
    -> NodeToClientVersionData
    -> Text -- ^ Benchmark name (used for naming resulting files)
    -> (WalletId, WalletName, s)
    -> Bool -- ^ If @True@, will trace detailed progress to a .timelog file.
    -> Percentage -- ^ Target sync progress
    -> (SNetworkId n
        -> WalletLayer IO s
        -> WalletName
        -> Text
        -> Time
        -> IO results)
    -> IO SomeBenchmarkResults
bench_restoration
    pipeliningStrat proxy tr wlTr socket np vData benchname (wid, wname, s) traceToDisk
        targetSync benchmarks = do
    putStrLn $ "*** " ++ T.unpack benchname
    let networkId = networkIdVal (sNetworkId @n)
    let tl = newTransactionLayer (keyFlavorFromState @s) networkId
    let gp = genesisParameters np
    withNetworkLayer (trMessageText wlTr) pipeliningStrat
        np socket vData sTol $ \nw -> do
            let ti = neverFails "bench db shouldn't forecast into future"
                    $ timeInterpreter nw
            withBenchDBLayer @s ti wlTr wid
                $ \dbf -> withWalletLayerTracer
                    benchname pipeliningStrat traceToDisk
                $ \progressTrace -> do
                    let gps = (emptyGenesis gp, np)
                    db <- unsafeRunExceptT $ W.createWallet gps dbf wid wname s
                    let tracer =
                            trMessageText wlTr <>
                            contramap walletWorkerLogToBlockHeight progressTrace
                    let w = WalletLayer tracer gps nw tl db
                    void
                        $ forkIO
                        $ unsafeRunExceptT
                        $ W.restoreWallet w

                    -- NOTE: This is now the time to restore /all/ wallets.
                    (_, restorationTime) <- bench "restoration" $ do
                        waitForWalletSyncTo
                            targetSync
                            tr
                            proxy
                            w
                            wid
                            gp
                            vData

                    results <-
                        benchmarks proxy w wname benchname restorationTime
                    saveBenchmarkPoints benchname results
                    pure $ SomeBenchmarkResults results

walletWorkerLogToBlockHeight
    :: WalletWorkerLog
    -> Maybe (Quantity "block" Word32)
walletWorkerLogToBlockHeight = \case
    MsgChainFollow (MsgChainSync (MsgChainRollForward bs _nodeTip)) ->
        Just $ blockHeight $ NE.last bs
    _ ->
        Nothing

saveBenchmarkPoints :: ToJSON a => Text -> a -> IO ()
saveBenchmarkPoints benchname = Aeson.encodeFile (T.unpack benchname <> ".json")

withWalletLayerTracer
    :: Show a
    => Text
    -> a
    -> Bool
    -> (Tracer IO (Maybe (Quantity "block" Word32)) -> IO r)
    -> IO r
withWalletLayerTracer benchname pipelining traceToDisk act = do
    let benchmarkFilename
            = T.unpack $ benchname
                <> "."
                <> T.replace " " "_" (T.pack $ show pipelining)
                <> ".timelog"
    t0 <- getCurrentTime
    if
        | traceToDisk ->
            withFile
                benchmarkFilename
                WriteMode
                $ \h -> do
                -- Use a custom tracer to output (time, blockHeight) to a file
                -- each time we apply blocks.
                let fileTr = Tracer $ \msg -> do
                        liftIO . B8.hPut h . T.encodeUtf8 . (<> "\n") $ msg
                        hFlush h
                act $ traceBlockHeadersProgressForPlotting t0 fileTr
        | otherwise -> act nullTracer

dummyAddress
    :: SNetworkId n
    -> Address

dummyAddress n = case n of
    SMainnet ->
        Address $ BS.pack $ 0 : replicate 56 0
    _ ->
        Address $ BS.pack $ 1 : replicate 56 0

-- | Hash the given text, and construct a mnemonic from it.
dummySeedFromName :: Text -> SomeMnemonic
dummySeedFromName = SomeMnemonic @24
    . entropyToMnemonic
    . unsafeMkEntropy @256
    . blake2b256
    . T.encodeUtf8

traceBlockHeadersProgressForPlotting
    :: UTCTime
    -> Tracer IO Text
    -> Tracer IO (Maybe (Quantity "block" Word32))
traceBlockHeadersProgressForPlotting t0  tr = Tracer $ \bs -> do
    let mtip = pretty . getQuantity <$> bs
    time <- pretty . (`diffUTCTime` t0) <$> getCurrentTime
    case mtip of
        Just tip -> traceWith tr $ time <> " " <> tip
        Nothing -> pure ()

withBenchDBLayer
    :: forall s a
     . ( PersistAddressBook s
       , WalletFlavor s
       )
    => TimeInterpreter IO
    -> Trace IO Text
    -> WalletId
    -> (DBFresh IO s -> IO a)
    -> IO a
withBenchDBLayer ti tr wid action =
    withTempSqliteFile $ \dbFile ->
        withDBFresh (walletFlavor @s) tr'
            (Just migrationDefaultValues) dbFile ti wid action
  where
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 0
        , Sqlite.defaultHardforkEpoch = Nothing
        , Sqlite.defaultKeyDeposit = Coin 0
        }
    tr' = trMessageText tr

prepareNode
    :: forall n. HasSNetworkId n
    => Tracer IO (BenchmarkLog n)
    -> SNetworkId n
    -> CardanoNodeConn
    -> NetworkParameters
    -> NodeToClientVersionData
    -> IO ()
prepareNode tr proxy socketPath np vData = do
    traceWith tr $ MsgSyncStart proxy
    let networkId = networkIdVal (sNetworkId @n)
    sl <- withNetworkLayer nullTracer
            tunedForMainnetPipeliningStrategy
            np socketPath vData sTol $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        waitForNodeSync tr nw
    traceWith tr $ MsgSyncCompleted proxy sl

-- | Regularly poll the wallets to monitor syncing progress. Block until all
-- wallets reach the given percentage.
waitForWalletSyncTo
    :: forall s n
    .  Percentage
    -> Tracer IO (BenchmarkLog n)
    -> SNetworkId n
    -> WalletLayer IO s
    -> WalletId
    -> GenesisParameters
    -> NodeToClientVersionData
    -> IO ()
waitForWalletSyncTo targetSync tr proxy walletLayer wid gp vData = do
    posixNow <- utcTimeToPOSIXSeconds <$> getCurrentTime
    progress <-  do
        w <- fst' <$> W.readWallet walletLayer
        syncProgress nl (slotNo $ currentTip w)
    traceWith tr $ MsgRestorationTick posixNow progress
    threadDelay 1000000
    unless (progress > Syncing (Quantity targetSync))
        $ waitForWalletSyncTo targetSync tr proxy walletLayer wid gp vData
  where
    WalletLayer _ _ nl _ _ = walletLayer
    fst' (x,_,_) = x

reportProgress
    :: NetworkLayer IO block
    -> Tracer IO (BenchmarkLog n)
    -> Percentage
    -> IO SlotNo
    -> IO ()
reportProgress nw tr targetSync readSlot =  do
    posixNow <- utcTimeToPOSIXSeconds <$> getCurrentTime
    progress <- readSlot >>= syncProgress nw
    traceWith tr $ MsgRestorationTick posixNow progress
    threadDelay 1000000
    if progress > Syncing (Quantity targetSync)
        then return ()
        else reportProgress nw tr targetSync readSlot

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: forall n. Tracer IO (BenchmarkLog n)
    -> NetworkLayer IO Block
    -> IO SlotNo
waitForNodeSync tr nw = loop 960 -- allow 240 minutes for first tip
  where
    loop :: Int -> IO SlotNo
    loop retries = do
        nodeTip <- currentNodeTip nw
        if slotNo nodeTip /= 0 then do
            prog <- syncProgress nw $ slotNo nodeTip
            traceWith tr $ MsgNodeTipTick nodeTip prog
            if prog == Ready
                then pure (slotNo nodeTip)
                else do
                    -- 2 seconds poll interval
                    threadDelay 2000000
                    loop retries
        else
            if retries > 0 then do
                 let delay = 15000000
                 traceWith tr $ MsgRetryShortly delay
                 threadDelay delay
                 loop (retries - 1)
            else throwString "Gave up in waitForNodeSync, waiting a tip"

data BenchmarkLog (n :: NetworkDiscriminant)
    = MsgNodeTipTick BlockHeader SyncProgress
    | MsgRestorationTick POSIXTime SyncProgress
    | MsgSyncStart (SNetworkId n)
    | MsgSyncCompleted (SNetworkId n) SlotNo
    | MsgRetryShortly Int
    deriving (Show, Eq)

instance HasPrivacyAnnotation (BenchmarkLog n)
instance HasSeverityAnnotation (BenchmarkLog n) where
    getSeverityAnnotation = \case
        MsgNodeTipTick{} -> Info
        MsgRestorationTick{} -> Info
        MsgSyncStart{} -> Info
        MsgSyncCompleted{} -> Info
        MsgRetryShortly{} -> Info

instance HasSNetworkId n => ToText (BenchmarkLog n) where
    toText = \case
        MsgNodeTipTick tip progress ->
            "Initial node synchronization: "
                +| progress |+ " "
                +| tip ||+ ""
        MsgRestorationTick _posixTime progressList ->
            "Restoring: "
                +| progressList |+ ""
        MsgSyncStart _ ->
            "Syncing "
                +| network |+ " node... "
        MsgSyncCompleted _ tip ->
            "Completed sync of "
                +| network |+ " up to " +|| tip ||+ ""
        MsgRetryShortly delay ->
            "Fetching tip failed, retrying in "
                +|| (delay `div` 1000) ||+ "ms"
        where network = networkDiscriminantVal $ sNetworkId @n

-- | Format a type-level per-myriad number as percent
--
-- >>> showPercentFromPermyriad (Proxy @1)
-- "0.01"
--
-- >>> showPercentFromPermyriad (Proxy @0)
-- "0"
--
-- Unfortunately we have:
--
-- >>> showPercentFromPermyriad (Proxy @10)
-- "0.10"
--
-- rather than "0.1", but we'll have to live with it.
showPercentFromPermyriad :: forall (p :: Nat) . KnownNat p => Proxy p -> Text
showPercentFromPermyriad =
    T.pack . display . (/100) . toRational . natVal
  where
    -- I cannot find a haskell way to format a rational with as few decimals as
    -- possible, so this will have to do:
    display :: Rational -> String
    display 0 = "0"
    display x = showFFloat @Double (Just 2) (fromRat x) ""

sTol :: SyncTolerance
sTol = mkSyncTolerance 3600

guardIsRecentEra :: AnyCardanoEra -> IO AnyRecentEra
guardIsRecentEra (Cardano.AnyCardanoEra era) = case era of
    Cardano.ConwayEra -> pure $ Write.AnyRecentEra Write.RecentEraConway
    Cardano.BabbageEra -> pure $ Write.AnyRecentEra Write.RecentEraBabbage
    Cardano.AlonzoEra -> invalidEra
    Cardano.MaryEra -> invalidEra
    Cardano.AllegraEra -> invalidEra
    Cardano.ShelleyEra -> invalidEra
    Cardano.ByronEra -> invalidEra
    where
    invalidEra = throwIO $ W.ExceptionWriteTxEra $ W.ErrNodeNotYetInRecentEra $
        Cardano.AnyCardanoEra era

withNonEmptyUTxO :: Wallet s -> Set Tx -> e -> IO a -> IO (Either e a)
withNonEmptyUTxO wallet pendingTxs failure action
    | emptyUTxO = pure (Left failure)
    | otherwise = Right <$> action
  where
    emptyUTxO = UTxO.null (availableUTxO pendingTxs wallet)

benchEstimateTxFee
    :: forall n s.
        ( AddressBookIso s
        , WalletFlavor s
        , Excluding '[SharedKey] (KeyOf s)
        , CredFromOf s ~ 'CredFromKeyK
        )
    => SNetworkId n
    -> WalletLayer IO s
    -> IO Time
benchEstimateTxFee network (WalletLayer _ _ netLayer txLayer dbLayer) =
    fmap snd <$> bench "estimate tx fee" $ do
        (Write.InAnyRecentEra _era protocolParams, timeTranslation)
            <- W.readNodeTipStateForTxWrite netLayer
        -- For an output with zero ada, the transactionFee function should
        -- automatically assign a minimal amount of lovelace to the output
        -- before balancing the transaction and computing the fee:
        let bundleWithZeroAda = TokenBundle.fromCoin (Coin 0)
        let outputWithZeroAda = TxOut (dummyAddress network) bundleWithZeroAda
        W.transactionFee @s
            dbLayer
            protocolParams
            txLayer
            timeTranslation
            dummyChangeAddressGen
            defaultTransactionCtx
            (PreSelection [outputWithZeroAda])

data CannotEstimateFeeForWalletWithEmptyUTxO =
    CannotEstimateFeeForWalletWithEmptyUTxO
    deriving (Show, Generic)

instance Buildable CannotEstimateFeeForWalletWithEmptyUTxO where
    build = genericF

instance ToJSON CannotEstimateFeeForWalletWithEmptyUTxO where
    toJSON = genericToJSON Aeson.defaultOptions
