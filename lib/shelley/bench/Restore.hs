{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark measuring how long restoration takes for different wallets.
--
-- Easiest run using
-- @
--     $ export NODE_DB="node-db-testnet"
--     $ nix-build -A benchmarks.cardano-wallet.restore -o restore && ./restore/bin/restore testnet
-- @
--
-- or
-- @
--     $ ./.buildkite/bench-restore.sh shelley testnet
-- @
--
-- since it relies on lots of configuration most most easily retrieved with nix.

module Main where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.DB.Sqlite
    ( destroyDBLayer )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet
    ( WalletLayer (..), WalletLog (..) )
import Cardano.Wallet.Api.Types
    ( toApiUtxoStatistics )
import Cardano.Wallet.BenchShared
    ( RestoreBenchArgs (..)
    , Time
    , argsNetworkDir
    , bench
    , execBenchWithNode
    , runBenchmarks
    )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, newDBLayer )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network
    ( FollowLog (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal (..)
    , Passphrase (..)
    , PaymentAddress
    , PersistPrivateKey
    , WalletKey
    , digest
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery, GenChange (..), IsOurs, IsOwned, KnownAddresses )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndAnyState, mkRndAnyState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, SeqAnyState (..), mkAddressPoolGap, mkSeqAnyState )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip, getState, totalUTxO )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), mkSyncTolerance, syncProgress )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , ChimericAccount
    , Coin (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , SlotNo (..)
    , SortOrder (..)
    , TxOut (..)
    , UTxOStatistics (..)
    , WalletId (..)
    , WalletName (..)
    , computeUtxoStatistics
    , log10
    )
import Cardano.Wallet.Shelley
    ( HasNetworkId (..), SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData, Shelley, emptyGenesis, fromCardanoBlock )
import Cardano.Wallet.Shelley.Launch
    ( NetworkConfiguration (..), parseGenesisData )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTagFor (..), newTransactionLayer )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic, unsafeRunExceptT )
import Control.Arrow
    ( first )
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( bracket, evaluate, throwIO )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Control.Tracer
    ( Tracer (..), traceWith )
import Data.Aeson
    ( ToJSON (..), genericToJSON, (.=) )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock.POSIX
    ( getCurrentTime, utcTimeToPOSIXSeconds )
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
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat )
import Say
    ( sayErr )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hFlush, withFile )
import System.IO.Temp
    ( withSystemTempFile )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = execBenchWithNode argsNetworkConfig cardanoRestoreBench

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
cardanoRestoreBench :: Trace IO Text -> NetworkConfiguration -> FilePath -> IO ()
cardanoRestoreBench tr c socketFile = do
    (SomeNetworkDiscriminant networkProxy, np, vData, _b)
        <- unsafeRunExceptT $ parseGenesisData c

    let network = networkDescription networkProxy
    sayErr $ "Network: " <> network

    prepareNode (trMessageText tr) networkProxy socketFile np vData
    runBenchmarks
        [ bench_restoration @_ @ShelleyKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletSeq "0-percent-seq" $ mkSeqAnyState' @0 networkProxy)
            benchmarksSeq

        , bench_restoration @_ @ByronKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletRnd "0-percent-rnd" $ mkRndAnyState @0)
            benchmarksRnd

        , bench_restoration @_ @ByronKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletRnd "0.1-percent-rnd" $ mkRndAnyState @1)
            benchmarksRnd

        , bench_restoration @_ @ByronKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletRnd "0.2-percent-rnd" $ mkRndAnyState @2)
            benchmarksRnd

        , bench_restoration @_ @ByronKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletRnd "0.4-percent-rnd" $ mkRndAnyState @4)
            benchmarksRnd

         , bench_restoration @_ @ShelleyKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletSeq "0.1-percent-seq" $ mkSeqAnyState' @1 networkProxy)
            benchmarksSeq

         , bench_restoration @_ @ShelleyKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletSeq "0.2-percent-seq" $ mkSeqAnyState' @2 networkProxy)
            benchmarksSeq

         , bench_restoration @_ @ShelleyKey
            networkProxy
            (trMessageText tr)
            socketFile
            np
            vData
            (walletSeq "0.4-percent-seq" $ mkSeqAnyState' @4 networkProxy)
            benchmarksSeq
        ]
  where
    walletRnd
        :: Text
        -> (ByronKey 'RootK XPrv -> Int -> s)
        -> (WalletId, WalletName, s)
    walletRnd wname mkState =
        let
            xprv = Byron.generateKeyFromSeed seed mempty
            wid = WalletId $ digest $ publicKey xprv
            rngSeed = 0
            s = mkState xprv rngSeed
        in
            (wid, WalletName wname, s)

    walletSeq
        :: Text
        -> ((ShelleyKey 'RootK XPrv, Passphrase "encryption") -> AddressPoolGap -> s)
        -> (WalletId, WalletName, s)
    walletSeq wname mkState =
        let
            xprv = Shelley.generateKeyFromSeed (seed, Nothing) mempty
            wid = WalletId $ digest $ publicKey xprv
            Right gap = mkAddressPoolGap 20
            s = mkState (xprv, mempty) gap
        in
            (wid, WalletName wname, s)

    mkSeqAnyState'
        :: forall (p :: Nat) (n :: NetworkDiscriminant). ()
        => Proxy n
        -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
        -> AddressPoolGap
        -> SeqAnyState n ShelleyKey p
    mkSeqAnyState' _ = mkSeqAnyState @p @n

    networkDescription :: forall n. (NetworkDiscriminantVal n) => Proxy n -> Text
    networkDescription _ = networkDiscriminantVal @n


seed :: SomeMnemonic
seed = SomeMnemonic . unsafeMkMnemonic @15 $ T.words
    "involve key curtain arrest fortune custom lens marine before \
    \material wheel glide cause weapon wrap"

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
    , importOneAddressTime :: Time
    , importManyAddressesTime :: Time
    , estimateFeesTime :: Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchRndResults where
    build = genericF

instance ToJSON BenchRndResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksRnd
    :: forall (n :: NetworkDiscriminant) s t k p.
        ( s ~ RndAnyState n p
        , t ~ IO Shelley
        , k ~ ByronKey
        , PaymentAddress n k
        , NetworkDiscriminantVal n
        , KnownNat p
        )
    => Proxy n
    -> WalletLayer s t k
    -> WalletId
    -> WalletName
    -> Time
    -> IO BenchRndResults
benchmarksRnd _ w wid wname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet w wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $ do
        pure $ computeUtxoStatistics log10 (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses w wid (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k @t w wid Nothing Nothing Nothing Descending

    (_, estimateFeesTime) <- bench "estimate tx fee" $ do
        let out = TxOut (dummyAddress @n) (Coin 1)
        runExceptT $ withExceptT show $ W.estimateFeeForPayment @_ @s @t @k
            w wid (out :| []) (Quantity 0) Nothing

    oneAddress <- genAddresses 1 cp
    (_, importOneAddressTime) <- bench "import one addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses @_ @s @k w wid oneAddress

    manyAddresses <- genAddresses 1000 cp
    (_, importManyAddressesTime) <- bench "import many addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses @_ @s @k w wid manyAddresses

    let walletOverview = WalletOverview{utxo,addresses,transactions}

    pure BenchRndResults
        { benchName = getWalletName wname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
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
        xprv = Byron.generateKeyFromSeed seed mempty

data BenchSeqResults = BenchSeqResults
    { benchName :: Text
    , restoreTime :: Time
    , readWalletTime :: Time
    , listAddressesTime :: Time
    , listTransactionsTime :: Time
    , estimateFeesTime :: Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchSeqResults where
    build = genericF

instance ToJSON BenchSeqResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksSeq
    :: forall (n :: NetworkDiscriminant) s t k p.
        ( s ~ SeqAnyState n k p
        , t ~ IO Shelley
        , k ~ ShelleyKey
        , PaymentAddress n k
        , NetworkDiscriminantVal n
        , KnownNat p
        )
    => Proxy n
    -> WalletLayer s t k
    -> WalletId
    -> WalletName
    -> Time
    -> IO BenchSeqResults
benchmarksSeq _ w wid wname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet w wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $ do
        pure $ computeUtxoStatistics log10 (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses w wid (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k @t w wid Nothing Nothing Nothing Descending

    (_, estimateFeesTime) <- bench "estimate tx fee" $ do
        let out = TxOut (dummyAddress @n) (Coin 1)
        runExceptT $ withExceptT show $ W.estimateFeeForPayment @_ @s @t @k
            w wid (out :| []) (Quantity 0) Nothing

    let walletOverview = WalletOverview{utxo,addresses,transactions}

    pure BenchSeqResults
        { benchName = getWalletName wname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
        , estimateFeesTime
        , walletOverview
        }

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> * -> *) s t results.
        ( IsOurs s Address
        , IsOurs s ChimericAccount
        , IsOwned s k
        , WalletKey k
        , NFData s
        , Show s
        , PersistState s
        , CompareDiscovery s
        , KnownAddresses s
        , PersistPrivateKey (k 'RootK)
        , NetworkDiscriminantVal n
        , HasNetworkId n
        , TxWitnessTagFor k
        , t ~ IO Shelley
        , Buildable results
        , ToJSON results
        )
    => Proxy n
    -> Tracer IO (BenchmarkLog n)
    -> FilePath
       -- ^ Socket path
    -> NetworkParameters
    -> NodeVersionData
    -> (WalletId, WalletName, s)
    -> (Proxy n -> WalletLayer s t k -> WalletId -> WalletName -> Time -> IO results)
    -> IO SomeBenchmarkResults
bench_restoration proxy tr socket np vData (wid, wname, s) benchmarks = do
    let networkId = networkIdVal proxy
    let tl = newTransactionLayer @k @(IO Shelley) networkId
    withNetworkLayer nullTracer np socket vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        withBenchDBLayer @s @k (timeInterpreter nw) $ \db -> do
            withFile timelogFilepath WriteMode $ \h -> do
                -- Use a custom tracer to output (time, blockHeight) to a file
                -- each time we apply blocks.
                let fileTr = Tracer $ \msg -> do
                        liftIO . B8.hPut h . T.encodeUtf8 . (<> "\n") $ msg
                        hFlush h
                let w = WalletLayer
                        (traceProgressForPlotting fileTr)
                        (emptyGenesis gp, np, mkSyncTolerance 3600)
                        nw
                        tl
                        db
                wallet <- unsafeRunExceptT $ W.createWallet w wid wname s
                void $ forkIO $ unsafeRunExceptT $ W.restoreWallet @_ @s @t @k w wid
                (_, restorationTime) <- bench "restoration" $ do
                    waitForWalletSync tr proxy w wallet gp vData

                results <- benchmarks proxy w wid wname restorationTime
                Aeson.encodeFile resultsFilepath results
                unsafeRunExceptT (W.deleteWallet w wid)
                    $> SomeBenchmarkResults results
  where
    basename = T.unpack (coerce wname)
    timelogFilepath = basename <> ".timelog"
    resultsFilepath = basename <> ".json"

dummyAddress
    :: forall (n :: NetworkDiscriminant). NetworkDiscriminantVal n
    => Address
dummyAddress
    | networkDiscriminantVal @n == networkDiscriminantVal @'Mainnet =
        Address $ BS.pack $ 0 : replicate 56 0
    | otherwise =
        Address $ BS.pack $ 1 : replicate 56 0

traceProgressForPlotting :: Tracer IO Text -> Tracer IO WalletLog
traceProgressForPlotting tr = Tracer $ \case
    MsgFollow (MsgApplyBlocks bs) -> do
        let tip = pretty . getQuantity . blockHeight . NE.last $ bs
        time <- pretty . utcTimeToPOSIXSeconds <$> getCurrentTime
        traceWith tr (time <> " " <> tip)
    _ -> return ()

withBenchDBLayer
    :: forall s k a.
        ( IsOwned s k
        , NFData s
        , Show s
        , PersistState s
        , IsOurs s ChimericAccount
        , IsOurs s Address
        , PersistPrivateKey (k 'RootK)
        )
    => TimeInterpreter IO
    -> (DBLayer IO s k -> IO a)
    -> IO a
withBenchDBLayer ti action =
    withSystemTempFile "bench.db" $ \dbFile _ -> do
        let before = newDBLayer nullTracer migrationDefaultValues (Just dbFile) ti
        let after = destroyDBLayer . fst
        bracket before after $ \(_ctx, db) -> action db
  where
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 0
        , Sqlite.defaultHardforkEpoch = Nothing
        }

prepareNode
    :: forall n. (NetworkDiscriminantVal n)
    => Tracer IO (BenchmarkLog n)
    -> Proxy n
    -> FilePath
    -> NetworkParameters
    -> NodeVersionData
    -> IO ()
prepareNode tr proxy socketPath np vData = do
    traceWith tr $ MsgSyncStart proxy
    sl <- withNetworkLayer nullTracer np socketPath vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        waitForNodeSync tr nw
    traceWith tr $ MsgSyncCompleted proxy sl

-- | Regularly poll the wallet to monitor it's syncing progress. Block until the
-- wallet reaches 100%.
waitForWalletSync
    :: forall s t k n. (NetworkDiscriminantVal n)
    => Tracer IO (BenchmarkLog n)
    -> Proxy n
    -> WalletLayer s t k
    -> WalletId
    -> GenesisParameters
    -> NodeVersionData
    -> IO ()
waitForWalletSync tr proxy walletLayer wid gp vData = do
    (w, _, _) <- unsafeRunExceptT $ W.readWallet walletLayer wid
    let tolerance = mkSyncTolerance 3600
    now  <- getCurrentTime
    prog <- syncProgress tolerance (timeInterpreter nl) (currentTip w) now
    case prog of
        Ready -> return ()
        NotResponding -> do
            threadDelay 1000000
            waitForWalletSync tr proxy walletLayer wid gp vData
        Syncing{} -> do
            traceWith tr $ MsgRestorationTick prog
            threadDelay 1000000
            waitForWalletSync tr proxy walletLayer wid gp vData
  where
    WalletLayer _ _ nl _ _ = walletLayer

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: forall n. (NetworkDiscriminantVal n)
    => Tracer IO (BenchmarkLog n)
    -> NetworkLayer IO (IO Shelley) Block
    -> IO SlotNo
waitForNodeSync tr nw = loop 10
  where
    loop :: Int -> IO SlotNo
    loop retries = runExceptT (currentNodeTip nw) >>= \case
        Right nodeTip -> do
            let tolerance = mkSyncTolerance 300
            now  <- getCurrentTime
            prog <- syncProgress tolerance (timeInterpreter nw) nodeTip now
            traceWith tr $ MsgNodeTipTick nodeTip prog
            if prog == Ready
                then pure (slotNo nodeTip)
                else do
                    -- 2 seconds poll interval
                    threadDelay 2000000
                    loop retries
        Left e | retries > 0 -> do
                     let delay = 15000000
                     traceWith tr $ MsgRetryShortly delay
                     threadDelay delay
                     loop (retries - 1)
               | otherwise -> throwIO e

data BenchmarkLog (n :: NetworkDiscriminant)
    = MsgNodeTipTick BlockHeader SyncProgress
    | MsgRestorationTick SyncProgress
    | MsgSyncStart (Proxy n)
    | MsgSyncCompleted (Proxy n) SlotNo
    | MsgRetryShortly Int
    deriving (Show, Eq)

instance HasPrivacyAnnotation (BenchmarkLog n)
instance HasSeverityAnnotation (BenchmarkLog n) where
    getSeverityAnnotation = \case
        MsgNodeTipTick{} -> Info
        MsgRestorationTick{} -> Info
        MsgSyncStart{} -> Info
        MsgSyncCompleted{} -> Info
        MsgRetryShortly{} -> Warning

instance NetworkDiscriminantVal n => ToText (BenchmarkLog n) where
    toText = \case
        MsgNodeTipTick tip progress ->
            "Initial node synchronization: " +| progress |+ " " +| tip ||+""
        MsgRestorationTick progress ->
            "Restoring: "+| progress |+""
        MsgSyncStart _ ->
            "Syncing "+| networkDiscriminantVal @n |+" node... "
        MsgSyncCompleted _ tip ->
            "Completed sync of "+| networkDiscriminantVal @n |+" up to "+|| tip ||+""
        MsgRetryShortly delay ->
            "Fetching tip failed, retrying in " +|| (delay `div` 1000) ||+ "ms"
