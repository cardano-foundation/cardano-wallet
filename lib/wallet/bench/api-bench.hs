{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Copyright: © 2023–2023 IOHK

This benchmark measures the running time of various functions from
the main module "Cardano.Wallet". The benchmark loads wallet states
from database files inside a given directory. These wallets are not
actively synchronized to the blockchain.

This benchmark design is useful for the following tasks:

* Compare the performance for wallet states with different ``shapes'',
  e.g. wallets with many addresses and few UTxO, or vice-versa.
* Detect performance regressions or improvements as the codebase changes.
* Develop performance improvements, by enabling speedy testing of
  hypotheses on the code. (In order to keep recompilation times
  short, the benchmark exercises the "Cardano.Wallet" module,
  but not the full HTTP endpoints.)

This benchmark is not so useful for the following tasks:

* Accurately measure the absolute running time for wallets of
  realistic sizes. (Unless we generate a corresponding wallet state file.)
* Accurately measure the absolute running time of all HTTP API endpoints,
  as this is at odds with the goal of allowing speedy hypothesis testing.
  (We want the "Cardano.Wallet" module to faithfully represent the API
  and expect that only a small cost of serializing/deserializing to/from JSON
  is incurred comparing to the HTTP API.)

-}
module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Wallet.BenchShared
    ( Time, bench, initBenchmarkLogging, runBenchmarks )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.DB.Layer
    ( PersistAddressBook )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyNetworkLayer
    , dummyProtocolParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( BoundedAddressLength (..)
    , DelegationAddress (..)
    , Depth (..)
    , PersistPrivateKey
    , WalletKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState (..) )
import Cardano.Wallet.Primitive.Model
    ( totalUTxO )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, hoistTimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..), WalletId, WalletMetadata (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( HistogramBar (..), UTxOStatistics (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminant (..), SNetworkId (..) )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( networkIdVal )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTagFor (..), newTransactionLayer )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Data.Aeson
    ( ToJSON (..), genericToJSON, (.=) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Fmt
    ( Buildable, Builder, blockListF', build, genericF, nameF, pretty )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Say
    ( sayErr )

import qualified Cardano.Api as Cardano
import qualified Cardano.Tx.Balance.Internal.CoinSelection as CoinSelection
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.DB.Layer as DB
import qualified Cardano.Wallet.DB.Layer as Sqlite
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Transaction as Tx
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Environment as Sys
import qualified System.Exit as Sys

{-------------------------------------------------------------------------------
    Main function
-------------------------------------------------------------------------------}
main :: IO ()
main = do
    Sys.getArgs >>= \case
        [] -> do
            sayErr "expected 1 argument: directory with wallet database files"
            Sys.exitFailure
        (dir: _) -> benchmarkApi dir

benchmarkApi :: FilePath -> IO ()
benchmarkApi dir = do
    (_, tr) <- initBenchmarkLogging "wallet" Notice
    runBenchmarks
        [ benchmarkWallets "sequential"
            dir tr (Proxy @('Testnet 0)) benchmarksSeq
        , benchmarkWallets "shared"
            dir tr (Proxy @('Testnet 0)) benchmarksShared
        , benchmarkWallets "random"
            dir tr (Proxy @('Testnet 0)) benchmarksRnd
        ]

{-------------------------------------------------------------------------------
    Wallet API benchmarks
-------------------------------------------------------------------------------}
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
        [ "utxo" .= jsonFromUTxOStatistics utxo
        , "addresses" .= addresses
        , "transactions" .= transactions
        ]

jsonFromUTxOStatistics :: UTxOStatistics -> Aeson.Value
jsonFromUTxOStatistics UTxOStatistics{histogram,allStakes,boundType} =
    Aeson.object
        [ "total" .=
            (Quantity (fromIntegral allStakes) :: Quantity "lovelace" Natural)
        , "scale" .= genericToJSON Aeson.defaultOptions boundType
        , "distribution" .=
            Map.fromList (map (\(HistogramBar k v)-> (k,v)) histogram)
        ]

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        SeqState
-------------------------------------------------------------------------------}
data BenchSeqResults = BenchSeqResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , createMigrationPlanTime :: Time
    , delegationFeeTime :: Time
    , selectAssetsTime :: Time
    } deriving (Show, Generic)

instance Buildable BenchSeqResults where
    build = genericF

instance ToJSON BenchSeqResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksSeq
    :: forall (n :: NetworkDiscriminant) s k ktype.
        ( s ~ SeqState n k
        , k ~ ShelleyKey
        , ktype ~ 'CredFromKeyK
        , HasSNetworkId n
        , DelegationAddress n k ktype
        )
    => BenchmarkConfig n s k ktype
    -> IO BenchSeqResults
benchmarksSeq BenchmarkConfig{benchmarkName,ctx,wid} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet @_ @s @k ctx wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ fmap length
        $ unsafeRunExceptT
        $ W.getWalletUtxoSnapshot @_ @s @k @ktype ctx wid

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses @_ @s @k ctx wid (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ fmap length
        $ unsafeRunExceptT
        $ W.listAssets @s @k ctx wid

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending (Just 50)

    let era = Cardano.anyCardanoEra Cardano.BabbageEra
    (_, createMigrationPlanTime) <- bench "createMigrationPlan"
        $ unsafeRunExceptT
        $ W.createMigrationPlan @_ @k @s ctx era wid Tx.NoWithdrawal

    (_, delegationFeeTime) <- bench "delegationFee"
        $ W.delegationFee @_ @k @n
            (dbLayer ctx) (networkLayer ctx) (transactionLayer ctx)
            (timeInterpreter (networkLayer ctx))
            (Write.AnyRecentEra Write.RecentEraBabbage)
            (W.defaultChangeAddressGen (delegationAddress @n) (Proxy @k))
            wid

    (_, selectAssetsTime) <- bench "selectAssets"
        $ selectAssets @_ @s @k @ktype (Proxy @n) ctx wid

    pure BenchSeqResults
        { benchName = benchmarkName
        , walletOverview = WalletOverview{utxo,addresses,transactions}
        , readWalletTime
        , getWalletUtxoSnapshotTime
        , listAddressesTime
        , listAssetsTime
        , listTransactionsTime
        , listTransactionsLimitedTime
        , createMigrationPlanTime
        , delegationFeeTime
        , selectAssetsTime
        }

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        SharedState
-------------------------------------------------------------------------------}
data BenchSharedResults = BenchSharedResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , selectAssetsTime :: Time
    } deriving (Show, Generic)

instance Buildable BenchSharedResults where
    build = genericF

instance ToJSON BenchSharedResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksShared
    :: forall (n :: NetworkDiscriminant) s k ktype.
        ( s ~ SharedState n k
        , k ~ SharedKey
        , ktype ~ 'CredFromScriptK
        , HasSNetworkId n
        )
    => BenchmarkConfig n s k ktype
    -> IO BenchSharedResults
benchmarksShared BenchmarkConfig{benchmarkName,ctx,wid} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet @_ @s @k ctx wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ fmap length
        $ unsafeRunExceptT
        $ W.getWalletUtxoSnapshot @_ @s @k @ktype ctx wid

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses @_ @s @k ctx wid (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ fmap length
        $ unsafeRunExceptT
        $ W.listAssets @s @k ctx wid

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending (Just 50)

    (_, selectAssetsTime) <- bench "selectAssets"
        $ selectAssets @_ @s @k @ktype (Proxy @n) ctx wid

    pure BenchSharedResults
        { benchName = benchmarkName
        , walletOverview = WalletOverview{utxo,addresses,transactions}
        , readWalletTime
        , getWalletUtxoSnapshotTime
        , listAddressesTime
        , listAssetsTime
        , listTransactionsTime
        , listTransactionsLimitedTime
        , selectAssetsTime
        }

{-------------------------------------------------------------------------------
    Wallet API benchmarks
        RndState
-------------------------------------------------------------------------------}
data BenchRndResults = BenchRndResults
    { benchName :: Text
    , walletOverview :: WalletOverview
    , readWalletTime :: Time
    , getWalletUtxoSnapshotTime :: Time
    , listAddressesTime :: Time
    , listAssetsTime :: Time
    , listTransactionsTime :: Time
    , listTransactionsLimitedTime :: Time
    , createMigrationPlanTime :: Time
    , selectAssetsTime :: Time
    } deriving (Show, Generic)

instance Buildable BenchRndResults where
    build = genericF

instance ToJSON BenchRndResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksRnd
    :: forall (n :: NetworkDiscriminant) s k ktype.
        ( s ~ RndState n
        , k ~ ByronKey
        , ktype ~ 'CredFromKeyK
        , HasSNetworkId n
        )
    => BenchmarkConfig n s k ktype
    -> IO BenchRndResults
benchmarksRnd BenchmarkConfig{benchmarkName,ctx,wid} = do
    ((cp, pending), readWalletTime) <- bench "readWallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet @_ @s @k ctx wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $
        pure $ UTxOStatistics.compute (totalUTxO pending cp)

    (_, getWalletUtxoSnapshotTime) <- bench "getWalletUtxoSnapshot"
        $ fmap length
        $ unsafeRunExceptT
        $ W.getWalletUtxoSnapshot @_ @s @k @ktype ctx wid

    (addresses, listAddressesTime) <- bench "listAddresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses @_ @s @k ctx wid (const pure)

    (_, listAssetsTime) <- bench "listAssets"
        $ fmap length
        $ unsafeRunExceptT
        $ W.listAssets @s @k ctx wid

    (transactions, listTransactionsTime) <- bench "listTransactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending Nothing

    (_, listTransactionsLimitedTime) <- bench "listTransactions (max_count=50)"
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k ctx
            wid Nothing Nothing Nothing Descending (Just 50)

    let era = Cardano.anyCardanoEra Cardano.BabbageEra
    (_, createMigrationPlanTime) <- bench "createMigrationPlan"
        $ unsafeRunExceptT
        $ W.createMigrationPlan @_ @k @s ctx era wid Tx.NoWithdrawal

    (_, selectAssetsTime) <- bench "selectAssets"
        $ selectAssets @_ @s @k @ktype (Proxy @n) ctx wid

    pure BenchRndResults
        { benchName = benchmarkName
        , walletOverview = WalletOverview{utxo,addresses,transactions}
        , readWalletTime
        , getWalletUtxoSnapshotTime
        , listAddressesTime
        , listAssetsTime
        , listTransactionsTime
        , listTransactionsLimitedTime
        , createMigrationPlanTime
        , selectAssetsTime
        }

{-------------------------------------------------------------------------------
    Custom Wallet API functions
-------------------------------------------------------------------------------}
selectAssets
    :: forall (n :: NetworkDiscriminant) s (k :: Depth -> * -> *) ktype
    .   ( WalletKey k
        , NFData s
        , Show s
        , HasSNetworkId n
        , BoundedAddressLength k
        )
    => Proxy n
    -> MockWalletLayer IO s k ktype
    -> WalletId
    -> IO (Either String CoinSelection.Selection)
selectAssets networkId ctx wid = do
    let tr = trMessageText (tracer ctx)
    let out = TxOut (dummyAddress networkId) (TokenBundle.fromCoin $ Coin 1)
    (utxoAvailable, wallet, pendingTxs) <-
        unsafeRunExceptT $ W.readWalletUTxOIndex @_ @s @k ctx wid
    pp <- currentProtocolParameters (networkLayer ctx)
    era <- currentNodeEra (networkLayer ctx)
    runExceptT $ withExceptT show $ W.selectAssets @_ @s @k @ktype
        (contramap (W.MsgWallet . W.MsgBalanceTx) tr)
        (transactionLayer ctx)
        era
        pp
        W.SelectAssetsParams
        { outputs = [out]
        , pendingTxs
        , randomSeed = Nothing
        , txContext = Tx.defaultTransactionCtx
        , utxoAvailableForInputs = UTxOSelection.fromIndex utxoAvailable
        , utxoAvailableForCollateral = UTxOIndex.toMap utxoAvailable
        , wallet
        , selectionStrategy = CoinSelection.SelectionStrategyOptimal
        } $ \_state -> id

dummyAddress
    :: forall (n :: NetworkDiscriminant)
     . HasSNetworkId n
    => Proxy n
    -> Address
dummyAddress _proxy = case sNetworkId @n of
    SMainnet ->
        Address $ BS.pack $ 0 : replicate 56 0
    _ ->
        Address $ BS.pack $ 1 : replicate 56 0

{-------------------------------------------------------------------------------
    Benchmark Harness
-------------------------------------------------------------------------------}
newtype SomeBenchmarkResults = SomeBenchmarkResults Builder

instance Buildable SomeBenchmarkResults where
    build (SomeBenchmarkResults x) = x

data BenchmarkConfig (n :: NetworkDiscriminant) s k ktype =
    BenchmarkConfig
        { benchmarkName :: Text
        , networkId :: Proxy n
        , ctx :: MockWalletLayer IO s k ktype
        , wid :: WalletId
        }

-- | Run benchmarks on all wallet databases in a given directory.
benchmarkWallets
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> * -> *) ktype s results
     . ( NFData s
       , Show s
       , PersistAddressBook s
       , WalletKey k
       , PersistPrivateKey (k 'RootK)
       , TxWitnessTagFor k
       , Buildable results
       , ToJSON results
       , HasSNetworkId n
       )
    => Text
        -- ^ Benchmark name (used for naming resulting files)
    -> FilePath
        -- ^ Directory from which to load database files
    -> Trace IO Text
        -- ^ For wallet tracing
    -> Proxy n
    -> ( BenchmarkConfig n s k ktype -> IO results )
        -- ^ Benchmark to run
    -> IO [SomeBenchmarkResults]
benchmarkWallets benchName dir walletTr proxy action = do
    withWalletsFromDirectory dir walletTr proxy
        $ \ctx@(MockWalletLayer{dbLayer=DB.DBLayer{atomically,readWalletMeta}}) wid
        -> do
            Just (WalletMetadata{name},_) <- atomically $ readWalletMeta wid
            let config = BenchmarkConfig
                    { benchmarkName = benchName <> " " <> pretty name
                    , networkId = proxy
                    , ctx
                    , wid
                    }
            sayErr $ "*** " <> benchmarkName config
            results <- action config
            saveBenchmarkPoints (benchmarkName config) results
            pure $ SomeBenchmarkResults (build results)
  where
    saveBenchmarkPoints :: ToJSON a => Text -> a -> IO ()
    saveBenchmarkPoints benchname =
        Aeson.encodeFile (T.unpack benchname <> ".json")

data MockWalletLayer m s (k :: Depth -> * -> *) ktype =
    MockWalletLayer
        { networkLayer :: NetworkLayer m Read.Block
        , transactionLayer :: TransactionLayer k ktype SealedTx
        , dbLayer :: DBLayer m s k
        , tracer :: Trace IO Text
        } deriving (Generic)

mockNetworkLayer :: NetworkLayer IO Read.Block
mockNetworkLayer = dummyNetworkLayer
    { timeInterpreter = hoistTimeInterpreter liftIO mockTimeInterpreter
    , currentSlottingParameters = pure dummySlottingParameters
    , currentProtocolParameters = pure dummyProtocolParameters
    , currentNodeEra = pure $ Cardano.anyCardanoEra Cardano.BabbageEra
    }

mockTimeInterpreter :: TimeInterpreter IO
mockTimeInterpreter = dummyTimeInterpreter

withWalletsFromDirectory
    :: forall (n :: NetworkDiscriminant) s k ktype a
     . ( PersistAddressBook s
       , PersistPrivateKey (k 'RootK)
       , WalletKey k
       , TxWitnessTagFor k
       , HasSNetworkId n
       )
    => FilePath
        -- ^ Directory of database files
    -> Trace IO Text
    -> Proxy n
    -> (MockWalletLayer IO s k ktype -> WalletId -> IO a)
    -> IO [a]
withWalletsFromDirectory dir tr _proxy action = do
    DB.DBFactory{listDatabases,withDatabase}
        <- DB.newDBFactory tr' migrationDefaultValues ti (Just dir)
    wids <- listDatabases
    forM wids $ \wid ->
        withDatabase wid (flip action wid . mkMockWalletLayer)
  where
    mkMockWalletLayer db =
        MockWalletLayer mockNetworkLayer tl db tr
    ti = mockTimeInterpreter
    tl = newTransactionLayer @k (networkIdVal (sNetworkId @n))
    tr' = trMessageText tr
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 1000
        , Sqlite.defaultHardforkEpoch = Nothing
        , Sqlite.defaultKeyDeposit = Coin 0
        }
