{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.DB.Sqlite
    ( destroyDBLayer )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet
    ( WalletLayer (..), WalletLog (..) )
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
    ( CompareDiscovery, IsOurs, IsOwned, KnownAddresses )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( mkRndAnyState, mkRndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap
    , SeqAnyState (..)
    , SeqState (..)
    , mkAddressPoolGap
    , mkSeqAnyState
    , mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Primitive.Model
    ( currentTip, totalUTxO )
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
    , TxOut (..)
    , UTxOStatistics
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
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( bracket, throwIO )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Control.Tracer
    ( Tracer (..), traceWith )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( getCurrentTime, utcTimeToPOSIXSeconds )
import Fmt
    ( Buildable, build, fmt, genericF, pretty, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Nat )
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

    prepareNode networkProxy socketFile np vData
    runBenchmarks
        [ bench_restoration @_ @ShelleyKey
            networkProxy
            tr
            socketFile
            np
            vData
            "seq.timelog"
            (walletSeq "Seq Empty Wallet" $ mkSeqState networkProxy)

        , bench_restoration @_ @ByronKey
            networkProxy
            tr
            socketFile
            np
            vData
            "rnd.timelog"
            (walletRnd "Rnd Empty Wallet" mkRndState)

        , bench_restoration @_ @ByronKey
            networkProxy
            tr
            socketFile
            np
            vData
            "0.1-percent-rnd.timelog"
            (walletRnd "Rnd 0.1% Wallet" $ mkRndAnyState @1)

        , bench_restoration @_ @ByronKey
            networkProxy
            tr
            socketFile
            np
            vData
            "0.2-percent-rnd.timelog"
            (walletRnd "Rnd 0.2% Wallet" $ mkRndAnyState @2)

        , bench_restoration @_ @ByronKey
            networkProxy
            tr
            socketFile
            np
            vData
            "0.4-percent-rnd.timelog"
            (walletRnd "Rnd 0.4% Wallet" $ mkRndAnyState @4)

         , bench_restoration @_ @ShelleyKey
             networkProxy
             tr
             socketFile
             np
             vData
             "0.1-percent-seq.timelog"
             (walletSeq "Seq 0.1% Wallet" $ mkSeqAnyState' @1 networkProxy)

         , bench_restoration @_ @ShelleyKey
             networkProxy
             tr
             socketFile
             np
             vData
             "0.2-percent-seq.timelog"
             (walletSeq "Seq 0.2% Wallet" $ mkSeqAnyState' @2 networkProxy)

         , bench_restoration @_ @ShelleyKey
             networkProxy
             tr
             socketFile
             np
             vData
             "0.4-percent-seq.timelog"
             (walletSeq "Seq 0.4% Wallet" $ mkSeqAnyState' @4 networkProxy)
        ]
  where
    walletRnd
        :: Text
        -> (ByronKey 'RootK XPrv -> Int -> s)
        -> (WalletId, WalletName, s)
    walletRnd wname mkState =
        let
            seed = SomeMnemonic . unsafeMkMnemonic @15 $ T.words
                "involve key curtain arrest fortune custom lens marine before \
                \material wheel glide cause weapon wrap"
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
            seed = SomeMnemonic . unsafeMkMnemonic @15 $ T.words
                "involve key curtain arrest fortune custom lens marine before \
                \material wheel glide cause weapon wrap"
            xprv = Shelley.generateKeyFromSeed (seed, Nothing) mempty
            wid = WalletId $ digest $ publicKey xprv
            Right gap = mkAddressPoolGap 20
            s = mkState (xprv, mempty) gap
        in
            (wid, WalletName wname, s)

    mkSeqState
        :: forall (n :: NetworkDiscriminant). ()
        => Proxy n
        -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
        -> AddressPoolGap
        -> SeqState n ShelleyKey
    mkSeqState _ = mkSeqStateFromRootXPrv @n

    mkSeqAnyState'
        :: forall (p :: Nat) (n :: NetworkDiscriminant). ()
        => Proxy n
        -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
        -> AddressPoolGap
        -> SeqAnyState n ShelleyKey p
    mkSeqAnyState' _ = mkSeqAnyState @p @n

    networkDescription :: forall n. (NetworkDiscriminantVal n) => Proxy n -> Text
    networkDescription _ = networkDiscriminantVal @n

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

data BenchResults = BenchResults
    { qualifier :: Text
    , restorationTime :: Time
    , listingAddressesTime :: Time
    , estimatingFeesTime :: Time
    , utxoStatistics :: UTxOStatistics
    } deriving (Show, Generic)

instance Buildable BenchResults where
    build = genericF

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> * -> *) s t.
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
        )
    => Proxy n
    -> Trace IO Text
    -> FilePath
       -- ^ Socket path
    -> NetworkParameters
    -> NodeVersionData
    -> FilePath
       -- ^ Log output
    -> (WalletId, WalletName, s)
    -> IO BenchResults
bench_restoration proxy tracer socketPath np vData progressLogFile (wid, wname, s) = do
    let networkText = networkDiscriminantVal @n
    let networkId = networkIdVal proxy
    let tl = newTransactionLayer @k @(IO Shelley) networkId
    withNetworkLayer nullTracer np socketPath vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        withBenchDBLayer @s @k tracer (timeInterpreter nw) $ \db -> do
            BlockHeader sl _ _ _ <- unsafeRunExceptT $ currentNodeTip nw
            sayErr . fmt $ networkText ||+ " tip is at " +|| sl ||+ ""

            withFile progressLogFile WriteMode $ \h -> do
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
                    waitForWalletSync w wallet gp vData

                (utxoStatistics, _) <- bench "utxo statistics" $ do
                    (wallet', _, pending) <- unsafeRunExceptT $ W.readWallet w wid
                    pure $ computeUtxoStatistics log10 (totalUTxO pending wallet')

                (_, listingAddressesTime) <- bench "list addresses" $
                    unsafeRunExceptT $ W.listAddresses w wid (const pure)

                (_, estimatingFeesTime) <- bench "estimate tx fee" $ do
                    let out = TxOut (dummyAddress @n) (Coin 1)
                    runExceptT $ withExceptT show $ W.estimateFeeForPayment @_ @s @t @k
                        w wid (out :| []) (Quantity 0)

                unsafeRunExceptT $ W.deleteWallet w wid
                pure BenchResults
                    { qualifier = getWalletName wname
                    , restorationTime
                    , listingAddressesTime
                    , estimatingFeesTime
                    , utxoStatistics
                    }

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
    => Trace IO Text
    -> TimeInterpreter IO
    -> (DBLayer IO s k -> IO a)
    -> IO a
withBenchDBLayer tr ti action =
    withSystemTempFile "bench.db" $ \dbFile _ -> do
        let before = newDBLayer (trMessageText tr) migrationDefaultValues (Just dbFile) ti
        let after = destroyDBLayer . fst
        bracket before after $ \(_ctx, db) -> action db
  where
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 0
        , Sqlite.defaultHardforkEpoch = Nothing
        }

logChunk :: SlotNo -> IO ()
logChunk slot = sayErr . fmt $ "Processing "+||slot||+""

prepareNode
    :: forall n. (NetworkDiscriminantVal n)
    => Proxy n
    -> FilePath
    -> NetworkParameters
    -> NodeVersionData
    -> IO ()
prepareNode _ socketPath np vData = do
    sayErr . fmt $ "Syncing "+|networkDiscriminantVal @n|+" node... "
    sl <- withNetworkLayer nullTracer np socketPath vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        waitForNodeSync nw logQuiet
    sayErr . fmt $ "Completed sync of "+|networkDiscriminantVal @n|+" up to "+||sl||+""

-- | Regularly poll the wallet to monitor it's syncing progress. Block until the
-- wallet reaches 100%.
waitForWalletSync
    :: forall s t k. ()
    => WalletLayer s t k
    -> WalletId
    -> GenesisParameters
    -> NodeVersionData
    -> IO ()
waitForWalletSync walletLayer wid gp vData = do
    (w, _, _) <- unsafeRunExceptT $ W.readWallet walletLayer wid
    let tolerance = mkSyncTolerance 3600
    prog <- syncProgress
                tolerance
                (timeInterpreter nl)
                (currentTip w)
                =<< getCurrentTime
    case prog of
        Ready -> return ()
        NotResponding -> do
            threadDelay 1000000
            waitForWalletSync walletLayer wid gp vData
        Syncing (Quantity p) -> do
            sayErr . fmt $ "[INFO] restoring: "+|p|+""
            threadDelay 1000000
            waitForWalletSync walletLayer wid gp vData
  where
    WalletLayer _ _ nl _ _ = walletLayer

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: NetworkLayer IO (IO Shelley) Block
    -> (SlotNo -> SlotNo -> IO ())
    -> IO SlotNo
waitForNodeSync nw _logSlot = loop 10
  where
    loop :: Int -> IO SlotNo
    loop retries = runExceptT (currentNodeTip nw) >>= \case
        Right nodeTip -> do
            let tolerance = mkSyncTolerance 60
            prog <- syncProgress
                        tolerance
                        (timeInterpreter nw)
                        nodeTip
                        =<< getCurrentTime
            if prog == Ready
                then pure (slotNo nodeTip)
                else do
                    -- 2 seconds poll interval
                    threadDelay 2000000
                    loop retries
        Left e | retries > 0 -> do
                     sayErr "Fetching tip failed, retrying shortly..."
                     threadDelay 15000000
                     loop (retries - 1)
               | otherwise -> throwIO e

logQuiet :: SlotNo -> SlotNo -> IO ()
logQuiet _ _ = pure ()
