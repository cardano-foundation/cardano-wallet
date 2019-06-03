{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This benchmark gathers timings of the important database operations, for
-- various sizes of input. The chosen sizes are meant to approach and slightly
-- exceed realistic maximum values for a wallet.
--
-- For example, the number of inputs and outputs of a single transaction is
-- limited by the maximum transaction size. So we don't need to benchmark
-- transactions larger than that. However we could benchmark writing many large
-- transactions.
--
-- Additionally, the highest number of checkpoints that a wallet could ever want
-- is k=2160. However, there could be multiple wallets in the database, and each
-- checkpoint could have a large (fragmented) UTxO.
--
-- The coin selection algorithm attempts to minimize UTxO size, however a small
-- UTxO can't be taken for granted.
--
-- Smaller input sizes are also timed so that we can check that whether
-- operations are accidentally quadratic.
--
-- An important limit is SQLITE_MAX_VARIABLE_NUMBER, which defaults to 999. If
-- there are database statements with more parameters than that, the DBLayer
-- will throw exceptions.

import Prelude

import Cardano.Crypto.Wallet
    ( unXPub )
import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..), cleanDB )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , KeyToAddress (..)
    , Passphrase (..)
    , XPub
    , generateKeyFromSeed
    , getKey
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPool
    , SeqState (..)
    , defaultAddressPoolGap
    , emptyPendingIxs
    , mkAddressPool
    , mkSeqState
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet, unsafeInitWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletState (..)
    , fromFlatSlot
    )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM_, void )
import Criterion.Main
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString.Char8 as B8
import Data.List.Split
    ( chunksOf )
import qualified Data.Map as Map
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.System
    ( SystemTime (..), systemToUTCTime )
import Data.Typeable
    ( Typeable )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

main :: IO ()
main = defaultMain
    [ withDB $ \db -> bgroup "putTxHistory"
        [ bgroup "small transactions"
            [ bgroup "single call"
                [ bench "1e2 x   1io"   $ withCleanDB db $ benchPutTxHistory 1   100 1 1
                , bench "1e3 x   1io"   $ withCleanDB db $ benchPutTxHistory 1  1000 1 1
                -- , bench "1e4"   $ withCleanDB db $ benchPutTxHistory 1 10000 1 1
                ]
            , bgroup "batches of 10"
                [ bench "1e2 x   1io"   $ withCleanDB db $ benchPutTxHistory   10 10 1 1
                , bench "1e3 x   1io"   $ withCleanDB db $ benchPutTxHistory  100 10 1 1
                -- , bench "1e3.6" $ withCleanDB db $ benchPutTxHistory  400 10 1 1
                ]
            ]
        , bgroup "large transactions"
            -- The number of inputs and outputs of a transaction is limited by
            -- the maximum transaction size. So we don't need to benchmark
            -- further than that.
            -- TODO: calculate/look up maximum number of transaction inputs/outputs
            [ bgroup "single call"
                [ bench "1e2 x  10io" $ withCleanDB db $ benchPutTxHistory 1   100  10  10
                , bench "1e3 x  10io" $ withCleanDB db $ benchPutTxHistory 1  1000  10  10
                , bench "1e4 x  10io" $ withCleanDB db $ benchPutTxHistory 1 10000  10  10
                , bench "1e2 x 100io" $ withCleanDB db $ benchPutTxHistory 1   100 100 100
                , bench "1e3 x 100io" $ withCleanDB db $ benchPutTxHistory 1  1000 100 100
                , bench "1e4 x 100io" $ withCleanDB db $ benchPutTxHistory 1 10000 100 100
                ]
            ]
        ]
    , withDB $ \db -> bgroup "putCheckpoint"
        -- The very max number of checkpoints we are likely to insert per wallet
        -- is k=2160.
        [ bgroup "UTxO"
            -- A fragmented wallet will have a large number of UTxO. The coin
            -- selection algorithm tries to prevent fragmentation
            [ bench "1e2 x   0utxo" $ withCleanDB db $ benchPutCheckpoint   100   0
            , bench "1e3 x   0utxo" $ withCleanDB db $ benchPutCheckpoint  1000   0
            -- , bench "1e4" $ withCleanDB db $ benchPutCheckpoint 10000 0
            , bench "1e1 x   10utxo" $ withCleanDB db $ benchPutCheckpoint    10   10
            , bench "1e2 x   10utxo" $ withCleanDB db $ benchPutCheckpoint   100   10
            , bench "1e3 x   10utxo" $ withCleanDB db $ benchPutCheckpoint  1000   10
            , bench "1e1 x  100utxo" $ withCleanDB db $ benchPutCheckpoint    10  100
            , bench "1e2 x  100utxo" $ withCleanDB db $ benchPutCheckpoint   100  100
            , bench "1e3 x  100utxo" $ withCleanDB db $ benchPutCheckpoint  1000  100
            , bench "1e1 x 1000utxo" $ withCleanDB db $ benchPutCheckpoint    10 1000
            , bench "1e2 x 1000utxo" $ withCleanDB db $ benchPutCheckpoint   100 1000
            , bench "1e3 x 1000utxo" $ withCleanDB db $ benchPutCheckpoint  1000 1000
            ]
        , bgroup "SeqState"
            [ bench "1e2 x   10addr" $ withCleanDB db $ benchPutSeqState 100   10
            , bench "1e2 x  100addr" $ withCleanDB db $ benchPutSeqState 100  100
            , bench "1e2 x 1000addr" $ withCleanDB db $ benchPutSeqState 100 1000
            ]
        ]
    ]

----------------------------------------------------------------------------
-- Criterion env functions for database setup

withDB :: (DBLayerBench -> Benchmark) -> Benchmark
withDB = envWithCleanup (newDBLayer Nothing) (const (pure ()))

withCleanDB
    :: NFData b
    => DBLayerBench
    -> (DBLayerBench -> IO b)
    -> Benchmarkable
withCleanDB db = perRunEnv $ do
    void $ cleanDB db
    unsafeRunExceptT $ createWallet db testPk testCp testMetadata
    pure db

----------------------------------------------------------------------------
-- TxHistory benchmarks

benchPutTxHistory :: Int -> Int -> Int -> Int -> DBLayerBench -> IO ()
benchPutTxHistory numBatches batchSize numInputs numOutputs db = do
    let batches = mkTxHistory (numBatches*batchSize) numInputs numOutputs
    unsafeRunExceptT $ forM_ (chunksOf batchSize batches) $ \txs -> do
        putTxHistory db testPk (Map.fromList txs)

mkTxHistory :: Int -> Int -> Int -> [(Hash "Tx", (Tx, TxMeta))]
mkTxHistory numTx numInputs numOutputs =
    [ ( Hash (label "tx-" i)
      , ( Tx (mkInputs numInputs) (mkOutputs numOutputs)
        , TxMeta InLedger Incoming
          (fromFlatSlot (fromIntegral i))
          (Quantity (fromIntegral numOutputs))))
    | i <- [1..numTx] ]

mkInputs :: Int -> [TxIn]
mkInputs n = [TxIn (Hash (label "in" i)) (fromIntegral i) | i <- [1..n]]

mkOutputs :: Int -> [TxOut]
mkOutputs n = [TxOut (Address (label "addr" i)) (Coin 1) | i <- [1..n]]

----------------------------------------------------------------------------
-- UTxO benchmarks

benchPutCheckpoint :: Int -> Int -> DBLayerBench -> IO ()
benchPutCheckpoint numCheckpoints utxoSize db = do
    let cps = mkCheckpoints numCheckpoints utxoSize
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk) cps

mkCheckpoints :: Int -> Int -> [WalletBench]
mkCheckpoints numCheckpoints utxoSize = [ cp i | i <- [1..numCheckpoints]]
  where
    cp i = unsafeInitWallet (UTxO utxo) mempty
        (fromFlatSlot $ fromIntegral i)
        initDummyState
    utxo = Map.fromList $ zip (mkInputs utxoSize) (mkOutputs utxoSize)

----------------------------------------------------------------------------
-- SeqState Address Discovery

benchPutSeqState :: Int -> Int -> DBLayerBench -> IO ()
benchPutSeqState numCheckpoints numAddrs db =
    unsafeRunExceptT $ mapM_ (putCheckpoint db testPk)
        [ initWallet $ SeqState (mkPool numAddrs i) (mkPool numAddrs i)
          emptyPendingIxs | i <- [1..numCheckpoints] ]

mkPool
    :: forall t chain. (KeyToAddress t, Typeable chain)
    => Int -> Int -> AddressPool t chain
mkPool numAddrs i = mkAddressPool ourAccount defaultAddressPoolGap addrs
  where
    addrs =
        [ Address (label "addr-" (show i ++ "-" ++ show j))
        | j <- [1..numAddrs] ]

----------------------------------------------------------------------------
-- Mock data to use for benchmarks

data DummyTarget

type DBLayerBench = DBLayer IO (SeqState DummyTarget) DummyTarget
type WalletBench = Wallet (SeqState DummyTarget) DummyTarget

instance NFData (DBLayer m s t) where
    rnf _ = ()

instance KeyToAddress DummyTarget where
    keyToAddress = Address . unXPub . getKey

deriving instance Eq (SeqState DummyTarget)

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

testCp :: WalletBench
testCp = initWallet initDummyState

initDummyState :: SeqState DummyTarget
initDummyState = mkSeqState (xprv, mempty) defaultAddressPoolGap
  where
      bytes = entropyToBytes <$> unsafePerformIO $ genEntropy @(EntropySize 15)
      xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , passphraseInfo = Nothing
    , status = Ready
    , delegation = NotDelegating
    , creationTime = systemToUTCTime (MkSystemTime 0 0)
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

ourAccount :: Key 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where seed = Passphrase $ BA.convert $ BS.replicate 32 0

-- | Make a prefixed bytestring for use as a Hash or Address.
label :: Show n => B8.ByteString -> n -> B8.ByteString
label prefix n = prefix <> B8.pack (show n)
