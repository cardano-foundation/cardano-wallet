{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    , TxMeta (..)
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
    ( Benchmark
    , Benchmarkable
    , bench
    , bgroup
    , defaultMain
    , envWithCleanup
    , perRunEnv
    )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.List.Split
    ( chunksOf )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.System
    ( SystemTime (..), systemToUTCTime )
import Data.Typeable
    ( Typeable )
import Fmt
    ( (+|), (|+) )
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
            [ bgroup "single batch"
                [ bench "1e2 x 1io" $ withCleanDB db $ benchPutTxHistory $
                    PutTxHistoryBench
                        { _nBatches = 1
                        , _batchSize = 100
                        , _nOutputs = 1
                        , _nInputs = 1
                        }
                , bench "1e3 x 1io" $ withCleanDB db $ benchPutTxHistory $
                    PutTxHistoryBench
                        { _nBatches = 1
                        , _batchSize = 1000
                        , _nOutputs = 1
                        , _nInputs = 1
                        }
                ]
            , bgroup "many batches of 10"
                [ bench "1e2 x 1io" $ withCleanDB db $ benchPutTxHistory $
                    PutTxHistoryBench
                        { _nBatches = 10
                        , _batchSize = 10
                        , _nOutputs = 1
                        , _nInputs = 1
                        }
                , bench "1e3 x 1io" $ withCleanDB db $ benchPutTxHistory $
                    PutTxHistoryBench
                        { _nBatches = 100
                        , _batchSize = 10
                        , _nOutputs = 1
                        , _nInputs = 1
                        }
                ]
            ]
        , bgroup "large transactions - single batch"
            -- The number of inputs and outputs of a transaction is limited by
            -- the maximum transaction size. So we don't need to benchmark
            -- further than that.
            -- TODO: calculate/look up maximum number of transaction inputs/outputs
            [ bench "1e2 x 10io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 100
                    , _nOutputs = 10
                    , _nInputs = 10
                    }
            , bench "1e3 x 10io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 1000
                    , _nOutputs = 10
                    , _nInputs = 10
                    }
            , bench "1e4 x 10io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 10000
                    , _nOutputs = 10
                    , _nInputs = 10
                    }
            , bench "1e2 x 100io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 100
                    , _nOutputs = 100
                    , _nInputs = 100
                    }
            , bench "1e3 x 100io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 1000
                    , _nOutputs = 100
                    , _nInputs = 100
                    }
            , bench "1e4 x 100io" $ withCleanDB db $ benchPutTxHistory $
                PutTxHistoryBench
                    { _nBatches = 1
                    , _batchSize = 10000
                    , _nOutputs = 100
                    , _nInputs = 100
                    }
            ]
        ]
    , withDB benchPutCheckpoint
    ]

----------------------------------------------------------------------------
-- Checkpoint benchmarks (covers UTxO and SeqState)
--
-- The very max number of checkpoints we are likely to insert per wallet
-- is k=2160.
--
-- Currently the DBLayer will only store a single checkpoint (no rollback), so
-- the #Checkpoints axis is a bit meaningless.

benchPutCheckpoint :: DBLayerBench -> Benchmark
benchPutCheckpoint db = bgroup "putCheckpoint"
    [ bgroup "UTxO"
        -- A fragmented wallet will have a large number of UTxO. The coin
        -- selection algorithm tries to prevent fragmentation.
        --
        --      #Checkpoints   UTxO Size
        [ bUTxO          100           0
        , bUTxO         1000           0
        , bUTxO           10          10
        , bUTxO          100          10
        , bUTxO         1000          10
        , bUTxO           10         100
        , bUTxO          100         100
        , bUTxO         1000         100
        , bUTxO           10        1000
        , bUTxO          100        1000
        , bUTxO         1000        1000
        ]
    , bgroup "SeqState"
        --      #Checkpoints  #Addresses
        [ bSeqState      100          10
        , bSeqState      100         100
        , bSeqState      100        1000
        , bSeqState     1000          10
        , bSeqState     1000         100
        ]
    ]
  where
    bUTxO n s = bench lbl $ withCleanDB db $ benchPutUTxO n s
        where lbl = n|+"CP x "+|s|+"UTxO"
    bSeqState n a = bench lbl $ withCleanDB db $ benchPutSeqState n a
        where lbl = n|+"CP x "+|a|+"addr"

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

data PutTxHistoryBench = PutTxHistoryBench
    { _nBatches :: Int
        -- ^ Number of batches to insert
    , _batchSize :: Int
        -- ^ Number of transaction in each batch
    , _nInputs :: Int
        -- ^ Number of inputs for each transaction
    , _nOutputs :: Int
        -- ^ Number of outputs for each transaction
    }

benchPutTxHistory :: PutTxHistoryBench -> DBLayerBench -> IO ()
benchPutTxHistory (PutTxHistoryBench nBatch batchSize nOuts nInps) db = do
    let batches = mkTxHistory (nBatch*batchSize) nInps nOuts
    unsafeRunExceptT $ forM_ (chunksOf batchSize batches) $ \txs -> do
        putTxHistory db testPk (Map.fromList txs)

mkTxHistory :: Int -> Int -> Int -> [(Hash "Tx", (Tx, TxMeta))]
mkTxHistory numTx numInputs numOutputs =
    [ ( Hash (label "tx-" i)
      , ( Tx (mkInputs numInputs) (mkOutputs numOutputs)
        , TxMeta
            { status = InLedger
            , direction = Incoming
            , slotId = fromFlatSlot (fromIntegral i)
            , amount = Quantity (fromIntegral numOutputs)
            }
        )
      )
    | i <- [1..numTx] ]

mkInputs :: Int -> [TxIn]
mkInputs n = [TxIn (Hash (label "in" i)) (fromIntegral i) | i <- [1..n]]

mkOutputs :: Int -> [TxOut]
mkOutputs n = [TxOut (Address (label "addr" i)) (Coin 1) | i <- [1..n]]

----------------------------------------------------------------------------
-- UTxO benchmarks

benchPutUTxO :: Int -> Int -> DBLayerBench -> IO ()
benchPutUTxO numCheckpoints utxoSize db = do
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
initDummyState =
    mkSeqState (xprv, mempty) defaultAddressPoolGap
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
