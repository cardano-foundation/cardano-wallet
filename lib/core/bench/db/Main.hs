{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    ( KeyToAddress (..), Passphrase (..), generateKeyFromSeed, getKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( SeqState (..), defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletState (..)
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
import Data.List.Split
    ( chunksOf )
import Data.Quantity
    ( Quantity (..) )
import System.IO.Unsafe
    ( unsafePerformIO )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

withDB :: (DBLayerBench -> Benchmark) -> Benchmark
withDB = envWithCleanup (newDBLayer Nothing) (const (pure ()))

withCleanDB :: NFData b => DBLayerBench -> (DBLayerBench -> IO b) -> Benchmarkable
withCleanDB db = perRunEnv $ do
    void $ cleanDB db
    unsafeRunExceptT $ createWallet db testPk testCp testMetadata
    pure db

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
          (SlotId (fromIntegral i) 0)
          (Quantity (fromIntegral numOutputs))))
    | i <- [1..numTx] ]
  where
    mkInputs n = [TxIn (Hash (label "in" i)) (fromIntegral i) | i <- [1..n]]
    mkOutputs n = [TxOut (Address (label "addr" i)) (Coin 1) | i <- [1..n]]

label :: Show n => B8.ByteString -> n -> B8.ByteString
label prefix n = prefix <> B8.pack (show n)

main :: IO ()
main = defaultMain [
  withDB $ \db -> bgroup "putTxHistory"
      [ bgroup "single call"
          [ bench "1e2"   $ withCleanDB db $ benchPutTxHistory 1   100 1 1
          , bench "1e3"   $ withCleanDB db $ benchPutTxHistory 1  1000 1 1
          , bench "1e4"   $ withCleanDB db $ benchPutTxHistory 1 10000 1 1
          ]
      , bgroup "batches of 10"
          [ bench "1e2"   $ withCleanDB db $ benchPutTxHistory   10 10 1 1
          , bench "1e3"   $ withCleanDB db $ benchPutTxHistory  100 10 1 1
          , bench "1e3.6" $ withCleanDB db $ benchPutTxHistory  400 10 1 1
          ]
      ]
  ]


----------------------------------------------------------------------------

data DummyTarget

type DBLayerBench = DBLayer IO (SeqState DummyTarget) DummyTarget

instance NFData (DBLayer m s t) where
    rnf _ = ()

instance KeyToAddress DummyTarget where
    keyToAddress = Address . unXPub . getKey

deriving instance Eq (SeqState DummyTarget)

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

testCp :: Wallet (SeqState DummyTarget) DummyTarget
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
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid
