{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.BlockSyncerSpec
    ( spec
    ) where


import Prelude

import Cardano.Wallet.BlockSyncer
    ( BlockHeadersConsumed (..), tickingFunction )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteString
    ( ByteString, pack )
import Data.Map.Strict
    ( Map )
import Data.Time.Units
    ( Second, fromMicroseconds )
import Data.Tuple
    ( swap )
import Test.Hspec
    ( Arg, Spec, SpecWith, describe, it )
import Test.Hspec.Expectations
    ( shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , elements
    , generate
    , property
    , vector
    , withMaxSuccess
    )
import Test.QuickCheck.Gen
    ( Gen, choose, vectorOf )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Block syncer downloads blocks properly" $ do
        it "Check ticking function when blocks are sent"
            (withMaxSuccess 10 $ property tickingFunctionTest)
  where
      tickingFunctionTest
          :: (TickingTime, Blocks)
          -> Property
      tickingFunctionTest (TickingTime tickTime, Blocks consecutiveBlocks) = monadicIO $ liftIO $ do
          done <- newEmptyMVar
          readerChan <- newMVar []
          let reader = mkReader readerChan
          writer <- mkWriter done consecutiveBlocks
          threadId <- forkIO $ tickingFunction writer reader tickTime (BlockHeadersConsumed [])
          _ <- takeMVar done
          obtainedData <- takeMVar readerChan
          killThread threadId
          obtainedData `shouldBe` L.nub (reverse consecutiveBlocks)


newtype TickingTime = TickingTime Second deriving (Show)

instance Arbitrary TickingTime where
    -- No shrinking
    arbitrary = do
        tickTime <- fromMicroseconds . (* (1000 * 1000)) <$> choose (1, 3)
        return $ TickingTime tickTime

newtype Blocks = Blocks [Block]
    deriving Show

instance Arbitrary Blocks where
    arbitrary = do
        n <- arbitrary
        let h0 = BlockHeader 1 0 (Hash "initial block")
        let blocks = take n $ iterate next
                ( blockHeaderHash h0
                , Block h0 mempty
                )
        Blocks . map snd . mconcat <$> mapM duplicateMaybe blocks
      where
        next :: (Hash "BlockHeader", Block) -> (Hash "BlockHeader", Block)
        next (prev, b) =
            let
                epoch = epochIndex (header b)
                slot = slotNumber (header b) + 1
                h = BlockHeader epoch slot prev
            in
                (blockHeaderHash h, Block h mempty)

        duplicateMaybe :: a -> Gen [a]
        duplicateMaybe a = do
            predicate <- arbitrary
            if predicate then return [a, a] else return [a]


blockHeaderHash :: BlockHeader -> Hash "BlockHeader"
blockHeaderHash =
    Hash . CBOR.toStrictByteString . encodeBlockHeader
  where
    encodeBlockHeader (BlockHeader epoch slot prev) = mempty
        <> CBOR.encodeListLen 3
        <> CBOR.encodeWord64 epoch
        <> CBOR.encodeWord16 slot
        <> CBOR.encodeBytes (getHash prev)

mkWriter
    :: MVar ()
    -> [a]
    -> IO (IO [a])
mkWriter done xs0 = do
    ref <- newMVar xs0
    return $ do
        xs <- takeMVar ref
        case xs of
            [] -> putMVar done () *> return []
            _  -> do
                -- NOTE
                -- Not ideal because it makes the tests non-deterministic. Ideally,
                -- this should be seeded, or done differently.
                num <- generate $ choose (1, 3)
                let (left, right) = L.splitAt num xs
                putMVar ref right
                return left

mkReader
    :: MVar [a]
    -> a
    -> IO ()
mkReader ref x = do
    modifyMVar_ ref $ return . (x :)
