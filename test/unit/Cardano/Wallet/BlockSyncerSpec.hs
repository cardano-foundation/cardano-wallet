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
          :: (TickingArgs, Blocks)
          -> Property
      tickingFunctionTest (TickingArgs tickTime, Blocks consecutiveBlocks) = monadicIO $ liftIO $ do
          done <- newEmptyMVar
          consumerData <- newMVar []
          producerData <- newMVar $ BlocksToInject (map snd consecutiveBlocks)
          let reader = mkReader consumerData (Map.fromList $ swap <$> consecutiveBlocks)
          let blockDelivery = pushNextBlocks done producerData
          threadId <- forkIO $ tickingFunction blockDelivery reader tickTime (BlockHeadersConsumed [])
          _ <- takeMVar done
          obtainedData <- takeMVar consumerData
          killThread threadId
          obtainedData `shouldBe` ((map fst . reverse) consecutiveBlocks)


data TickingArgs = TickingArgs
    { _tickingTime :: Second
    } deriving (Show)

instance Arbitrary TickingArgs where
    -- No shrinking
    arbitrary = do
        tickTime <- fromMicroseconds . (* (1000 * 1000)) <$> choose (1, 3)
        return $ TickingArgs tickTime

newtype Blocks = Blocks [(Hash "BlockHeader", Block)]
    deriving Show

instance Arbitrary Blocks where
    arbitrary = do
        n <- arbitrary
        mode <- arbitrary
        let h0 = BlockHeader 1 0 (Hash "initial block")
        let blocks = take n $ iterate next
                ( blockHeaderHash h0
                , Block h0 mempty
                )
        case mode of
            ExactlyOnce -> return $ Blocks blocks
            AtLeastOnce -> Blocks . mconcat <$> mapM duplicateMaybe blocks
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

newtype BlocksToInject = BlocksToInject [Block] deriving (Show, Eq)


data DeliveryMode
    = ExactlyOnce
    | AtLeastOnce
    deriving Show

instance Arbitrary DeliveryMode where
    -- No shrinking
    arbitrary = elements [ExactlyOnce, AtLeastOnce]

pushNextBlocks
    :: MVar ()
    -> MVar BlocksToInject
    -> IO [Block]
pushNextBlocks done ref = do
    BlocksToInject blocksRemaining <- takeMVar ref
    case blocksRemaining of
        [] -> putMVar done () *> return []
        _  -> do
            -- NOTE
            -- Not ideal because it makes the tests non-deterministic. Ideally,
            -- this should be seeded, or done differently.
            num <- generate $ choose (1, 3)
            let (bOut, bStay) = L.splitAt num blocksRemaining
            putMVar ref $ BlocksToInject bStay
            return bOut


mkReader
    :: Ord k
    => MVar [v]
    -> Map k v
    -> k
    -> IO ()
mkReader ref m k = do
    case k `Map.lookup` m of
        Just v ->
            modifyMVar_ ref $ return . (v :)
        Nothing ->
            return ()
