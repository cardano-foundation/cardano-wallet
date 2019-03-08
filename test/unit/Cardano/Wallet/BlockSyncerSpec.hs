{-# LANGUAGE DataKinds #-}

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
    ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteString
    ( ByteString, pack )
import Data.Time.Units
    ( Second, fromMicroseconds )
import Test.Hspec
    ( Arg, Spec, SpecWith, describe, it )
import Test.Hspec.Expectations
    ( shouldBe )
import Test.QuickCheck
    ( generate, vector )
import Test.QuickCheck.Gen
    ( Gen, choose, vectorOf )

import qualified Data.List as L
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Block syncer downloads blocks properly" $ do
        let tests =
                [
                    ("check ticking function when blocks are sent in exactly-once fashion"
                    , ExactlyOnce)
                ,
                    ("check ticking function when blocks are sent in at-least-once fashion"
                    , AtLeastOnce)
                ]
        forM_ ((L.take 10 . L.cycle) tests) $ \pair ->
            uncurry tickingFunctionTest pair
  where
      tickingFunctionTest
          :: String
          -> DeliveryMode
          -> SpecWith (Arg (IO ()))
      tickingFunctionTest testDescr deliveryMode = it testDescr $ do
          chunkSizesToTest <- generateBlockChunkSizes
          tickingFunctionTime <- generate $ choose (1,3)
          let testTime = (L.length chunkSizesToTest + 1)*(fromIntegral tickingFunctionTime)*1000*1000
          let tickTime = (fromMicroseconds tickingFunctionTime*1000*1000)
          consecutiveBlocks <- liftIO $ mkConsecutiveTestBlocks (sum chunkSizesToTest)
          consumerData <- newEmptyMVar
          putMVar consumerData $ BlocksConsumed []
          producerData <- newEmptyMVar
          putMVar producerData $ BlocksToInject (chunkSizesToTest,consecutiveBlocks)
          blockToIOaction <- writeToIORefAction consumerData consecutiveBlocks
          let blockDelivery = pushNextBlocks producerData deliveryMode
          threadId <- forkIO $ tickingFunction blockDelivery blockToIOaction tickTime (BlockHeadersConsumed [])
          threadDelay testTime
          (BlocksConsumed obtainedData) <- takeMVar consumerData
          killThread threadId
          obtainedData `shouldBe` ((map fst . reverse) consecutiveBlocks)

mkConsecutiveTestBlocks
    :: Int
    -- ^ number of consecutive blocks to create
    -> IO [((Hash "BlockHeader"),Block)]
    -- ^ returns block paired with generated hashes starting from the oldest
mkConsecutiveTestBlocks blockNum =
    loop blockNum []
  where
      loop
          :: Int
          -> [((Hash "BlockHeader"),Block)]
          -> IO [((Hash "BlockHeader"),Block)]
      loop blockNumToGo res = do
          let bytelistGenerator = pack <$> vector 10 :: Gen ByteString
          if blockNumToGo <= 0 then
              return $ reverse res
          else
             case res of
                 [] -> do
                     lastBlockHeader <- Hash <$> generate bytelistGenerator
                     theBlockHeader <- Hash <$> generate bytelistGenerator
                     let theEpoch = 0
                     let theSlot = 1
                     let theBlock = Block (BlockHeader theEpoch theSlot lastBlockHeader) Set.empty
                     loop (blockNumToGo - 1) [(theBlockHeader, theBlock)]
                 (lastBlockHeader, Block (BlockHeader lastEpoch lastSlot _) _ ):_ -> do
                     let theBlock = Block (BlockHeader lastEpoch (lastSlot + 1) lastBlockHeader) Set.empty
                     theBlockHeader <- Hash <$> generate bytelistGenerator
                     loop (blockNumToGo - 1) $ (theBlockHeader, theBlock):res


newtype BlocksConsumed = BlocksConsumed [(Hash "BlockHeader")] deriving (Show, Eq)

newtype BlocksToInject = BlocksToInject ([Int],[((Hash "BlockHeader"),Block)]) deriving (Show, Eq)


data DeliveryMode = ExactlyOnce | AtLeastOnce

pushNextBlocks
    :: MVar BlocksToInject
    -> DeliveryMode
    -> IO [Block]
pushNextBlocks ref mode = do
    (BlocksToInject (blocksToTake, blocksRemaining)) <- takeMVar ref
    if L.null blocksRemaining then return [] else
        (do case blocksToTake of
                num : rest -> do
                    let (bOut, bStay) = L.splitAt num blocksRemaining
                    putMVar ref $ BlocksToInject (rest, bStay)
                    case mode of
                        ExactlyOnce ->
                            return $ map snd bOut
                        AtLeastOnce -> do
                            additionalBlocks <- generate $ choose (1,3) :: IO Int
                            return $ map snd (bOut ++ take additionalBlocks bStay)
                [] -> return [])


writeToIORefAction
    :: MVar BlocksConsumed
    -> [((Hash "BlockHeader"),Block)]
    -> IO (Block -> IO ())
writeToIORefAction ref blocks = return $
    \block -> do
        (BlocksConsumed headerHashesConsumed) <- takeMVar ref
        case (map fst . filter (\(_,b) -> b == block) ) blocks of
            [blockHeader] -> do
                putMVar ref $ BlocksConsumed (blockHeader : headerHashesConsumed)
            _ ->
                return ()


generateBlockChunkSizes :: IO [Int]
generateBlockChunkSizes = do
    numberOfTicks <- generate $ choose (1,15)
    generate $ generateBlockChunks numberOfTicks
  where
      generateBlockChunks
          :: Int
          -> Gen [Int]
      generateBlockChunks numberOfTicks = do
          let chunkSizeGen = choose (0,15)
          vectorOf numberOfTicks chunkSizeGen
