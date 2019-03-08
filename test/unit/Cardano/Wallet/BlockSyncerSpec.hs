{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

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

import qualified Data.List as L
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Block syncer downloads blocks properly" $ do
        it "Check ticking function when blocks are sent"
            (withMaxSuccess 10 $ property tickingFunctionTest)
  where
      tickingFunctionTest
          :: TickingArgs
          -> Property
      tickingFunctionTest (TickingArgs chunkSizesToTest tickTime testTime deliveryMode) = monadicIO $ liftIO $ do
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


data TickingArgs = TickingArgs
    { _chunkSizes :: [Int]
    , _tickingTime :: Second
    , _testTime :: Int
    , _deliveryMode :: DeliveryMode
    } deriving (Show)

instance Arbitrary TickingArgs where
    shrink (TickingArgs sizes t t' m) =
        [ TickingArgs sizes' t t' m | sizes' <- shrink sizes ]
    arbitrary = do
        sizes <- choose (1, 15) >>= generateBlockChunks
        (tickTime, testTime) <- choose (1, 3) >>= \t -> return
            ( fromMicroseconds (t * 1000 * 1000)
            , (L.length sizes + 1) * (fromIntegral t * 1000 * 1000)
            )
        deliveryMode <- elements [ExactlyOnce, AtLeastOnce]
        return $ TickingArgs sizes tickTime testTime deliveryMode
      where
        generateBlockChunks
            :: Int
            -> Gen [Int]
        generateBlockChunks n = do
              vectorOf n (choose (0, 15))

mkConsecutiveTestBlocks
    :: Int
    -- ^ number of consecutive blocks to create
    -> IO [((Hash "BlockHeader"),Block)]
    -- ^ returns block paired with generated hashes starting from the oldest
mkConsecutiveTestBlocks blockNum = do
    h  <- Hash <$> generate bytelistGenerator
    h' <- Hash <$> generate bytelistGenerator
    loop blockNum [(h', Block (BlockHeader 1 0 h) mempty)]
  where
    bytelistGenerator = pack <$> vector 10 :: Gen ByteString

    fromPreviousBlock
        :: (Hash "BlockHeader", Block)
        -> Gen (Hash "BlockHeader", Block)
    fromPreviousBlock (h, b) = do
        h' <- Hash <$> bytelistGenerator
        let epoch = epochIndex (header b)
        let slot = slotNumber (header b) + 1
        return (h',  Block (BlockHeader epoch slot h) mempty)

    loop
        :: Int
        -> [(Hash "BlockHeader", Block)]
        -> IO [(Hash "BlockHeader", Block)]
    loop n res
        | n <= 0 = return $ reverse res
        | otherwise = do
            let block = head res
            block' <- generate $ fromPreviousBlock block
            loop (n - 1) (block' : res)


newtype BlocksConsumed = BlocksConsumed [(Hash "BlockHeader")] deriving (Show, Eq)

newtype BlocksToInject = BlocksToInject ([Int],[((Hash "BlockHeader"),Block)]) deriving (Show, Eq)


data DeliveryMode = ExactlyOnce | AtLeastOnce deriving Show

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
