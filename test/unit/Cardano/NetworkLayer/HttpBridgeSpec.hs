module Cardano.NetworkLayer.HttpBridgeSpec
    ( spec
    ) where

import Prelude


import Cardano.NetworkLayer
    ( NetworkLayer (..) )
import Cardano.NetworkLayer.HttpBridge
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..), slotsPerEpoch )
import Control.Monad.Catch
    ( MonadThrow (..) )
import Control.Monad.Except
    ( runExceptT, throwError )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( Spec, SpecWith, beforeAll, describe, it, shouldBe, shouldSatisfy )

import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        beforeAll (pure $ mockNetworkLayer 105 (SlotId 106 1492)) $ do
             getNextBlocksSpec

getNextBlocksSpec :: (Show e, Eq e) => SpecWith (NetworkLayer IO e e)
getNextBlocksSpec = do
    it "should get something from the latest epoch" $ \network -> do
        blocks <- runExceptT $ nextBlocks network 1000 (SlotId 106 1000)
        -- the number of blocks between slots 1000 and 1492 inclusive
        fmap length blocks `shouldBe` Right 493
        let hdrs = either (const []) (map (slotId . header)) blocks
        map slotNumber hdrs `shouldBe` [1000 .. 1492]
        map epochIndex hdrs `shouldSatisfy` all (== 106)

    it "should get something from an unstable epoch" $ \network -> do
        blocks <- runExceptT $ nextBlocks network 1000 (SlotId 105 17000)
        fmap length blocks `shouldBe` Right 1000

    it "should get from old epochs" $ \network -> do
        Right blocks <- runExceptT $ nextBlocks network 1000 (SlotId 104 10000)
        length blocks `shouldBe` 1000
        map (epochIndex . slotId . header) blocks `shouldSatisfy` all (== 104)

    it "should produce no blocks if start slot is after tip" $ \network -> do
        blocks <- runExceptT $ nextBlocks network 1000 (SlotId 107 0)
        blocks `shouldBe` Right []

    it "should work for zero blocks" $ \network -> do
        blocks <- runExceptT $ nextBlocks network 0 (SlotId 106 1000)
        blocks `shouldBe` Right []

{-------------------------------------------------------------------------------
                             Mock HTTP Bridge
-------------------------------------------------------------------------------}

-- | Embed an epoch index and slot number into a hash.
mockHash :: SlotId -> Hash a
mockHash (SlotId ep sl) =
    Hash $ B8.pack ("Hash " <> show ep <> "." <> show sl)

-- | Extract the epoch index and slot number from a hash.
unMockHash :: Hash a -> SlotId
unMockHash (Hash h) = parse . map B8.unpack . B8.split '.' . B8.drop 5 $ h
  where
    parse [ep, sl] = SlotId (read ep) (read sl)
    parse _ = error $ "Could not read mock hash: " ++ B8.unpack h

-- | Create a block header from its hash, assuming that the hash was created
-- with 'mockHash'.
mockHeaderFromHash :: Hash a -> BlockHeader
mockHeaderFromHash h = BlockHeader slot prevHash
  where
    slot = unMockHash h
    prevHash =
        if slot == SlotId 0 0 then
            Hash "genesis"
        else
            mockHash (pred slot)

-- | Generate an entire epoch's worth of mock blocks. There are no transactions
-- generated.
mockEpoch :: Word64 -> [Block]
mockEpoch ep =
    [ Block (mockHeaderFromHash (mockHash sl)) mempty
    | sl <- [ SlotId ep i | i <- epochs ]
    ]
  where
    epochs = [ 0 .. fromIntegral (slotsPerEpoch - 1) ]

mockNetworkLayer :: MonadThrow m
    => Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> NetworkLayer m String String
mockNetworkLayer firstUnstableEpoch tip =
    mkNetworkLayer (mockHttpBridge firstUnstableEpoch tip)

-- | A network layer which returns mock blocks.
mockHttpBridge
    :: MonadThrow m
    => Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> HttpBridge m String
mockHttpBridge firstUnstableEpoch tip = HttpBridge
    { getBlock = \hash -> do
        -- putStrLn $ "mock getBlock " ++ show hash
        pure $ Block (mockHeaderFromHash hash) mempty
    , getEpoch = \ep -> do
        -- putStrLn $ "mock getEpoch " ++ show ep
        if ep < firstUnstableEpoch then
            pure $ mockEpoch ep
        else
            throwError $
                "mock epoch " ++ show ep ++ " > firstUnstableEpoch " ++
                show firstUnstableEpoch

    , getNetworkTip = do
        -- putStrLn $ "mock getNetworkTip"
        let hash = mockHash tip
        pure (hash, mockHeaderFromHash hash)
    }
