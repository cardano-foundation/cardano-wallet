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
    ( lift, runExceptT, throwError )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        let network = mockNetworkLayer noLog 105 (SlotId 106 1492)

        it "should get something from the latest epoch" $ do
            blocks <- runExceptT $ nextBlocks network (SlotId 106 1000)
            -- the number of blocks between slots 1000 and 1492 inclusive
            fmap length blocks `shouldBe` Right 493
            let hdrs = either (const []) (map (slotId . header)) blocks
            map slotNumber hdrs `shouldBe` [1000 .. 1492]
            map epochIndex hdrs `shouldSatisfy` all (== 106)

        it "should return all unstable blocks" $ do
            blocks <- runExceptT $ nextBlocks network (SlotId 105 0)
            fmap length blocks `shouldBe` Right (21600 + 1493)

        it "should return unstable blocks after the start slot" $ do
            blocks <- runExceptT $ nextBlocks network (SlotId 105 17000)
            -- this will be all the blocks between 105.17000 and 106.1492
            fmap length blocks `shouldBe` Right 6093

        it "should return just the tip block" $ do
            blocks <- runExceptT $ nextBlocks network (SlotId 106 1492)
            fmap length blocks `shouldBe` Right 1

        it "should get from packed epochs" $ do
            Right blocks <- runExceptT $ nextBlocks network (SlotId 100 0)
            -- an entire epoch's worth of blocks
            length blocks `shouldBe` 21600
            map (epochIndex . slotId . header) blocks `shouldSatisfy` all (== 100)

        it "should get from packed epochs and filter by start slot" $ do
            Right blocks <- runExceptT $ nextBlocks network (SlotId 104 10000)
            -- the number of remaining blocks in epoch 104
            length blocks `shouldBe` 11600
            map (epochIndex . slotId . header) blocks `shouldSatisfy` all (== 104)

        it "should produce no blocks if start slot is after tip" $ do
            blocks <- runExceptT $ nextBlocks network (SlotId 107 0)
            blocks `shouldBe` Right []

        it "should work for the first epoch" $ do
            Right blocks <- runExceptT $ nextBlocks network (SlotId 0 0)
            length blocks `shouldBe` 21600

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

mockNetworkLayer
    :: MonadThrow m
    => (String -> m ()) -- ^ logger function
    -> Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> NetworkLayer m String String
mockNetworkLayer logLine firstUnstableEpoch tip =
    mkNetworkLayer (mockHttpBridge logLine firstUnstableEpoch tip)

-- | A network layer which returns mock blocks.
mockHttpBridge
    :: MonadThrow m
    => (String -> m ()) -- ^ logger function
    -> Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> HttpBridge m String
mockHttpBridge logLine firstUnstableEpoch tip = HttpBridge
    { getBlock = \hash -> do
        lift $ logLine $ "mock getBlock " ++ show hash
        pure $ Block (mockHeaderFromHash hash) mempty

    , getEpoch = \ep -> do
        lift $ logLine $ "mock getEpoch " ++ show ep
        if ep < firstUnstableEpoch then
            pure $ mockEpoch ep
        else
            throwError $
                "mock epoch " ++ show ep ++ " > firstUnstableEpoch " ++
                show firstUnstableEpoch

    , getNetworkTip = do
        lift $ logLine "mock getNetworkTip"
        let hash = mockHash tip
        pure (hash, mockHeaderFromHash hash)
    }

-- If debugging, you might want to log with putStrLn.
noLog :: Monad m => String -> m ()
noLog = const (pure ())
