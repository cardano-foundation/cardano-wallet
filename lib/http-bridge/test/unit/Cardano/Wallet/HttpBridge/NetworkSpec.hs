{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.HttpBridge.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (..) )
import Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeLayer (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , SlotId (..)
    , SlotNo (..)
    , flatSlot
    , fromFlatSlot
    )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Word
    ( Word16, Word64 )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Cardano.Wallet.HttpBridge.Binary as B
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        let network = mockNetworkLayer noLog 105 (SlotId 106 1492)

        it "should get something from the latest epoch" $ do
            let h = mockBlockHeader 106 999
            blocks <- runExceptT $ nextBlocks network h
            -- the number of blocks between slots 1000 and 1492 inclusive
            fmap length blocks `shouldBe` Right 493
            let hdrs = either (const []) (map (fromSlotNo . slotNo . header)) blocks
            map slotNumber  hdrs `shouldBe` [1000 .. 1492]
            map epochNumber hdrs `shouldSatisfy` all (== 106)

        it "should return all unstable blocks" $ do
            let h = mockBlockHeader 105 0
            blocks <- runExceptT $ nextBlocks network h
            fmap length blocks `shouldBe` Right (21600 + 1492)

        it "should return unstable blocks after the start slot" $ do
            let h = mockBlockHeader 105 17000
            blocks <- runExceptT $ nextBlocks network h
            -- this will be all the blocks between 105.17000 and 106.1492
            fmap length blocks `shouldBe` Right 6092

        it "should return just the tip block" $ do
            let h = mockBlockHeader 106 1491
            blocks <- runExceptT $ nextBlocks network h
            fmap length blocks `shouldBe` Right 1

        it "should get from packed epochs" $ do
            let h = mockBlockHeader 100 0
            Right blocks <- runExceptT $ nextBlocks network h
            -- an entire epoch's worth of blocks
            length blocks `shouldBe` 21599
            map (epochNumber . fromSlotNo . slotNo . header) blocks
                `shouldSatisfy` all (== 100)

        it "should get from packed epochs and filter by start slot" $ do
            let h = mockBlockHeader 104 10000
            Right blocks <- runExceptT $ nextBlocks network h
            -- the number of remaining blocks in epoch 104
            length blocks `shouldBe` 11599
            map (epochNumber . fromSlotNo . slotNo . header) blocks
                `shouldSatisfy` all (== 104)

        it "should produce no blocks if start slot is after tip" $ do
            let h = mockBlockHeader 107 0
            blocks <- runExceptT $ nextBlocks network h
            blocks `shouldBe` Right []

        it "should work for the first epoch" $ do
            let h = mockBlockHeader 0 0
            Right blocks <- runExceptT $ nextBlocks network h
            length blocks `shouldBe` 21599

{-------------------------------------------------------------------------------
                             Mock HTTP Bridge
-------------------------------------------------------------------------------}

slotsPerEpoch :: Integral n => n
slotsPerEpoch = 21600

fromSlotNo :: SlotNo -> SlotId
fromSlotNo = fromFlatSlot (EpochLength slotsPerEpoch)

toSlotNo :: SlotId -> SlotNo
toSlotNo = flatSlot (EpochLength slotsPerEpoch)

mockBlockHeader :: Word64 -> Word16 -> BlockHeader
mockBlockHeader 0 0 = BlockHeader (SlotNo 0) (Hash "genesis")
mockBlockHeader ep sl = BlockHeader slot prevHash
  where
    slot = toSlotNo $ SlotId ep sl
    prevSlotId = fromSlotNo $ pred slot
    prevHash = if slot == SlotNo 0 then Hash "genesis" else mockHash prevSlotId

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
mockHeaderFromHash :: Hash a -> B.BlockHeader
mockHeaderFromHash h = B.BlockHeader (fromSlotNo slot) prev
  where
    BlockHeader slot prev = mockBlockHeader sl ep
    SlotId sl ep = unMockHash h

-- | Generate an entire epoch's worth of mock blocks. There are no transactions
-- generated.
mockEpoch :: Word64 -> [B.Block]
mockEpoch ep =
    [ B.Block (mockHeaderFromHash (mockHash sl)) mempty
    | sl <- [ SlotId ep i | i <- epochs ]
    ]
  where
    epochs = [ 0 .. (slotsPerEpoch - 1) ]

mockNetworkLayer
    :: Monad m
    => (String -> m ()) -- ^ logger function
    -> Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> NetworkLayer (HttpBridge 'Mainnet) m
mockNetworkLayer logLine firstUnstableEpoch tip =
    HttpBridge.mkNetworkLayer (mockHttpBridge logLine firstUnstableEpoch tip)

-- | A network layer which returns mock blocks.
mockHttpBridge
    :: Monad m
    => (String -> m ()) -- ^ logger function
    -> Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> HttpBridgeLayer m
mockHttpBridge logLine firstUnstableEpoch tip = HttpBridgeLayer
    { getBlock = \hash -> do
        lift $ logLine $ "mock getBlock " ++ show hash
        pure $ B.Block (mockHeaderFromHash hash) mempty

    , getEpoch = \ep -> do
        lift $ logLine $ "mock getEpoch " ++ show ep
        if ep < firstUnstableEpoch then
            pure $ mockEpoch ep
        else
            pure []

    , getNetworkTip = do
        lift $ logLine "mock getNetworkTip"
        let hash = mockHash tip
        pure (hash, mockHeaderFromHash hash)

    , postSignedTx = undefined
    }

-- If debugging, you might want to log with putStrLn.
noLog :: Monad m => String -> m ()
noLog = const (pure ())
