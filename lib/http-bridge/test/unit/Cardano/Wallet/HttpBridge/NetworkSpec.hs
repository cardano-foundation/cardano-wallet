{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.HttpBridge.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Environment
    ( Network (..) )
import Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeLayer (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( Restorer (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , SlotId (..)
    , flatSlot
    , slotMinBound
    )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        let network = mockRestorer noLog 105 (SlotId 106 1492)

        it "should get something from the latest epoch" $ do
            let h = BlockHeader
                    { slotId = SlotId 106 999
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 106 998)
                    }
            blocks <- runExceptT $ nextBlocks network h
            -- the number of blocks between slots 1000 and 1492 inclusive
            fmap length blocks `shouldBe` Right 493
            let hdrs = either (const []) (map (slotId . header)) blocks
            map slotNumber hdrs `shouldBe` [1000 .. 1492]
            map epochNumber hdrs `shouldSatisfy` all (== 106)

        it "should return all unstable blocks" $ do
            let h = BlockHeader
                    { slotId = SlotId 105 0
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 104 21599)
                    }
            blocks <- runExceptT $ nextBlocks network h
            fmap length blocks `shouldBe` Right (21600 + 1492)

        it "should return unstable blocks after the start slot" $ do
            let h = BlockHeader
                    { slotId = SlotId 105 17000
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 105 16999)
                    }
            blocks <- runExceptT $ nextBlocks network h
            -- this will be all the blocks between 105.17000 and 106.1492
            fmap length blocks `shouldBe` Right 6092

        it "should return just the tip block" $ do
            let h = BlockHeader
                    { slotId = SlotId 106 1491
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 106 1490)
                    }
            blocks <- runExceptT $ nextBlocks network h
            fmap length blocks `shouldBe` Right 1

        it "should get from packed epochs" $ do
            let h = BlockHeader
                    { slotId = SlotId 100 0
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 99 21599)
                    }
            Right blocks <- runExceptT $ nextBlocks network h
            -- an entire epoch's worth of blocks
            length blocks `shouldBe` 21599
            map (epochNumber . slotId . header) blocks
                `shouldSatisfy` all (== 100)

        it "should get from packed epochs and filter by start slot" $ do
            let h = BlockHeader
                    { slotId = SlotId 104 10000
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 104 9999)
                    }
            Right blocks <- runExceptT $ nextBlocks network h
            -- the number of remaining blocks in epoch 104
            length blocks `shouldBe` 11599
            map (epochNumber . slotId . header) blocks
                `shouldSatisfy` all (== 104)

        it "should produce no blocks if start slot is after tip" $ do
            let h = BlockHeader
                    { slotId = SlotId 107 0
                    , blockHeight = Quantity 0
                    , prevBlockHash = mockHash (SlotId 106 21599)
                    }
            blocks <- runExceptT $ nextBlocks network h
            blocks `shouldBe` Right []

        it "should work for the first epoch" $ do
            let h = BlockHeader
                    { slotId = slotMinBound
                    , blockHeight = Quantity 0
                    , prevBlockHash = Hash "genesis"
                    }
            Right blocks <- runExceptT $ nextBlocks network h
            length blocks `shouldBe` 21599

{-------------------------------------------------------------------------------
                             Mock HTTP Bridge
-------------------------------------------------------------------------------}

slotsPerEpoch :: Word16
slotsPerEpoch = 21600

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
mockHeaderFromHash h = BlockHeader slot (mockBlockHeight slot) prevHash
  where
    slot@(SlotId ep sl) = unMockHash h
    prevHash =
        case (ep, sl) of
            (0, 0) -> Hash "genesis"
            (_, 0) -> mockHash (SlotId (ep-1) (slotsPerEpoch - 1))
            _ -> mockHash (SlotId ep (sl - 1))

-- | Uses the flat SlotId as blockHeight. This would only happen naturally if
-- no slot is block-less.
mockBlockHeight :: SlotId -> Quantity "block" Natural
mockBlockHeight = Quantity . fromIntegral . flatSlot (EpochLength slotsPerEpoch)

-- | Generate an entire epoch's worth of mock blocks. There are no transactions
-- generated.
mockEpoch :: Word64 -> [Block Tx]
mockEpoch ep =
    [ Block (mockHeaderFromHash (mockHash sl)) mempty
    | sl <- [ SlotId ep i | i <- epochs ]
    ]
  where
    epochs = [ 0 .. fromIntegral (slotsPerEpoch - 1) ]

mockRestorer
    :: Monad m
    => (String -> m ()) -- ^ logger function
    -> Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> Restorer (Block Tx) m
mockRestorer logLine firstUnstableEpoch tip =
    HttpBridge.mkRestorer @'Testnet
        (mockHttpBridge logLine firstUnstableEpoch tip)

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
        pure $ Block (mockHeaderFromHash hash) mempty

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
