module Cardano.NetworkLayer.HttpBridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.NetworkLayer.HttpBridge
    ( HttpBridge (..)
    , HttpBridgeError (..)
    , RustBackend
    , nextBlocks
    , runRustBackend
    )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..), slotsPerEpoch )
import Control.Exception
    ( Exception, throwIO )
import Control.Monad
    ( (<=<) )
import Control.Monad.Catch
    ( MonadThrow (..) )
import Control.Monad.Except
    ( ExceptT, runExceptT, throwError )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( Spec, SpecWith, beforeAll, describe, it, shouldBe, shouldSatisfy )

import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "Getting next blocks with a mock backend" $ do
        beforeAll (pure $ mockHttpBridge 105 (SlotId 106 1492)) $ do
             getNextBlocksSpec

getNextBlocksSpec :: SpecWith (HttpBridge IO)
getNextBlocksSpec = do
    it "should get something from the latest epoch" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 106 1000)
        -- the number of blocks between slots 1000 and 1492 inclusive
        length blocks `shouldBe` 493
        let hdrs = map (slotId . header) blocks
        map slotNumber hdrs `shouldBe` [1000 .. 1492]
        map epochIndex hdrs `shouldSatisfy` all (== 106)

    it "should get something from an unstable epoch" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 105 17000)
        length blocks `shouldBe` 1000

    it "should get from old epochs" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 104 10000)
        length blocks `shouldBe` 1000
        map (epochIndex . slotId . header) blocks `shouldSatisfy` all (== 104)

    it "should produce no blocks if start slot is after tip" $ \network -> do
        blocks <- runBackend network $ nextBlocks 1000 (SlotId 107 0)
        blocks `shouldBe` []

    it "should work for zero blocks" $ \network -> do
        blocks <- runBackend network $ nextBlocks 0 (SlotId 106 1000)
        blocks `shouldBe` []

unsafeRunExceptT :: (Exception e, MonadIO m) => ExceptT e m a -> m a
unsafeRunExceptT = either (liftIO . throwIO) pure <=< runExceptT

runBackend :: Exception e => HttpBridge IO -> ExceptT e RustBackend a -> IO a
runBackend network = runRustBackend network . unsafeRunExceptT


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

-- | A network layer which returns mock blocks.
mockHttpBridge
    :: MonadThrow m
    => Word64 -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> HttpBridge m
mockHttpBridge firstUnstableEpoch tip = HttpBridge
    { getBlock = \hash -> do
        -- putStrLn $ "mock getBlock " ++ show hash
        pure $ Block (mockHeaderFromHash hash) mempty
    , getEpoch = \ep -> do
        -- putStrLn $ "mock getEpoch " ++ show ep
        if ep < firstUnstableEpoch then
            pure $ mockEpoch ep
        else
            throwError $ HttpBridgeError $
                "mock epoch " ++ show ep ++ " > firstUnstableEpoch " ++
                show firstUnstableEpoch

    , getNetworkTip = do
        -- putStrLn $ "mock getNetworkTip"
        let hash = mockHash tip
        pure (hash, mockHeaderFromHash hash)
    }
