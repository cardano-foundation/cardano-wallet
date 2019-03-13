module Cardano.ChainProducer.RustHttpBridge.MockNetworkLayer
    ( mockNetworkLayer
    ) where

import Prelude

import Cardano.ChainProducer.RustHttpBridge.NetworkLayer
    ( NetworkLayer (..), NetworkLayerError (..) )
import Cardano.Wallet.Primitive
    ( Block (..), BlockHeader (..), Hash (..) )
import Cardano.Wallet.Slotting
    ( EpochIndex (..)
    , LocalSlotIndex (..)
    , SlotId (..)
    , slotPrev
    , slotsPerEpoch
    )
import Control.Monad.Catch
    ( MonadThrow (..) )
import Control.Monad.Except
    ( throwError )

import qualified Data.ByteString.Char8 as B8


-- | Embed an epoch index and slot number into a hash.
mockHash :: SlotId -> Hash a
mockHash (SlotId (EpochIndex ep) (LocalSlotIndex sl))
    = Hash $ B8.pack ("Hash " <> show ep <> "." <> show sl)

-- | Extract the epoch index and slot number from a hash.
unMockHash :: Hash a -> SlotId
unMockHash (Hash h) = parse . map B8.unpack . B8.split '.' . B8.drop 5 $ h
    where
        parse [ep, sl] =
            SlotId (EpochIndex (read ep)) (LocalSlotIndex (read sl))
        parse _ = error $ "Could not read mock hash: " ++ B8.unpack h

-- | Create a block header from its hash, assuming that the hash was created
-- with 'mockHash'.
mockHeaderFromHash :: Hash a -> BlockHeader
mockHeaderFromHash h = BlockHeader ep sl prevHash
    where
        slotId@(SlotId ep sl) = unMockHash h
        prevHash = maybe (Hash "?") mockHash (slotPrev slotId)

-- | Generate an entire epoch's worth of mock blocks. There are no transactions
-- generated.
mockEpoch :: EpochIndex -> [Block]
mockEpoch ep = [ Block (mockHeaderFromHash (mockHash sl)) mempty
               | sl <- [ SlotId ep i | i <- epochs ] ]
    where epochs = [ 0 .. fromIntegral (slotsPerEpoch - 1) ]

-- | A network layer which returns mock blocks.
mockNetworkLayer
    :: MonadThrow m
    => EpochIndex -- ^ make getEpoch fail for epochs after this
    -> SlotId -- ^ the tip block
    -> NetworkLayer m
mockNetworkLayer firstUnstableEpoch tip = NetworkLayer
    { getBlock = \hash -> do
            -- putStrLn $ "mock getBlock " ++ show hash
            pure $ Block (mockHeaderFromHash hash) mempty
    , getEpoch = \ep -> do
            -- putStrLn $ "mock getEpoch " ++ show ep
            if ep < firstUnstableEpoch
                then pure $ mockEpoch ep
                else throwError $ NetworkLayerError
                     $ "mock epoch " ++ show ep ++ " > firstUnstableEpoch "
                     ++ show firstUnstableEpoch
    , getNetworkTip = do
            -- putStrLn $ "mock getNetworkTip"
            let hash = mockHash tip
            pure (hash, mockHeaderFromHash hash)
    }
