{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Primitive.Types.Block.Gen
    ( genBlockHeader
    )
where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Data.Quantity
    ( Quantity (Quantity)
    )
import Data.Word
    ( Word32
    )
import Test.QuickCheck
    ( Gen
    , elements
    )

genBlockHeader :: SlotNo -> Gen BlockHeader
genBlockHeader sl = do
    BlockHeader sl (mockBlockHeight sl) <$> genHash <*> (Just <$> genHash)
  where
    mockBlockHeight :: SlotNo -> Quantity "block" Word32
    mockBlockHeight = Quantity . fromIntegral . unSlotNo

    genHash =
        elements
            [ Hash
                $ unsafeFromHex
                    "aac1308b9868af89c396b08ff6f3cfea8e0859c94d1b3bc834baeaaff8645448"
            , Hash
                $ unsafeFromHex
                    "d93b27cc7bb6fd2fe6ee42de5328c13606bb714a78475a41335207d2afd6026e"
            , Hash
                $ unsafeFromHex
                    "63b8828e2eadc3f14b9b691fa9df76139a9c9b13a12ec862b324cc5a88f9fcc5"
            ]
