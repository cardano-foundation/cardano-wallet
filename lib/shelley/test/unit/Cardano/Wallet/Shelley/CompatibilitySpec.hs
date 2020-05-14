{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Shelley.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Hash.Class
    ( digest )
import Cardano.Wallet.Primitive.Types
    ( EpochLength (..), Hash (..), SlotId (..), fromFlatSlot )
import Cardano.Wallet.Shelley.Compatibility
    ( ShelleyBlock, TPraosStandardCrypto, fromTip, toPoint, toShelleyHash )
import Data.Proxy
    ( Proxy (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( Crypto (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), SlotNo (..), Tip (..), getTipPoint )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), choose, frequency, property, vector, (===) )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Conversions" $
        it "toPoint' . fromTip' == getTipPoint" $ property $ \gh tip -> do
            let fromTip' = fromTip gh epochLength
            let toPoint' = toPoint gh epochLength
            toPoint' (fromTip' tip) === (getTipPoint tip)

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Tip ShelleyBlock) where
    arbitrary = frequency
        [ (10, return TipGenesis)
        , (90, arbitraryTip)
        ]
      where
        arbitraryTip = do
            n <- choose (0, 100)
            hash <- toShelleyHash
                . Hash
                . digest (Proxy @(HASH TPraosStandardCrypto))
                . BS.pack <$> vector 5
            return $ Tip (SlotNo n) hash (BlockNo n)

epochLength :: EpochLength
epochLength = EpochLength 10

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> choose (0, 100)
