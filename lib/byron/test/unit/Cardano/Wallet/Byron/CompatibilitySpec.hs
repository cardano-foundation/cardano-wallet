{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Byron.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Hashing
    ( decodeHash )
import Cardano.Wallet.Byron.Compatibility
    ( ByronBlock, fromTip, toGenTx, toPoint )
import Cardano.Wallet.Byron.TransactionSpec
    ( goldenMainnet__1_1
    , goldenMainnet__1_25
    , goldenMainnet__25_1
    , goldenMainnet__2_2
    , goldenTestnet__1_1
    , goldenTestnet__1_25
    , goldenTestnet__25_1
    , goldenTestnet__2_2
    )
import Cardano.Wallet.Primitive.Types
    ( EpochLength (..), Hash (..), SealedTx (..), SlotId (..), fromFlatSlot )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad
    ( forM_ )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronHash (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), SlotNo (..), Tip (..), getTipPoint )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Golden Tests - toGenTx (doesn't throw)" $ do
        forM_
            [ goldenMainnet__1_1
            , goldenMainnet__2_2
            , goldenMainnet__1_25
            , goldenMainnet__25_1
            , goldenTestnet__1_1
            , goldenTestnet__2_2
            , goldenTestnet__1_25
            , goldenTestnet__25_1
            ] $ \bytes ->
            let str = show $ toGenTx $ SealedTx $ unsafeFromHex bytes
            in it (take 23 str) (putStrLn str)

    describe "Conversions" $
        it "toPoint' . fromTip' == getTipPoint" $ property $ \gh tip -> do
            let fromTip' = fromTip gh epochLength
            let toPoint' = toPoint gh epochLength
            toPoint' (fromTip' tip) === (getTipPoint tip)

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vector 32

instance (Arbitrary (Tip ByronBlock)) where
    arbitrary = oneof
        [ return TipGenesis
        , arbitraryTip
        ]
      where
        arbitraryTip = do
            n <- choose (0, 100)
            return $ Tip (SlotNo n) hash (BlockNo n)
        hash = ByronHash
            . either (error . show) id
            . decodeHash
            . T.pack
            $ replicate 64 '0'

epochLength :: EpochLength
epochLength = EpochLength 10

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> choose (0, 100)
