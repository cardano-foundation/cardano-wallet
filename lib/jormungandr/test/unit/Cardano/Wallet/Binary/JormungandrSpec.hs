{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Binary.JormungandrSpec (spec) where

import Prelude

import Cardano.Wallet.Binary.Jormungandr
    ( BlockHeader (..), getBlockHeader, runGet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), SlotId (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.ByteString.Lazy as BL

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    describe "Decoding blocks" $ do
        it "should decode a genesis block header" $ do
            runGet getBlockHeader genesisBlock
            `shouldBe`
            (BlockHeader
                { version = 0
                , contentSize = 159
                , slot = SlotId {epochNumber = 0 , slotNumber = 0}
                , chainLength = 0
                , contentHash = Hash "\255\173\235\254\205Y\217\234\161.\144:\213\129\NUL\247\193\227X\153s\156=\ENQ\208\"\131\\\ACK\157+O"
                , parentHeaderHash = Hash "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
                })

genesisBlock :: BL.ByteString
genesisBlock = either error BL.fromStrict $ convertFromBase @ByteString Base16
    "005200000000009f000000000000000000000000ffadebfecd59d9eaa12e903a\
    \d58100f7c1e35899739c3d05d022835c069d2b4f000000000000000000000000\
    \00000000000000000000000000000000000000000047000048000000005cc1c2\
    \4900810200c200010108000000000000087001410f01840000000a01e030a694\
    \b80dbba2d1b8a4b55652b03d96315c8414b054fa737445ac2d2a865c76002604\
    \0001000000ff0005000006000000000000000000000000000000000000000000\
    \0000000000002c020001833324c37869c122689a35917df53a4f2294a3a52f68\
    \5e05f5f8e53b87e7ea452f000000000000000e"
