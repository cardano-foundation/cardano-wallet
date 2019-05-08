{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Binary.JormungandrSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Binary.Jormungandr
    ()
import Data.ByteString
    ( ByteString )

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )

import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Test.Hspec
    ( Spec, describe, shouldBe, xit )

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    describe "Decoding blocks" $ do
        xit "should decode a genesis block" $ do
            unsafeDeserialiseFromBytes decodeGenesisBlock genesisBlock
            `shouldBe`
            BlockHeader (SlotId 0 0) (Hash "?")
  where
    unsafeDeserialiseFromBytes = undefined
    decodeGenesisBlock = error "TODO: import from Binary.Jormungandr"

genesisBlock :: ByteString
genesisBlock = either error id $ convertFromBase @ByteString Base16
    "005200000000009f000000000000000000000000ffadebfecd59d9eaa12e903a\
    \d58100f7c1e35899739c3d05d022835c069d2b4f000000000000000000000000\
    \00000000000000000000000000000000000000000047000048000000005cc1c2\
    \4900810200c200010108000000000000087001410f01840000000a01e030a694\
    \b80dbba2d1b8a4b55652b03d96315c8414b054fa737445ac2d2a865c76002604\
    \0001000000ff0005000006000000000000000000000000000000000000000000\
    \0000000000002c020001833324c37869c122689a35917df53a4f2294a3a52f68\
    \5e05f5f8e53b87e7ea452f000000000000000e"
