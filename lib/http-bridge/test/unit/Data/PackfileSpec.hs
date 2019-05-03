module Cardano.Wallet.Binary.PackfileSpec (spec) where

import Prelude

import Cardano.Wallet.Binary
    ( decodeBlock )
import Cardano.Wallet.Binary.Packfile
    ( PackfileError (..), decodePackfile )
import Cardano.Wallet.BinarySpec
    ( unsafeDeserialiseFromBytes )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), SlotId (..) )
import Data.Either
    ( fromRight, isRight )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8


-- version 1 header
packFileHeader :: L8.ByteString
packFileHeader = "\254CARDANOPACK\NUL\NUL\NUL\SOH"

testOneBlob :: L8.ByteString
testOneBlob = packFileHeader <> "\0\0\0\5hello\0\0\0"

testTwoBlobs :: L8.ByteString
testTwoBlobs = packFileHeader
        <> "\0\0\0\11first block\0"
        <> "\0\0\0\12second block"

-- Get this file from cardano-http-bridge with:
-- wget -O test/data/epoch-mainnet-104 http://localhost:8080/mainnet/epoch/104
testPackfile :: FilePath
testPackfile = "test/data/Cardano/Wallet/Binary/PackfileSpec-epoch-mainnet-104"

spec :: Spec
spec = do
    describe "Decoding pack file" $ do
        it "should not decode junk" $ do
            let decoded = decodePackfile "junkjunkjunkjunk"
            decoded `shouldBe` Left MissingMagicError

        it "should ensure pack file type" $ do
            let decoded = decodePackfile "\254CARDANOYOLO\NUL\NUL\NUL\SOH"
            decoded `shouldBe` Left WrongFileTypeError

        it "should ensure pack file version is lesser" $ do
            let decoded = decodePackfile "\254CARDANOPACK\NUL\NUL\NUL\2"
            decoded `shouldBe` Left VersionTooNewError

        it "should ensure pack file version is greater" $ do
            let decoded = decodePackfile "\254CARDANOPACK\NUL\NUL\NUL\0"
            decoded `shouldBe` Left VersionTooOldError

        it "should decode an empty pack file" $ do
            decodePackfile packFileHeader `shouldBe` Right []

        it "should decode a single blob" $ do
            decodePackfile testOneBlob `shouldBe` Right ["hello"]

        it "should decode multiple blobs" $ do
            decodePackfile testTwoBlobs `shouldBe`
                Right ["first block", "second block"]

        it "should not decode overly large blobs" $ do
            let decoded = decodePackfile (packFileHeader <> "\64\0\0\0")
            decoded `shouldBe`
                Left (BlobDecodeError "read block of size: 1073741824")

        it "should reject extra data" $ do
            let decoded = decodePackfile (packFileHeader <> "a")
            decoded `shouldBe` Left (BlobDecodeError "not enough bytes")

    describe "Decoding a mainnet pack file" $ do
        it "should decode pack file for mainnet epoch 104" $ do
            bs <- L8.readFile testPackfile
            let decoded = decodePackfile bs
            decoded `shouldSatisfy` isRight
            length (fromRight [] decoded) `shouldBe` 21600

        it "should decode the entire blocks" $ do
            bs <- L8.readFile testPackfile
            let Right (first:second:_) = decodePackfile bs
            B8.length first `shouldBe` 648092
            B8.length second `shouldBe` 1181

        it "should decode correct blocks" $ do
            bs <- L8.readFile testPackfile
            let (ebb:first:second:_) =
                    map (slotId . header) $ unsafeDeserialiseEpoch bs
            epochNumber ebb `shouldBe` 104
            epochNumber first `shouldBe` 104
            slotNumber ebb `shouldBe` 0 -- epoch genesis block
            slotNumber first `shouldBe` 0 -- first block
            slotNumber second `shouldBe` 1 -- second block

-- | Decode all blocks in a pack file, without error handling
unsafeDeserialiseEpoch :: L8.ByteString -> [Block]
unsafeDeserialiseEpoch = either giveUp decodeBlocks . decodePackfile
    where
        decodeBlocks = map decodeBlob
        decodeBlob = unsafeDeserialiseFromBytes decodeBlock . L8.fromStrict
        giveUp err = error ("Could not decode pack file: " <> show err)
