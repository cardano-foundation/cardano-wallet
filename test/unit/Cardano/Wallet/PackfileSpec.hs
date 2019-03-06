{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.PackfileSpec (spec) where

import Prelude

import qualified Data.ByteString.Lazy.Char8 as L8

import Test.Hspec
    ( Spec, describe, it, shouldBe )

import Cardano.Wallet.Binary.Packfile

-- version 1 header
packFileHeader :: L8.ByteString
packFileHeader = "\254CARDANOPACK\NUL\NUL\NUL\SOH"

testOneBlob :: L8.ByteString
testOneBlob = packFileHeader <> "\0\0\0\5hello\0\0\0"

testTwoBlobs :: L8.ByteString
testTwoBlobs = packFileHeader
        <> "\0\0\0\11first block\0"
        <> "\0\0\0\12second block"

spec :: Spec
spec = do
    describe "Decoding pack file" $ do
        it "should not decode junk" $ do
            let decoded = decodePackfile "junkjunkjunkjunk"
            decoded `shouldBe` Left MissingMagicError

        it "should ensure pack file type" $ do
            let decoded = decodePackfile "\254CARDANOYOLO\NUL\NUL\NUL\SOH"
            decoded `shouldBe` Left WrongFileTypeError

        it "should ensure pack file version" $ do
            let decoded = decodePackfile "\254CARDANOPACK\NUL\NUL\NUL\2"
            decoded `shouldBe` Left VersionTooNewError

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
