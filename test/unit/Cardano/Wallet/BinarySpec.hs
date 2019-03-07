{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.BinarySpec
    ( spec

    -- * Helpers
    , unsafeDeserialiseFromBytes
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( decodeBlock, decodeBlockHeader, decodeTx, encodeTx, txId )
import Cardano.Wallet.Primitive
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Set as Set

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    describe "Decoding blocks" $ do
        it "should decode a block header" $ do
            bs <- L8.readFile "test/data/Cardano/Wallet/BinarySpec-block-header-1"
            let decoded = unsafeDeserialiseFromBytes decodeBlockHeader bs
            decoded `shouldBe` blockHeader1

        it "should decode a block without transactions" $ do
            bs <- L8.readFile "test/data/Cardano/Wallet/BinarySpec-block-1"
            let decoded = unsafeDeserialiseFromBytes decodeBlock bs
            decoded `shouldBe` block1

        it "should decode a block with transactions" $ do
            bs <- L8.readFile "test/data/Cardano/Wallet/BinarySpec-block-2"
            let decoded = unsafeDeserialiseFromBytes decodeBlock bs
            decoded `shouldBe` block2

        it "should decode a testnet block with a transaction" $ do
            bs <- L8.readFile "test/data/Cardano/Wallet/BinarySpec-block-3"
            let decoded = unsafeDeserialiseFromBytes decodeBlock bs
            decoded `shouldBe` block3

    describe "Encoding Tx" $ do
        let txs = Set.toList (transactions block2 <> transactions block3)
        let roundTripTx tx = do
                let bytes = CBOR.toLazyByteString (encodeTx tx)
                let tx' = unsafeDeserialiseFromBytes decodeTx bytes
                tx `shouldBe` tx'

        it "encode . decode = pure (1)" $ do
            roundTripTx (txs !! 0)

        it "encode . decode = pure (2)" $ do
            roundTripTx (txs !! 1)

        it "should compute correct txId (1)" $ do
            let hash = txId (txs !! 0)
            let hash' = hash16 "c470563001e448e61ff1268c2a6eb458ace1d04011a02cb262b6d709d66c23d0"
            hash `shouldBe` hash'

        it "should compute correct txId (2)" $ do
            let hash = txId (txs !! 1)
            let hash' = hash16 "d30d37f1f8674c6c33052826fdc5bc198e3e95c150364fd775d4bc663ae6a9e6"
            hash `shouldBe` hash'

-- A mainnet block header
blockHeader1 :: BlockHeader
blockHeader1 = BlockHeader
    { epochIndex = 105
    , slotNumber = 9520
    , prevBlockHash = hash16 "9f3c67b575bf2c5638291949694849d6ce5d29efa1f2eb3ed0beb6dac262e9e0"
    }

-- A mainnet block
block1 :: Block
block1 = Block
    { header = BlockHeader
        { epochIndex = 105
        , slotNumber = 9519
        , prevBlockHash = hash16 "4d97da40fb62bec847d6123762e82f9325f11d0c8e89deee0c7dbb598ed5f0cf"
        }
    , transactions = mempty
    }

-- A mainnet block with a transaction
block2 :: Block
block2 = Block
    { header = BlockHeader
        { epochIndex = 105
        , slotNumber = 9876
        , prevBlockHash = hash16 "da73001193ab3e6a43921385941c5f96b8f56de3908e78fae06f038b91dadd9d"
        }
    , transactions = Set.fromList
        [ Tx
            { inputs =
                [ TxIn
                    { inputId = hash16 "60dbb2679ee920540c18195a3d92ee9be50aee6ed5f891d92d51db8a76b02cd2"
                    , inputIx = 3
                    }
                ]
            , outputs =
                [ TxOut
                    { address = addr58 "DdzFFzCqrhsug8jKBMV5Cr94hKY4DrbJtkUpqptoGEkovR2QSkcAcRgjnUyegE689qBX6b2kyxyNvCL6mfqiarzRB9TRq8zwJphR31pr"
                    , coin = Coin 285000000
                    }
                , TxOut
                    { address = addr58 "DdzFFzCqrhsmxmuQpgjUrvRwF5ZKnyQ7pGrS4q53u5B516wcc26maHz9M4myYAkQVc5m9E4DKJjRDjPxuDdK3ZsHb1Dnqf3XorZ1PnzX"
                    , coin = Coin 1810771919
                    }
                ]
            }
        ]
    }

-- A testnet block with a transaction
block3 :: Block
block3 = Block
    { header = BlockHeader
        { epochIndex = 30
        , slotNumber = 9278
        , prevBlockHash = hash16 "b065b5fe97bec5fd130e7a639189499c9d0b1fcf9348c5c19f7a22700da7a35e"
        }
    , transactions = Set.fromList
        [ Tx
            { inputs =
                [ TxIn
                    { inputId = hash16 "6967e2b5c3ad5ae07a9bd8d888f1836195a04f7a1cb4b6d083261870068fab1b"
                    , inputIx = 1}
                , TxIn
                    { inputId = hash16 "7064addc0968bccd7d57d2e7aa1e9c2f666d8387042483fc1e87200cfb96c8f1"
                    , inputIx = 0} ]
            , outputs =
                [ TxOut
                    { address = addr58 "37btjrVyb4KBsw2f3V76ntfwqDPgyf3QmmdsrTSmCnuTGYtS9JgVXzxeQEsKjgWurKoyw9BDNEtLxWtU9znK49SC8bLTirk6YqcAESFxXJkSyXhQKL"
                    , coin = Coin 1404176490
                    }
                , TxOut
                    { address = addr58 "37btjrVyb4KD5Ne4yvGAHGbQuHUYQX1VPsXh85rBh3UrGSMWdRSFxBYQ9RQRHCMezN6AMLd3uYTC5hbeVTUiPzfQUTCEogg2HrSJKQUjAgsoYZHwT3"
                    , coin = Coin 1004099328
                    }
                ]
            }
        ]
    }


-- * Helpers

-- | Make a Hash from a Base16 encoded string, without error handling.
hash16 :: ByteString -> Hash a
hash16 = either bomb Hash . convertFromBase Base16
    where
        bomb msg = error ("Could not decode test string: " <> msg)

-- | Make an Address from a Base58 encoded string, without error handling.
addr58 :: ByteString -> Address
addr58 = maybe (error "addr58: Could not decode") Address . decodeBase58 bitcoinAlphabet

-- | CBOR deserialise without error handling - handy for prototypes or testing.
unsafeDeserialiseFromBytes :: (forall s. CBOR.Decoder s a) -> BL.ByteString -> a
unsafeDeserialiseFromBytes decoder bytes =
    either (\e -> error $ "unsafeDeserialiseFromBytes: " <> show e) snd $
        CBOR.deserialiseFromBytes decoder bytes
