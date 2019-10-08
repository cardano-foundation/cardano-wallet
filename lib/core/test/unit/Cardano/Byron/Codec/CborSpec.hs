{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Byron.Codec.CborSpec
    ( spec
    ) where

import Prelude

import Cardano.Byron.Codec.Cbor
    ( decodeAddressDerivationPath
    , decodeAddressPayload
    , decodeAllAttributes
    , decodeBlock
    , decodeBlockHeader
    , decodeDerivationPathAttr
    , decodeSignedTx
    , decodeTx
    , decodeTxWitness
    , deserialiseCbor
    , encodeAttributes
    , encodeDerivationPathAttr
    , encodeSignedTx
    , encodeTx
    , encodeTxWitness
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase (..)
    , fromMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isLeft )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Expectation, HasCallStack, Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , conjoin
    , property
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    describe "Decoding blocks" $ do
        it "should decode a block header" $ do
            bs <- L8.readFile
                "test/data/Cardano/Byron/Codec/CborSpec-block-header-1"
            let decoded = unsafeDeserialiseCbor decodeBlockHeader bs
            decoded `shouldBe` blockHeader1

        it "should decode a block without transactions" $ do
            bs <- L8.readFile "test/data/Cardano/Byron/Codec/CborSpec-block-1"
            let decoded = unsafeDeserialiseCbor decodeBlock bs
            decoded `shouldBe` block1

        it "should decode a block with transactions" $ do
            bs <- L8.readFile "test/data/Cardano/Byron/Codec/CborSpec-block-2"
            let decoded = unsafeDeserialiseCbor decodeBlock bs
            decoded `shouldBe` block2

        it "should decode a testnet block with a transaction" $ do
            bs <- L8.readFile "test/data/Cardano/Byron/Codec/CborSpec-block-3"
            let decoded = unsafeDeserialiseCbor decodeBlock bs
            decoded `shouldBe` block3

        it "should decode a block with many transactions" $ do
            bs <- L8.readFile "test/data/Cardano/Byron/Codec/CborSpec-block-4"
            let decoded = unsafeDeserialiseCbor decodeBlock bs
            decoded `shouldBe` block4

        it "should fail to decode a junk block" $ do
            let junk = mconcat (replicate 100 "junk")
                decoded = CBOR.deserialiseFromBytes decodeBlock junk
            decoded `shouldSatisfy` isLeft

        it "should fail to decode a block with block header data" $ do
            bs <- L8.readFile
                "test/data/Cardano/Byron/Codec/CborSpec-block-header-1"
            let decoded = CBOR.deserialiseFromBytes decodeBlock bs
            decoded `shouldBe`
                Left (CBOR.DeserialiseFailure 3 "expected list of length 3")

    describe "Encoding Tx" $ do
        let txs = transactions block2 <> transactions block3
        let roundTripTx tx = do
                let bytes = CBOR.toLazyByteString (encodeTx tx)
                let tx' = unsafeDeserialiseCbor decodeTx bytes
                tx `shouldBe` tx'

        it "encode . decode = pure (1)" $ do
            roundTripTx (txs !! 0)

        it "encode . decode = pure (2)" $ do
            roundTripTx (txs !! 1)

    let pkWit = TxWitness "trust me"

    describe "Encoding Tx Witness" $ do
        it "(encode . decode) = pure" $ do
            cborRoundtrip
                decodeTxWitness
                encodeTxWitness
                pkWit

    describe "Encoding Signed Tx" $ do
        let txs = transactions block2 <> transactions block3
        it "(encode . decode) = pure" $ do
            cborRoundtrip
                decodeSignedTx
                encodeSignedTx
                (txs !! 0, [pkWit])

    describe "decodeAddress <-> encodeAddress roundtrip" $ do
        it "DerivationPath roundtrip" (property prop_derivationPathRoundTrip)

    describe "Golden Tests for Byron Addresses w/ random scheme (Mainnet)" $ do
        it "decodeDerivationPath - mainnet - initial account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584283581ca08bcb9e5e8cd30d5aea6d434c46abd8604fe4907d\
                    \56b9730ca28ce5a101581e581c22e25f2464ec7295b556d86d0ec33bc1\
                    \a681e7656da92dbc0582f5e4001a3abe2aa5"
                , accIndex =
                    2147483648
                , addrIndex =
                    2147483648
                }

        it "decodeDerivationPath - mainnet - another account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584283581cb039e80866203e82fc834b8e6a355b83ec6f8fd199\
                    \66078a40e6d6b2a101581e581c22e27fb12d08728073cd416dfbfcb8dc\
                    \0e760335d1d60f65e8740034001a4bce4d1a"
                , accIndex =
                    2694138340
                , addrIndex =
                    2512821145
                }

    describe "Golden Tests for Byron Addresses w/ random scheme (Testnet)" $ do
        it "decodeDerivationPath - testnet - initial account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584983581ca03d42af673855aabcef3059e21c37235ae706072d\
                    \38150dcefae9c6a201581e581c22e25f2464ec7295b556d86d0ec33bc1\
                    \a681e7656da92dbc0582f5e402451a4170cb17001a39a0b7b5"
                , accIndex =
                    2147483648
                , addrIndex =
                    2147483648
                }

        it "decodeDerivationPath - testnet - another account" $
            decodeDerivationPathTest DecodeDerivationPath
                { mnem =
                    arbitraryMnemonic
                , addr =
                    "82d818584983581c267b40902921c3afd73926a83a23ca08ae9626a64a\
                    \4b5616d14d6709a201581e581c22e219c90fb572d565134f6daeab650d\
                    \c871d130430afe594116f1ae02451a4170cb17001aee75f28a"
                , accIndex =
                    3337448281
                , addrIndex =
                    3234874775
                }

cborRoundtrip
    :: (HasCallStack, Show a, Eq a)
    => (forall s. CBOR.Decoder s a)
    -> (a -> CBOR.Encoding)
    -> a
    -> Expectation
cborRoundtrip decode encode a = do
    let bytes = CBOR.toLazyByteString $ encode a
    let a' = unsafeDeserialiseCbor decode bytes
    a `shouldBe` a'

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Show, Eq)

-- An aribtrary mnemonic sentence for the tests
arbitraryMnemonic :: [Text]
arbitraryMnemonic =
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

decodeDerivationPathTest :: DecodeDerivationPath -> Expectation
decodeDerivationPathTest DecodeDerivationPath{..} =
    decoded `shouldBe` Just (Just (Index accIndex, Index addrIndex))
  where
    payload = unsafeDeserialiseCbor decodeAddressPayload $
        BL.fromStrict (unsafeFromHex addr)
    decoded = deserialiseCbor (decodeAddressDerivationPath pwd) payload
    Right seed = fromMnemonic @'[12] mnem
    key = generateKeyFromSeed seed mempty
    pwd = payloadPassphrase key

{-------------------------------------------------------------------------------
                           Derivation Path Roundtrip
-------------------------------------------------------------------------------}

prop_derivationPathRoundTrip
    :: Passphrase "addr-derivation-payload"
    -> Passphrase "addr-derivation-payload"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> Property
prop_derivationPathRoundTrip pwd pwd' acctIx addrIx =
    let
        encoded = CBOR.toLazyByteString $ encodeAttributes
            [ encodeDerivationPathAttr pwd acctIx addrIx ]
        decoded = unsafeDeserialiseCbor
                (decodeAllAttributes >>=  decodeDerivationPathAttr pwd)
                encoded
        decoded' = unsafeDeserialiseCbor
                (decodeAllAttributes >>=  decodeDerivationPathAttr pwd')
                encoded
    in
        conjoin
            [ decoded === Just (acctIx, addrIx)
            , pwd /= pwd' ==> decoded' === Nothing
            ]

instance {-# OVERLAPS #-} Arbitrary (Passphrase "addr-derivation-payload") where
    arbitrary = do
        -- TODO n <- choose (minSeedLengthBytes, 32)
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        return $ Passphrase $ BA.convert bytes

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

{-------------------------------------------------------------------------------
                                  Test Data
-------------------------------------------------------------------------------}

-- A mainnet block header
blockHeader1 :: BlockHeader
blockHeader1 = BlockHeader
    { slotId = SlotId 105 9520
    , blockHeight = Quantity 2276029
    , headerHash = Hash "http-bridge"
    , parentHeaderHash = Hash $ unsafeFromHex
        "9f3c67b575bf2c5638291949694849d6ce5d29efa1f2eb3ed0beb6dac262e9e0"
    }

-- A mainnet block
block1 :: Block ([TxIn], [TxOut])
block1 = Block
    { header = BlockHeader
        { slotId = SlotId 105 9519
        , blockHeight = Quantity 2276028
        , headerHash = Hash "http-bridge"
        , parentHeaderHash = parentHeaderHash0
        }
    , transactions = mempty
    }
  where
    parentHeaderHash0 = Hash $ unsafeFromHex
        "4d97da40fb62bec847d6123762e82f9325f11d0c8e89deee0c7dbb598ed5f0cf"

-- A mainnet block with a transaction
block2 :: Block ([TxIn], [TxOut])
block2 = Block
    { header = BlockHeader
        { slotId = SlotId 105 9876
        , blockHeight = Quantity 2276385
        , headerHash = Hash "http-bridge"
        , parentHeaderHash = parentHeaderHash0
        }
    , transactions =
        [ ( [ TxIn { inputId = inputId0, inputIx = 3 } ]
          , [ TxOut { address = address0, coin = Coin  285000000 }
            , TxOut { address = address1, coin = Coin 1810771919 }
            ]
          )
        ]
    }
  where
    parentHeaderHash0 = Hash $ unsafeFromHex
        "da73001193ab3e6a43921385941c5f96b8f56de3908e78fae06f038b91dadd9d"
    inputId0 = Hash $ unsafeFromHex
        "60dbb2679ee920540c18195a3d92ee9be50aee6ed5f891d92d51db8a76b02cd2"
    address0 = Address $ unsafeFromHex
        "82d818584283581c797eae689c4ae43f03e35e9460311f94be94bfc3ea5a76d2f6c048\
        \08a101581e581c1afbc57540db15381fdacad79f50a10cff0c465f327a4f20951348f1\
        \001ae1a9cde7"
    address1 = Address $ unsafeFromHex
        "82d818584283581c3e8a0407c6de369cc1ad9394394442ee4f3a9c0aa3901f6672f668\
        \b0a101581e581c1afbc57540db15782ccbedd74b6fbd51d04e4104bfd451e64058d9f3\
        \001a806e7f0c"

-- A testnet block with a transaction
block3 :: Block ([TxIn], [TxOut])
block3 = Block
    { header = BlockHeader
        { slotId = SlotId 30 9278
        , blockHeight = Quantity 657163
        , headerHash = Hash "http-bridge"
        , parentHeaderHash = parentHeaderHash0
        }
    , transactions =
        [ ( [ TxIn { inputId = inputId0, inputIx = 1 }
            , TxIn { inputId = inputId1, inputIx = 0 }
            ]
          , [ TxOut { address = address0, coin = Coin 1404176490 }
            , TxOut { address = address1, coin = Coin 1004099328 }
            ]
          )
        ]
    }
  where
    parentHeaderHash0 = Hash $ unsafeFromHex
        "b065b5fe97bec5fd130e7a639189499c9d0b1fcf9348c5c19f7a22700da7a35e"
    inputId0 = Hash $ unsafeFromHex
        "6967e2b5c3ad5ae07a9bd8d888f1836195a04f7a1cb4b6d083261870068fab1b"
    inputId1 = Hash $ unsafeFromHex
        "7064addc0968bccd7d57d2e7aa1e9c2f666d8387042483fc1e87200cfb96c8f1"
    address0 = Address $ unsafeFromHex
        "82d818584983581c33935c07c07e788c7cccace299584f8fd1384fca449cd1bc0fe5e1\
        \a2a201581e581c25f59ec34005e181d4af50411aa31f1a4f5b7b8c63d4bda553f03697\
        \02451a4170cb17001a71518443"
    address1 = Address $ unsafeFromHex
        "82d818584983581c6a49d9d611eea0a9d8fc3219ccf5ade53b337b7f1e0b824e28eb48\
        \b0a201581e581c27b756273d10060d450d2280fee8047c7cd69e6b162a8a381eb57190\
        \02451a4170cb17001a96ee1bde"

-- A mainnet block with multiple transactions
block4 :: Block ([TxIn], [TxOut])
block4 = Block
    { header = BlockHeader
        { slotId = SlotId 14 18
        , blockHeight = Quantity 302376
        , headerHash = Hash "http-bridge"
        , parentHeaderHash = parentHeaderHash0
        }
    , transactions =
        [ ( [ TxIn
                { inputId = inputId0
                , inputIx = 0
                }
            ]
          , [ TxOut
                { address = addr0
                , coin = Coin 3841254542346
                }
            , TxOut
                { address = addr1
                , coin = Coin 2700667457
                }
            ]
          )
        , ( [ TxIn
                { inputId = inputId1
                , inputIx = 0
                }
            ]
          , [ TxOut
                { address = addr2
                , coin = Coin 3832107959251
                }
            , TxOut
                { address = addr3
                , coin = Coin 11823271860
                }
            ]
          )
        ]
    }
  where
    parentHeaderHash0 = Hash $ unsafeFromHex
        "f4283844eb78ca6f6333b007f5a735d71499d6ce7cc816846a033a36784bd299"
    inputId0 = Hash $ unsafeFromHex
        "f91292301d4bb1b6e040cecdff4030959b49c95e7dae087782dd558bebb6668a"
    addr0 = Address $ unsafeFromHex
        "82d818584283581c65243b0eb267a1e23e31779f4d158d64add2cac0426efab0432812\
        \05a101581e581cca3e553c9c63c51ae19d3143d1fdb71f757aa3c1d1c4d93a9b21a721\
        \001a89f0bb9f"
    addr1 = Address $ unsafeFromHex
        "82d818584283581ca1f35e0a51601097936e996a02d75d0e723788d3de791f2a9d257f\
        \17a101581e581cc91aaa9c4f659b29441f8f18edc1f4764b4da001a626a1d5bc4b448e\
        \001a90c0f092"
    inputId1 = Hash $ unsafeFromHex
        "96e170491afb6ebd579fd57c76c684f22436f8cc3a912397ddb1c9c51b86fb53"
    addr2 = Address $ unsafeFromHex
        "82d818584283581cc5187f50a05790381d57bd376d08e959d849b09ffa90199b7cdb0a\
        \e7a101581e581cca3e553c9c63c536953d5843d94c01ffa6e48add9d3006265d607a12\
        \001a959dbfa2"
    addr3 = Address $ unsafeFromHex
        "82d818584283581c490fa50c10dfd6d1cebb7980461af82ecbba2ff48f6d315d0a84ea\
        \22a101581e581c760ff0854c82c212bf7dbd3b358dfc745d847d5bf41b260f045b7bee\
        \001a9fec655a"
