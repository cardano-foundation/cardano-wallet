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
    , decodeDerivationPathAttr
    , decodeTx
    , deserialiseCbor
    , encodeAttributes
    , encodeDerivationPathAttr
    , encodeTx
    )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , conjoin
    , property
    , vector
    , (===)
    , (==>)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{- HLINT ignore spec "Use head" -}

spec :: Spec
spec = parallel $ do
    describe "Encoding Tx" $ do
        let txs = txs1 <> txs2
        let roundTripTx tx = do
                let bytes = CBOR.toLazyByteString (encodeTx tx)
                let tx' = unsafeDeserialiseCbor decodeTx bytes
                tx `shouldBe` tx'

        it "encode . decode = pure (1)" $ do
            roundTripTx (txs !! 0)

        it "encode . decode = pure (2)" $ do
            roundTripTx (txs !! 1)

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

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Show, Eq)

-- An arbitrary mnemonic sentence for the tests
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
    Right seed = mkSomeMnemonic @'[12] mnem
    key = generateKeyFromSeed seed mempty
    pwd = payloadPassphrase key

{-------------------------------------------------------------------------------
                           Derivation Path Roundtrip
-------------------------------------------------------------------------------}

prop_derivationPathRoundTrip
    :: Passphrase "addr-derivation-payload"
    -> Passphrase "addr-derivation-payload"
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
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
        bytes <- BS.pack <$> vector 32
        return $ Passphrase $ BA.convert bytes

instance Arbitrary (Index 'WholeDomain 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

{-------------------------------------------------------------------------------
                                  Test Data
-------------------------------------------------------------------------------}

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin.fromWord64

-- A mainnet block with a transaction
txs1 :: [([TxIn], [TxOut])]
txs1 =
    [ ( [ TxIn { inputId = inputId0, inputIx = 3 } ]
      , [ TxOut { address = address0, tokens = coinToBundle  285000000 }
        , TxOut { address = address1, tokens = coinToBundle 1810771919 }
        ]
      )
    ]
  where
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
txs2 :: [([TxIn], [TxOut])]
txs2 =
    [ ( [ TxIn { inputId = inputId0, inputIx = 1 }
        , TxIn { inputId = inputId1, inputIx = 0 }
        ]
      , [ TxOut { address = address0, tokens = coinToBundle 1404176490 }
        , TxOut { address = address1, tokens = coinToBundle 1004099328 }
        ]
      )
    ]
  where
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
