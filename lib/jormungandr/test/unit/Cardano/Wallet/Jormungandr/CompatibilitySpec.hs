{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( signData )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), Hash (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress, unsafeFromHex )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )

import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "signData @(Jormungandr 'Mainnet)" $ do
        let fakeIn0 = Hash $ unsafeFromHex
                "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
        let fakeIn1 = Hash $ unsafeFromHex
                "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
        let addr0 = unsafeDecodeAddress @'Mainnet
                "ca1qw0rtl2hpm6zlj4vsjkjyrtk7x2ujclrx8wwxakh4yh7s2xegalpxyayx7e"
        let addr1 = unsafeDecodeAddress @'Mainnet
                "ca1qv474k2hgzr5h68v5khvfv6p95mac6wlkztw9azwe73casfk420ykxsjume"
        let change = unsafeDecodeAddress @'Mainnet
                "ca1q0nh79pzqxge7qy8jp2zu9rdxqsepe55qlw756zzuy8hgddqvru95l5syeu"
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                , TxOut change (Coin 13105)
                ]
            , gtExpected =
                "7bfbf66a97e0b4a96fa27c8fb80cb20f89280debf259b1a6b7e4d4b9e8c7c552"
            }
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                ]
            , gtExpected =
                "0be6fdb0c1d9c5c2493a319bcc659e97d1b04fcad002878e186f46852b6cf208"
            }
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                , (TxIn fakeIn1 1, Coin 42)
                , (TxIn fakeIn0 2, Coin 123456789)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 14)
                , TxOut addr1 (Coin 123456)
                ]
            , gtExpected =
                "115ba47ce89d8d92c5ed6b5c11be57ee5c757848f65a350eb49adff8f625b7a6"
            }

    describe "signData @(Jormungandr 'Testnet)" $ do
        let fakeIn0 = Hash $ unsafeFromHex
                "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
        let fakeIn1 = Hash $ unsafeFromHex
                "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
        let addr0 = unsafeDecodeAddress @'Testnet
                "ta1s0uz3rda4xx66fttdfxhdz9kh0pd5p9llhgeslmfll227yy2l9s66mhf7pv"
        let addr1 = unsafeDecodeAddress @'Testnet
                "ta1s09dcqava9nsl889fyrhuufx0reavt76qa803rzmllfur7cxcmt2xqsxwsm"
        let change = unsafeDecodeAddress @'Testnet
                "ta1sw4rdkek9rc5ywhrfna92wulm7ljdr5000g8kysz2hf2hgeyl5h755fhef2"
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                , TxOut change (Coin 13105)
                ]
            , gtExpected =
                "4c9097d2121d0a7f10c9c04d25a310999d36cf07ac232fd0e72feb1ea8e93dc7"
            }
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                ]
            , gtExpected =
                "1de8b9655106fe0b8c2e5952a9f105d518838296b79d542f4e96b5980ed2b909"
            }
        goldenTestSignData GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                , (TxIn fakeIn1 1, Coin 42)
                , (TxIn fakeIn0 2, Coin 123456789)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 14)
                , TxOut addr1 (Coin 123456)
                ]
            , gtExpected =
                "0ba2569f273b5bbb2cf37a749dab8416cf1574cc22b7757953d46d8a979657da"
            }

data GoldenTestSignData = GoldenTestSignData
    { gtInputs :: [(TxIn, Coin)]
    , gtOutputs :: [TxOut]
    , gtExpected :: ByteString
    }

-- | Generate tx ids for the given transaction and compare the result with an
-- expected output obtained from jcli (see appendix below)
--
-- Note that jcli doesn't give the fragment id but, it gives the signing data
-- (so the tx id as we knew it).
goldenTestSignData
    :: GoldenTestSignData
    -> SpecWith ()
goldenTestSignData (GoldenTestSignData ins outs expected) =
    it ("golden test: " <> B8.unpack expected) $
        hex (getHash $ signData ins outs) `shouldBe` expected
  where
    hex = convertToBase @ByteString @ByteString Base16

{-------------------------------------------------------------------------------
            Generating Golden Test Vectors For TxId
-------------------------------------------------------------------------------}

-- NETWORK=${NETWORK:-testnet}
-- case $NETWORK in
--   'mainnet')
--     DISCRIMINATION=""
--     ;;
--   'testnet')
--     DISCRIMINATION="--testing"
--     ;;
--   *)
--     echo "Unknown network: $NETWORK"
--     exit 1
-- esac
--
-- # Dummy Data
-- BLOCK0=13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba
-- FAKEIN0=666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4
-- FAKEIN1=1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1
--
-- # Generate some keys
-- ROOTPRV0=$(jcli key generate --type ed25519extended)
-- ROOTPUB0=$(echo $ROOTPRV0 | jcli key to-public)
-- ADDR0=$(jcli address single $DISCRIMINATION $ROOTPUB0)
-- echo "Addr 0: $ADDR0"
--
-- ROOTPRV1=$(jcli key generate --type ed25519extended)
-- ROOTPUB1=$(echo $ROOTPRV1 | jcli key to-public)
-- ADDR1=$(jcli address single $DISCRIMINATION $ROOTPUB1)
-- echo "Addr 1: $ADDR1"
--
-- ROOTPRV2=$(jcli key generate --type ed25519extended)
-- ROOTPUB2=$(echo $ROOTPRV2 | jcli key to-public)
-- CHANGE=$(jcli address single $DISCRIMINATION $ROOTPUB2)
-- echo "Change: $CHANGE"
--
-- echo ""
--
-- # One input, one output, one change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-output $ADDR0 1337 \
--   | jcli transaction finalize $CHANGE \
--   | jcli transaction add-witness /tmp/wit \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit /tmp/tx
-- echo -e "$TX\n"
--
-- #  One input, one output, no change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-output $ADDR0 1337 \
--   | jcli transaction finalize \
--   | jcli transaction add-witness /tmp/wit \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit /tmp/tx
-- echo -e "$TX\n"
--
-- #  3 inputs, 2 outputs, no change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit0
-- echo $ROOTPRV1 | jcli transaction make-witness $FAKEIN1 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit1
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-input $FAKEIN1 1 42 \
--   | jcli transaction add-input $FAKEIN0 2 123456789 \
--   | jcli transaction add-output $ADDR0 14 \
--   | jcli transaction add-output $ADDR1 123456 \
--   | jcli transaction finalize \
--   | jcli transaction add-witness /tmp/wit0 \
--   | jcli transaction add-witness /tmp/wit1 \
--   | jcli transaction add-witness /tmp/wit0 \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit* /tmp/tx
-- echo -e "$TX\n"
