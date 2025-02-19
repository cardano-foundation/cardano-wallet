{-# LANGUAGE QuasiQuotes #-}
module Cardano.Wallet.Deposit.Pure.API.AddressSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.Address
    ( DecodingError (..)
    , decodeAddress
    , encodeAddress
    )
import Cardano.Wallet.Read.Address
    ( isBootstrapCompactAddr
    , toShortByteString
    )
import Control.Monad
    ( forM_
    )
import Data.ByteString.Base58
    ( bitcoinAlphabet
    , decodeBase58
    , encodeBase58
    )
import Data.Either
    ( isLeft
    , isRight
    )
import Data.Function
    ( (&)
    )
import Data.Maybe
    ( isJust
    )
import Data.Text
    ( Text
    )
import Test.Cardano.Ledger.Core.Arbitrary
    ()
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , checkCoverage
    , counterexample
    , cover
    , elements
    , forAll
    , label
    , oneof
    , property
    , (===)
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "address codec" $ do
        it "rountrips correctly on random addresses" $ forAll arbitrary $ \x ->
            decodeAddress (encodeAddress x)
                === Right x

        it "decodeAddress text = Right addr ==> encodeAddress addr == text"
            $ checkCoverage $ forAll genArbitrarilyEncodedAddress $ \text -> do
                let getErrorLabel e = case e of
                        InvalidBech32Encoding _e -> "invalid bech32 encoding"
                        InvalidBase58Encoding -> "invalid base58 encoding"
                        InvalidHumanReadablePart _hrp -> "invalid hrp"
                        InvalidDataPart _ -> "invalid data part"
                        AddressFlavorMismatch -> "flavor mismatch"
                        AddressDecodingError _ -> "decoding error"
                        AddressNetworkMismatch -> "network mismatch"

                let res = decodeAddress text
                case res of
                    Right addr -> label "success" $ encodeAddress addr === text
                    Left e -> label (getErrorLabel e) $ property True
                    & cover 0.2 (isLeft res) "failure"
                    & cover 0.2 (isRight res) "success"

        it "isBootstrapAddr decides whether bech32 or base58 encoding is used"
            $ forAll arbitrary $ \addr ->
                let
                    isBase58 = isJust . decodeBase58 bitcoinAlphabet . T.encodeUtf8
                    isBech32 = isRight . Bech32.decodeLenient

                    encodedAddr = encodeAddress addr
                in
                    if isBootstrapCompactAddr addr
                    then property $ isBase58 encodedAddr
                    else property $ isBech32 encodedAddr
                    & counterexample (T.unpack encodedAddr)

        it "roundtrips correctly on some addresses from online examples"
            $ do
                let testCases =
                        [ "addr1z92l7rnra7sxjn5qv5fzc4fwsrrm29mgkleqj9a0y46j5lyjz4gwd3njhyqwntdkcm8rrgapudajydteywgtuvl6etjs9nqzg5"
                        , "addr_test1wppg9l6relcpls4u667twqyggkrpfrs5cdge9hhl9cv2upchtch0h"
                        , "37btjrVyb4KDXBNC4haBVPCrro8AQPHwvCMp3RFhhSVWwfFmZ6wwzSK6JK1hY6wHNmtrpTf1kdbva8TCneM2YsiXT7mrzT21EacHnPpz5YyUdj64na"
                        ]
                forM_ testCases $ \addr ->
                    encodeAddress <$> decodeAddress addr
                        `shouldBe` Right addr

        it "fails to decode addresses where the network tag doesn't match the bech32 hrp" $ do
            let secretlyMainnetAddr = "addr_test1z92l7rnra7sxjn5qv5fzc4fwsrrm29mgkleqj9a0y46j5lyjz4gwd3njhyqwntdkcm8rrgapudajydteywgtuvl6etjshn59kk"
            decodeAddress secretlyMainnetAddr
                `shouldBe` Left AddressNetworkMismatch

-- | Generate 'Text' heavily biased towards values of incorrectly encoded
-- addresses
genArbitrarilyEncodedAddress :: Gen Text
genArbitrarilyEncodedAddress = oneof
    [ encodeAddrBech32 <$> genAddrHrp <*> arbitrary
    , encodeAddrBase58 <$> arbitrary
    ]
  where
    encodeAddrBech32 hrp addr = Bech32.encodeLenient hrp dataPart
      where
        bytes = SBS.fromShort $ toShortByteString addr
        dataPart = Bech32.dataPartFromBytes bytes

    genAddrHrp = elements
        [ [Bech32.humanReadablePart|addr|]
        , [Bech32.humanReadablePart|addr_test|]
        , [Bech32.humanReadablePart|notaddr|]
        ]

    encodeAddrBase58 = T.decodeUtf8
        . encodeBase58 bitcoinAlphabet
        . SBS.fromShort
        . toShortByteString
