{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.TypesSpec (spec) where

import Prelude hiding
    ( id )

import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Types
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletMigrationInfo (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkInformation (..)
    , ApiNetworkTip (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiT (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , ApiWalletPassphrase (..)
    , ByronWalletPostData (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChainCode (..)
    , DelegationAddress (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PaymentAddress (..)
    , XPub (..)
    , networkDiscriminantVal
    , passphraseMaxLength
    , passphraseMinLength
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), ShelleyKey (..), publicKeySize )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( CheckSumBits
    , ConsistentEntropy
    , Entropy
    , EntropySize
    , MnemonicException (..)
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToBytes
    , entropyToMnemonic
    , mkEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Coin (..)
    , Direction (..)
    , EpochNo (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolId (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotNo (..)
    , SortOrder (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , computeUtxoStatistics
    , log10
    , poolIdBytesLength
    , walletNameMaxLength
    , walletNameMinLength
    )
import Control.Lens
    ( Lens', at, (^.) )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Crypto.Hash
    ( hash )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( isAlphaNum )
import Data.Either
    ( isRight )
import Data.FileEmbed
    ( embedFile, makeRelativeToProject )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Swagger
    ( Definitions
    , NamedSchema (..)
    , Operation
    , PathItem (..)
    , Schema
    , Swagger
    , ToSchema (..)
    , definitions
    , delete
    , get
    , patch
    , paths
    , post
    , put
    )
import Data.Swagger.Declare
    ( Declare )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Typeable
    ( Typeable, splitTyConApp, tyConName, typeRep )
import Data.Word
    ( Word32, Word8 )
import GHC.TypeLits
    ( KnownSymbol, natVal, symbolVal )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>)
    , (:>)
    , Capture
    , Header'
    , QueryParam
    , ReqBody
    , StdMethod (..)
    , Verb
    )
import Servant.Swagger.Test
    ( validateEveryToJSON )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Proxy (Proxy)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Aeson.Internal.GoldenSpecs
    ( goldenSpecsWithNotePlain )
import Test.Aeson.Internal.RoundtripSpecs
    ( roundtripSpecs )
import Test.Aeson.Internal.Utils
    ( TypeName (..), TypeNameInfo (..), mkTypeNameInfo )
import Test.Hspec
    ( Spec
    , SpecWith
    , describe
    , expectationFailure
    , it
    , runIO
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , frequency
    , oneof
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Time
    ( genUniformTime )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Prelude

spec :: Spec
spec = do
    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            jsonRoundtripAndGolden $ Proxy @(ApiAddress 'Testnet)
            jsonRoundtripAndGolden $ Proxy @ApiTimeReference
            jsonRoundtripAndGolden $ Proxy @ApiNetworkTip
            jsonRoundtripAndGolden $ Proxy @ApiBlockReference
            jsonRoundtripAndGolden $ Proxy @ApiNetworkInformation
            jsonRoundtripAndGolden $ Proxy @ApiStakePool
            jsonRoundtripAndGolden $ Proxy @(AddressAmount 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiTransaction 'Testnet)
            jsonRoundtripAndGolden $ Proxy @ApiWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWalletMigrationInfo
            jsonRoundtripAndGolden $ Proxy @ApiWalletPassphrase
            jsonRoundtripAndGolden $ Proxy @ApiUtxoStatistics
            jsonRoundtripAndGolden $ Proxy @ApiFee
            jsonRoundtripAndGolden $ Proxy @ApiStakePoolMetrics
            jsonRoundtripAndGolden $ Proxy @ApiTxId
            jsonRoundtripAndGolden $ Proxy @(PostTransactionData 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(PostTransactionFeeData 'Testnet)
            jsonRoundtripAndGolden $ Proxy @WalletPostData
            jsonRoundtripAndGolden $ Proxy @ByronWalletPostData
            jsonRoundtripAndGolden $ Proxy @WalletPutData
            jsonRoundtripAndGolden $ Proxy @WalletPutPassphraseData
            jsonRoundtripAndGolden $ Proxy @(ApiT (Hash "Tx"))
            jsonRoundtripAndGolden $ Proxy @(ApiT (Passphrase "encryption"))
            jsonRoundtripAndGolden $ Proxy @(ApiT (WalletDelegation (ApiT PoolId)))
            jsonRoundtripAndGolden $ Proxy @(ApiT Address, Proxy 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiT AddressPoolGap)
            jsonRoundtripAndGolden $ Proxy @(ApiT Direction)
            jsonRoundtripAndGolden $ Proxy @(ApiT TxStatus)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletBalance)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletId)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletName)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletPassphraseInfo)
            jsonRoundtripAndGolden $ Proxy @(ApiT SyncProgress)

    describe "Textual encoding" $ do
        describe "Can perform roundtrip textual encoding & decoding" $ do
            textRoundtrip $ Proxy @Iso8601Time
            textRoundtrip $ Proxy @SortOrder

    describe "AddressAmount" $ do
        it "fromText . toText === pure"
            $ property
            $ \(i :: AddressAmount 'Testnet) ->
                (fromText . toText) i === pure i
        it "fromText \"22323\"" $
            let err =
                    "Parse error. " <>
                    "Expecting format \"<amount>@<address>\" but got \"22323\""
            in
                fromText @(AddressAmount 'Testnet) "22323"
                    === Left (TextDecodingError err)

    describe
        "can perform roundtrip HttpApiData serialization & deserialization" $ do
            httpApiDataRoundtrip $ Proxy @(ApiT WalletId)
            httpApiDataRoundtrip $ Proxy @(ApiT AddressState)
            httpApiDataRoundtrip $ Proxy @Iso8601Time
            httpApiDataRoundtrip $ Proxy @(ApiT SortOrder)

    describe
        "verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using validateToJSON." $
        validateEveryToJSON (Proxy :: Proxy (Api 'Testnet))

    describe
        "verify that every path specified by the servant server matches an \
        \existing path in the specification" $
        validateEveryPath (Proxy :: Proxy (Api 'Testnet))

    describe "verify JSON parsing failures too" $ do
        it "ApiT Address" $ do
            let msg = "Error in $: Unable to decode Address: \
                    \encoding is neither Bech32 nor Base58."
            Aeson.parseEither parseJSON [aesonQQ|"-----"|]
                `shouldBe` (Left @String @(ApiT Address, Proxy 'Testnet) msg)

        it "ApiT (Passphrase \"encryption\") (too short)" $ do
            let minLength = passphraseMinLength (Proxy :: Proxy "encryption")
            let msg = "Error in $: passphrase is too short: \
                    \expected at least " <> show minLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|"patate"|]
                `shouldBe` (Left @String @(ApiT (Passphrase "encryption")) msg)

        it "ApiT (Passphrase \"encryption\") (too long)" $ do
            let maxLength = passphraseMaxLength (Proxy :: Proxy "encryption")
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show maxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*maxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "encryption")) msg)

        it "ApiT WalletName (too short)" $ do
            let msg = "Error in $: name is too short: \
                    \expected at least " <> show walletNameMinLength <> " character"
            Aeson.parseEither parseJSON [aesonQQ|""|]
                `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiT WalletName (too long)" $ do
            let msg = "Error in $: name is too long: \
                    \expected at most " <> show walletNameMaxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*walletNameMaxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiMnemonicT '[12] (not enough words)" $ do
            let msg = "Error in $: Invalid number of words: 12 words\
                    \ are expected."
            Aeson.parseEither parseJSON [aesonQQ|
                ["toilet", "toilet", "toilet"]
            |] `shouldBe` (Left @String @(ApiMnemonicT '[12] "test") msg)

        it "ApiT AddressPoolGap (too small)" $ do
            let msg = "Error in $: An address pool gap must be a natural number between "
                    <> show (getAddressPoolGap minBound)
                    <> " and "
                    <> show (getAddressPoolGap maxBound)
                    <> "."
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap minBound - 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT AddressPoolGap (too big)" $ do
            let msg = "Error in $: An address pool gap must be a natural number between "
                    <> show (getAddressPoolGap minBound)
                    <> " and "
                    <> show (getAddressPoolGap maxBound)
                    <> "."
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap maxBound + 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT AddressPoolGap (not a integer)" $ do
            let msg = "Error in $: expected Integer, encountered floating number\
                    \ 2.5"
            Aeson.parseEither parseJSON [aesonQQ|
                2.5
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT (Hash \"Tx\")" $ do
            let msg = "Error in $: Unable to decode (Hash \"Tx\"): \
                    \expected Base16 encoding"
            Aeson.parseEither parseJSON [aesonQQ|
                "-----"
            |] `shouldBe` (Left @String @(ApiT (Hash "Tx")) msg)

        it "ApiT WalletId" $ do
            let msg = "Error in $: wallet id should be a hex-encoded \
                    \string of 40 characters"
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT WalletId) msg)

        it "AddressAmount (too small)" $ do
            let msg = "Error in $.amount.quantity: expected Natural, \
                    \encountered negative number -14"
            Aeson.parseEither parseJSON [aesonQQ|
                { "address": "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
                , "amount": {"unit":"lovelace","quantity":-14}
                }
            |] `shouldBe` (Left @String @(AddressAmount 'Testnet) msg)

        it "AddressAmount (too big)" $ do
            let msg = "Error in $: invalid coin value: value has to be lower \
                    \than or equal to " <> show (getCoin maxBound)
                    <> " lovelace."
            Aeson.parseEither parseJSON [aesonQQ|
                { "address": "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
                , "amount":
                    { "unit":"lovelace"
                    ,"quantity":#{getCoin maxBound + 1}
                    }
                }
            |] `shouldBe` (Left @String @(AddressAmount 'Testnet) msg)

        it "ApiT PoolId" $ do
            let msg = "Error in $: \"stake pool id wrongly formatted: expected \
                      \hex string - the exact error: base16: input: \
                      \invalid encoding at offset: 0\""
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT PoolId" $ do
            let msg = "Error in $: stake pool id invalid: expected "
                    <>  show poolIdBytesLength
                    <> " bytes but got 22"
            Aeson.parseEither parseJSON [aesonQQ|
                "4c43d68b21921034519c36d2475f5adba989bb4465ec"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)


    describe "verify HttpApiData parsing failures too" $ do
        it "ApiT WalletId" $ do
            let msg = "wallet id should be a hex-encoded string of 40 characters"
            parseUrlPiece "invalid-id"
                `shouldBe` (Left @Text @(ApiT WalletId) msg)

        it "ApiT AddressState" $ do
            let msg = "Unable to decode the given value: \"patate\".\
                    \ Please specify one of the following values: used, unused."
            parseUrlPiece "patate"
                `shouldBe` (Left @Text @(ApiT AddressState) msg)

    describe "encodeAddress & decodeAddress (Mainnet)" $ do
        let proxy = Proxy @'Mainnet
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Mainnet) ->
                (ShowFmt <$> decodeAddress @'Mainnet (encodeAddress @'Mainnet a))
                    === Right (ShowFmt a)
        negativeTest proxy "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
            ("This address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @'Mainnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ca1qvqsyqcyq5rqwzqfpg9scrgk66qs0"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("This type of address is not supported.")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("This type of address is not supported.")
        -- NOTE:
        -- Data below have been generated with [jcli](https://github.com/input-output-hk/jormungandr/tree/master/doc/jcli)
        -- as described in the annex at the end of the file.
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "addr1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677z5t3m7d"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "addr1q00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7n5fgsv"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "addr1q3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677\
            \rvjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wvuat2l"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "addr1qn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr\
            \7sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8n8hz07"

    describe "balance-check - decodeAddress (Testnet)" $ do
        let addrs =
                [ "DdzFFzCqrhsh8jZMJXCtCF9mihxDLFzZNzAHYMq1U2xK9QHSqTLskNTysn2X8jgiavMSedregoQNEy9cSGwiqTc5JyXuxsSRnyweXt81"
                , "DdzFFzCqrhsjmWi2azAPFVKAo1ifnxF5ZxE8btf6459BidBNLSmNMf43mxXdfe5W6egtFVEta23zAk4HGMUnZv5tX42HsMwNgauWn1yC"
                , "DdzFFzCqrhsk6erDuTLsQvnRs2rwcCCDoexSHKYM2wXbpJ9Megj2QX7L7fhVFL7FeBEhPPyHP4WcThW82FBaQLyQHFh9QLp8PMBzgNKa"
                , "DdzFFzCqrhsp99qGU1iRUTCUkFY2ftuePpXxVYSJPv2LEWZdVABujqAE8UdHaShdvXpDVhbxnZKon8DZvFDUgppFDWMKL8RgJxGTsUX1"
                , "DdzFFzCqrhsqbscWBi1sBCNU3MpTuRCN693etZQuwEDbmRdXkbdPdPZagpW1SKs5vL3AQDgPF4DqegjxPBRqMarW12h4E5GjB8Uhq8t5"
                , "DdzFFzCqrhssubhp3zw7ttuUrTSwmizZwt3EgbcSWHJvCCT22hnbHyor9S4R8XBABxBnmo7p93QWumBUxoLbSCpSQMxiNUCnY6kXyUxL"
                , "DdzFFzCqrhsuvrWexrNB3cpBA2VuAFRnq7XWw6xrypk2dNdH9q8KUNVf6af4iZBjTyAADsAkdcgACKsCUaUvDss5rWv1B3vN1TERzxp2"
                , "DdzFFzCqrhsxmdiCWWpkC3PuAr2w3WpQzRxHvUJpygvZxqL4ksYusSiC321EyD896iWHo9vBh65ADe4cY1GSZyh5jr5yQvXbX3C9gsUP"
                , "DdzFFzCqrhszN5PcGnbUsbbi4gVPzHzmbVDNoQVLvgmDXsFo9TsYaY7GtfpZKnGy9EgjRNcLogyPd9EjnwNcYJXhZLprz4zobz8PyjwG"
                , "DdzFFzCqrhszsMF5W2mCRirNX1ZdwE4UtdWeFdvt9F8DTum9bvnNswwhvVgXjNtihsV8TJLkX78U23BWdwc8Q2L47K5LLxKZG86DYfns"
                , "DdzFFzCqrht2u75sMbuiBKxqUt9sbiS9aaoS39tyDgQNLtcMsS7vmpKpKYbAWVRwyWJcr8XQPckS7gZ9UYYcveySCUn6aUUxx6ejg9LT"
                , "DdzFFzCqrht2vq14VoKFzyZS4kKoFaPh6PTzjWoaMPvENDuc87wG2qZWFhiuEPBChRWnJjUpxPcn199BpBCk51gBSS27fv9uCZNs9hhU"
                , "DdzFFzCqrht3obxWBrfX7CB7nYnPcQgJuwp5jnpWb46jQ6HGpbo48mHFgarRaiYqVecS9VLNujeB4Xa1Ppm3XKoYjzGmrZkkicm5rRdx"
                , "DdzFFzCqrht4hsxVpJ5sDVfWfyQBwB7LSVrvn1qbwGA6xF4LogCsj2dmreh1PjLD7D4MJAKRRVqpJyxjKaHtMNhYgkBJLTcuWf2i6SZk"
                , "DdzFFzCqrht4PRDBs1j7vk7M7YLMWcwt9pPfDTuEAuBae16gCV1RWb22NjSsQHGsxaJAUDY7nYY8frVPfb6qZx1ru6Bh56GNVcb6PMsp"
                , "DdzFFzCqrht6kPg8Z9FG5YApsXxFd32UKTiSGNvjbEp8NQiyTDVr5WbPWrM1iLtwZcCeVC6c5CXBLwvtFoRoXa2kVvet8vwqRcdvamVB"
                , "DdzFFzCqrht7FHQQb7GLzqR2JzENiQHBT8UEiL3HCA8CDwR4ZFBYWJKHqML3jg737c8cYwv52YyB2NdYGiQSBW68ERdk1px2ZzjKWLWY"
                , "DdzFFzCqrht8DfNtzu1zF8LfXXSkqv6m59pNt7LNaBi3MDV2Dz8S2YAa4ppXgSgQby1MbiFGJ4N2H9cAd4QysjPg9bWW1ejSDWYgrRf8"
                ]
        forM_ addrs $ \addr -> it (show addr) $ do
            decodeAddress @'Testnet addr `shouldSatisfy` isRight

    describe "encodeAddress & decodeAddress (Testnet)" $ do
        let proxy = Proxy @'Testnet
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Testnet) ->
                (ShowFmt <$> decodeAddress @'Testnet (encodeAddress @'Testnet a))
                    === Right (ShowFmt a)
        negativeTest proxy "ca1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zqx4le2"
            ("This address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @'Testnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ta1dvqsyqcyq5rqwzqfpg9scrg5v76st"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("This type of address is not supported.")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("This type of address is not supported.")
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "addr1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zgltetp"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "addr1s00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr70qn29q"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "addr1s3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677\
            \rvjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wkdnx06"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "addr1sn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr\
            \7sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8fke02m"

    describe "pointless tests to trigger coverage for record accessors" $ do
        it "ApiAddress" $ property $ \x ->
            let
                x' = ApiAddress
                    { id = id (x :: ApiAddress 'Testnet)
                    , state = state (x :: ApiAddress 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiWallet" $ property $ \x ->
            let
                x' = ApiWallet
                    { id = id (x :: ApiWallet)
                    , addressPoolGap = addressPoolGap (x :: ApiWallet)
                    , balance = balance (x :: ApiWallet)
                    , delegation = delegation (x :: ApiWallet)
                    , name = name (x :: ApiWallet)
                    , passphrase = passphrase (x :: ApiWallet)
                    , reward = reward (x :: ApiWallet)
                    , state = state (x :: ApiWallet)
                    , tip = tip (x :: ApiWallet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiByronWallet" $ property $ \x ->
            let
                x' = ApiByronWallet
                    { id = id (x :: ApiByronWallet)
                    , balance = balance (x :: ApiByronWallet)
                    , name = name (x :: ApiByronWallet)
                    , passphrase = passphrase (x :: ApiByronWallet)
                    , state = state (x :: ApiByronWallet)
                    , tip = tip (x :: ApiByronWallet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiByronWalletMigrationInfo" $ property $ \x ->
            let
                x' = ApiByronWalletMigrationInfo
                    { migrationCost =
                        migrationCost (x :: ApiByronWalletMigrationInfo)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiWalletPassphrase" $ property $ \x ->
            let
                x' = ApiWalletPassphrase
                    { passphrase =
                        passphrase (x :: ApiWalletPassphrase)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiFee" $ property $ \x ->
            let
                x' = ApiFee
                    { amount = amount (x :: ApiFee)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiTxId" $ property $ \x ->
            let
                x' = ApiTxId
                    { id = id (x :: ApiTxId)
                    }
            in
                x' === x .&&. show x' === show x
        it "WalletPostData" $ property $ \x ->
            let
                x' = WalletPostData
                    { addressPoolGap = addressPoolGap (x :: WalletPostData)
                    , mnemonicSentence = mnemonicSentence (x :: WalletPostData)
                    , mnemonicSecondFactor = mnemonicSecondFactor (x :: WalletPostData)
                    , name = name (x :: WalletPostData)
                    , passphrase = passphrase (x :: WalletPostData)
                    }
            in
                x' === x .&&. show x' === show x
        it "WalletPutData" $ property $ \x ->
            let
                x' = WalletPutData
                    { name = name (x :: WalletPutData)
                    }
            in
                x' === x .&&. show x' === show x
        it "WalletPutPassphraseData" $ property $ \x ->
            let
                x' = WalletPutPassphraseData
                    { oldPassphrase = oldPassphrase (x :: WalletPutPassphraseData)
                    , newPassphrase = newPassphrase (x :: WalletPutPassphraseData)
                    }
            in
                x' === x .&&. show x' === show x
        it "PostTransactionData" $ property $ \x ->
            let
                x' = PostTransactionData
                    { payments = payments (x :: PostTransactionData 'Testnet)
                    , passphrase = passphrase (x :: PostTransactionData 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "PostTransactionFeeData" $ property $ \x ->
            let
                x' = PostTransactionFeeData
                    { payments = payments (x :: PostTransactionFeeData 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "PostExternalTransactionData" $ property $ \x ->
            let
                x' = PostExternalTransactionData
                    { payload = payload (x :: PostExternalTransactionData)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiTransaction" $ property $ \x ->
            let
                x' = ApiTransaction
                    { id = id (x :: ApiTransaction 'Testnet)
                    , amount = amount (x :: ApiTransaction 'Testnet)
                    , insertedAt = insertedAt (x :: ApiTransaction 'Testnet)
                    , pendingSince = pendingSince (x :: ApiTransaction 'Testnet)
                    , depth = depth (x :: ApiTransaction 'Testnet)
                    , direction = direction (x :: ApiTransaction 'Testnet)
                    , inputs = inputs (x :: ApiTransaction 'Testnet)
                    , outputs = outputs (x :: ApiTransaction 'Testnet)
                    , status = status (x :: ApiTransaction 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "AddressAmount" $ property $ \x ->
            let
                x' = AddressAmount
                    { address = address (x :: AddressAmount 'Testnet)
                    , amount = amount (x :: AddressAmount 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiTimeReference" $ property $ \x ->
            let
                x' = ApiTimeReference
                    { time = time (x :: ApiTimeReference)
                    , block = block (x :: ApiTimeReference)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiBlockReference" $ property $ \x ->
            let
                x' = ApiBlockReference
                    { slotNumber = slotNumber (x :: ApiBlockReference)
                    , epochNumber = epochNumber (x :: ApiBlockReference)
                    , height = height (x :: ApiBlockReference)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiNetworkTip" $ property $ \x ->
            let
                x' = ApiNetworkTip
                    { slotNumber = slotNumber (x :: ApiNetworkTip)
                    , epochNumber = epochNumber (x :: ApiNetworkTip)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiNetworkInformation" $ property $ \x ->
            let
                x' = ApiNetworkInformation
                    { syncProgress = syncProgress (x :: ApiNetworkInformation)
                    , nodeTip = nodeTip (x :: ApiNetworkInformation)
                    , networkTip = networkTip (x :: ApiNetworkInformation)
                    }
            in
                x' === x .&&. show x' === show x

-- Golden tests files are generated automatically on first run. On later runs
-- we check that the format stays the same. The golden files should be tracked
-- in git.
--
-- Example:
-- >>> roundtripAndGolden $ Proxy @ Wallet
--
-- ...will compare @ToJSON@ of @Wallet@ against `Wallet.json`. It may either
-- match and succeed, or fail and write `Wallet.faulty.json` to disk with the
-- new format. Faulty golden files should /not/ be commited.
--
-- The directory `test/data/Cardano/Wallet/Api` is used.
jsonRoundtripAndGolden
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => Proxy a
    -> Spec
jsonRoundtripAndGolden proxy = do
    roundtripSpecs proxy
    typeNameInfo <- runIO mkCompatibleTypeNameInfo
    goldenSpecsWithNotePlain settings typeNameInfo Nothing
  where
    -- NOTE
    -- We use a custom 'TypeNameInfo' instead of the default one provided by
    -- @hspec-golden-aeson@ because the defaults generates names that are
    -- invalid for Windows file-system.
    mkCompatibleTypeNameInfo :: IO (TypeNameInfo a)
    mkCompatibleTypeNameInfo = do
        typeNameInfo <- mkTypeNameInfo settings proxy
        pure $ typeNameInfo
            { typeNameTypeName =
                mkValidForWindows (typeNameTypeName typeNameInfo)
            }
      where
        mkValidForWindows :: TypeName -> TypeName
        mkValidForWindows (TypeName typeName) =
            TypeName (filter isAlphaNum typeName)

    settings :: Settings
    settings = defaultSettings
        { goldenDirectoryOption =
            CustomDirectoryName $ foldr (</>) mempty
                [ "test", "data", "Cardano", "Wallet", "Api" ]
        , useModuleNameAsSubDirectory =
            False
        , sampleSize = 10
        }

-- Perform roundtrip tests for FromHttpApiData & ToHttpApiData instances
httpApiDataRoundtrip
    :: forall a.
        ( Arbitrary a
        , FromHttpApiData a
        , ToHttpApiData a
        , Typeable a
        , Eq a
        , Show a
        )
    => Proxy a
    -> Spec
httpApiDataRoundtrip proxy =
    it ("URL encoding of " <> cons (typeRep proxy)) $ property $ \(x :: a) -> do
        let bytes = toUrlPiece x
        let x' = parseUrlPiece bytes
        x' `shouldBe` Right x
  where
    cons rep =
        let
            (c, args) = splitTyConApp rep
        in
            case args of
                [] ->
                    tyConName c
                xs ->
                    "(" <> tyConName c <> " " <> unwords (cons <$> xs) <> ")"

-- | Generate addresses from the given keys and compare the result with an
-- expected output obtained from jcli (see appendix below)
goldenTestAddr
    :: forall n. (DelegationAddress n ShelleyKey, EncodeAddress n)
    => Proxy n
    -> [ByteString]
    -> Text
    -> SpecWith ()
goldenTestAddr _proxy pubkeys expected = it ("golden test: " <> T.unpack expected) $ do
    case traverse (convertFromBase Base16) pubkeys of
        Right [spendingKey] -> do
            let xpub = ShelleyKey (XPub spendingKey chainCode)
            let addr = encodeAddress @n (paymentAddress @n xpub)
            addr `shouldBe` expected
        Right [spendingKey, delegationKey] -> do
            let xpubSpending = ShelleyKey (XPub spendingKey chainCode)
            let xpubDeleg = ShelleyKey (XPub delegationKey chainCode)
            let addr = encodeAddress @n (delegationAddress @n xpubSpending xpubDeleg)
            addr `shouldBe` expected
        _ ->
            expectationFailure "goldenTestAddr: provided invalid inputs public keys"
  where
    chainCode = ChainCode "<ChainCode is not used by singleAddressFromKey>"

negativeTest
    :: forall n. DecodeAddress n
    => Proxy n
    -> Text
    -> String
    -> SpecWith ()
negativeTest _proxy bytes msg = it ("decodeAddress failure: " <> msg) $
    decodeAddress @n bytes === Left (TextDecodingError msg)

{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Proxy 'Testnet) where
    shrink _ = []
    arbitrary = pure Proxy

instance Arbitrary (ApiAddress t) where
    shrink _ = []
    arbitrary = ApiAddress
        <$> fmap (, Proxy @t) arbitrary
        <*> arbitrary

instance Arbitrary AddressState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Address where
    arbitrary = (unShowFmt . fst)
        <$> arbitrary @(ShowFmt Address, Proxy 'Testnet)

instance {-# OVERLAPS #-} KnownNetwork network
    => Arbitrary (ShowFmt Address, Proxy (network :: NetworkDiscriminant)) where
    arbitrary = do
        let proxy = Proxy @network
        addr <- ShowFmt <$> frequency
            [ (10, genAddress (single @network) (grouped @network))
            , (1, genLegacyAddress (30, 100))
            ]
        return (addr, proxy)

genAddress :: Word8 -> Word8 -> Gen Address
genAddress singleByte groupedByte = oneof
    [ (\bytes -> Address (BS.pack (singleByte:bytes)))
        <$> vectorOf publicKeySize arbitrary
    , (\bytes -> Address (BS.pack (groupedByte:bytes)))
        <$> vectorOf (2*publicKeySize) arbitrary
    ]

genLegacyAddress :: (Int, Int) -> Gen Address
genLegacyAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    addrPayload <- BS.pack <$> vectorOf n arbitrary
    let crc = BS.pack [26,1,2,3,4]
    return $ Address (prefix <> addrPayload <> crc)

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    arbitrary = Quantity <$> arbitraryBoundedEnum

instance Arbitrary ApiWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWalletMigrationInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletPassphrase where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiFee where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiTxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Iso8601Time where
    arbitrary = Iso8601Time <$> genUniformTime

instance Arbitrary SortOrder where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary WalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ByronWalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutPassphraseData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (WalletDelegation (ApiT PoolId)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes

instance Arbitrary ApiStakePoolMetrics where
    arbitrary = do
        stakes <- Quantity . fromIntegral <$> choose (1::Integer, 1000000000000)
        blocks <- Quantity . fromIntegral <$> choose (1::Integer, 1000*22600)
        pure $ ApiStakePoolMetrics stakes blocks

instance Arbitrary ApiStakePool where
    arbitrary = ApiStakePool
        <$> arbitrary
        <*> arbitrary
        <*> choose (0.0, 1.0)

instance Arbitrary WalletId where
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        WalletName . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink (WalletName t)
        | T.length t <= walletNameMinLength = []
        | otherwise = [WalletName $ T.take walletNameMinLength t]

instance (PassphraseMaxLength purpose, PassphraseMinLength purpose) =>
    Arbitrary (Passphrase purpose) where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy purpose
    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes ]
      where p = Proxy :: Proxy purpose

instance Arbitrary WalletPassphraseInfo where
    arbitrary = WalletPassphraseInfo <$> genUniformTime

instance Arbitrary SyncProgress where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ natVal @n Proxy
            entropy =
                mkEntropy  @n . B8.pack <$> vectorOf (size `quot` 8) arbitrary
        in
            either (error . show . UnexpectedEntropyError) Prelude.id <$> entropy

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    )
    => Arbitrary (ApiMnemonicT (mw ': '[]) purpose)
  where
    arbitrary = do
        ent <- arbitrary @(Entropy n)
        return $ ApiMnemonicT
            ( Passphrase $ entropyToBytes ent
            , mnemonicToText $ entropyToMnemonic ent
            )

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , Arbitrary (ApiMnemonicT rest purpose)
    )
    => Arbitrary (ApiMnemonicT (mw ': rest) purpose)
  where
    arbitrary = do
        ApiMnemonicT x <- arbitrary @(ApiMnemonicT '[mw] purpose)
        ApiMnemonicT y <- arbitrary @(ApiMnemonicT rest purpose)
        -- NOTE
        -- If we were to "naively" combine previous generators without weights,
        -- we would be tilting probabilities towards the leftmost element, so
        -- that every element would be twice as likely to appear as its right-
        -- hand neighbour, with an exponential decrease. (After the 7th element,
        -- subsequent elements would have less than 1 percent chance of
        -- appearing.) By tweaking the weights a bit as we have done below, we
        -- make it possible for every element to have at least 10% chance of
        -- appearing, for lists up to 10 elements.
        frequency
            [ (1, pure $ ApiMnemonicT x)
            , (5, pure $ ApiMnemonicT y)
            ]

instance Arbitrary ApiTimeReference where
    arbitrary = ApiTimeReference <$> genUniformTime <*> arbitrary
    shrink (ApiTimeReference t b) = ApiTimeReference t <$> shrink b

instance Arbitrary ApiBlockReference where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiNetworkTip where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiNetworkInformation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = SlotId <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary (AddressAmount t) where
    arbitrary = AddressAmount
        <$> fmap (, Proxy @t) arbitrary
        <*> arbitrary
    shrink _ = []

instance Arbitrary (PostTransactionData t) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (PostTransactionFeeData t) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PostExternalTransactionData where
    arbitrary = do
        count <- choose (0, 32)
        bytes <- BS.pack <$> replicateM count arbitrary
        return $ PostExternalTransactionData bytes
    shrink (PostExternalTransactionData bytes) =
        PostExternalTransactionData . BS.pack <$> shrink (BS.unpack bytes)

instance Arbitrary (ApiTransaction t) where
    shrink = genericShrink
    arbitrary = do
        txStatus <- arbitrary
        txInsertedAt <- case txStatus of
            (ApiT Pending) -> pure Nothing
            (ApiT InLedger) -> arbitrary
        txPendingSince <- case txStatus of
            (ApiT Pending) -> arbitrary
            (ApiT InLedger) -> pure Nothing
        ApiTransaction
            <$> arbitrary
            <*> arbitrary
            <*> pure txInsertedAt
            <*> pure txPendingSince
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> pure txStatus

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 1000000000000000)

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        -- NOTE: No need for a crazy high indexes
        <*> Test.QuickCheck.scale (`mod` 3) arbitrary

instance Arbitrary ApiUtxoStatistics where
    arbitrary = do
        utxos <- arbitrary
        let (UTxOStatistics histoBars stakes bType) =
                computeUtxoStatistics log10 utxos
        let boundCountMap =
                Map.fromList $ map (\(HistogramBar k v)-> (k,v)) histoBars
        return $ ApiUtxoStatistics
            (Quantity $ fromIntegral stakes)
            (ApiT bType)
            boundCountMap

instance Arbitrary (ApiTxInput t) where
    shrink _ = []
    arbitrary = ApiTxInput <$> arbitrary <*> arbitrary

instance Arbitrary (Quantity "slot" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary Direction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Quantity "block" Natural) where
    arbitrary  =fmap (Quantity . fromIntegral) (arbitrary @Word32)

{-------------------------------------------------------------------------------
                   Specification / Servant-Swagger Machinery

  Below is a bit of complicated API-Level stuff in order to achieve two things:

  1/ Verify that every response from the API that actually has a JSON content
     type returns a JSON instance that matches the JSON format described by the
     specification (field names should be the same, and constraints on values as
     well).
     For this, we need three things:
         - ToJSON instances on all those types, it's a given with the above
         - Arbitrary instances on all those types, that reflect as much as
           possible, all possible values of those types. Also given by using
           'genericArbitrary' whenever possible.
         - ToSchema instances which tells how do a given type should be
           represented.
     The trick is for the later point. In a "classic" scenario, we would have
     defined the `ToSchema` instances directly in Haskell on our types, which
     eventually becomes a real pain to maintain. Instead, we have written the
     spec by hand, and we want to check that our implementation matches it.
     So, we "emulate" the 'ToSchema' instance by:
         - Parsing the specification file (which is embedded at compile-time)
         - Creating missing 'ToSchema' by doing lookups in that global schema

  2/ The above verification is rather weak, because it just controls the return
     types of endpoints, but not that those endpoints are somewhat valid. Thus,
     we've also built another check 'validateEveryPath' which crawls our servant
     API type, and checks whether every path we have in our API appears in the
     specification. It does it by defining a few recursive type-classes to
     crawl the API, and for each endpoint:
         - construct the corresponding path (with verb)
         - build an HSpec scenario which checks whether the path is present
    This seemingly means that the identifiers we use in our servant paths (in
    particular, those for path parameters) should exactly match the specs.

-------------------------------------------------------------------------------}

-- | Specification file, embedded at compile-time and decoded right away
specification :: Swagger
specification =
    unsafeDecode bytes
  where
    bytes = $(
        let swaggerYaml = "../../specifications/api/swagger.yaml"
        in liftIO (lookupEnv "SWAGGER_YAML") >>=
        maybe (makeRelativeToProject swaggerYaml) pure >>=
        embedFile
        )
    unsafeDecode =
        either (error . (msg <>) . show) Prelude.id . Yaml.decodeEither'
    msg = "Whoops! Failed to parse or find the api specification document: "

instance ToSchema (ApiAddress t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAddress"

instance ToSchema ApiWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWallet"

instance ToSchema ApiByronWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWallet"

instance ToSchema ApiByronWalletMigrationInfo where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiByronWalletMigrationInfo"

instance ToSchema ApiWalletPassphrase where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletPassphrase"

instance ToSchema ApiStakePool where
    declareNamedSchema _ = declareSchemaForDefinition "ApiStakePool"

instance ToSchema ApiStakePoolMetrics where
    declareNamedSchema _ = declareSchemaForDefinition "ApiStakePoolMetrics"

instance ToSchema ApiFee where
    declareNamedSchema _ = declareSchemaForDefinition "ApiFee"

instance ToSchema ApiTxId where
    declareNamedSchema _ = declareSchemaForDefinition "ApiTxId"

instance ToSchema WalletPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletPostData"

instance ToSchema ByronWalletPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletPostData"

instance ToSchema WalletPutData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletPutData"

instance ToSchema WalletPutPassphraseData where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletPutPassphraseData"

instance ToSchema (PostTransactionData t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostTransactionData"

instance ToSchema (PostTransactionFeeData t) where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiPostTransactionFeeData"

instance ToSchema (ApiTransaction t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiTransaction"

instance ToSchema ApiUtxoStatistics where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletUTxOsStatistics"

instance ToSchema ApiNetworkInformation where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkInformation"

-- | Utility function to provide an ad-hoc 'ToSchema' instance for a definition:
-- we simply look it up within the Swagger specification.
declareSchemaForDefinition :: T.Text -> Declare (Definitions Schema) NamedSchema
declareSchemaForDefinition ref =
    case specification ^. definitions . at ref of
        Nothing -> error $
            "unable to find the definition for " <> show ref <> " in the spec"
        Just schema ->
            return $ NamedSchema (Just ref) schema

-- | Verify that all servant endpoints are present and match the specification
class ValidateEveryPath api where
    validateEveryPath :: Proxy api -> Spec

instance {-# OVERLAPS #-} HasPath a => ValidateEveryPath a where
    validateEveryPath proxy = do
        let (verb, path) = getPath proxy
        it (show verb <> " " <> path <> " exists in specification") $ do
            case specification ^. paths . at path of
                Just item | isJust (item ^. atMethod verb) -> return @IO ()
                _ -> fail "couldn't find path in specification"

instance (ValidateEveryPath a, ValidateEveryPath b) => ValidateEveryPath (a :<|> b) where
    validateEveryPath _ = do
        validateEveryPath (Proxy @a)
        validateEveryPath (Proxy @b)

-- | Extract the path of a given endpoint, in a format that is swagger-friendly
class HasPath api where
    getPath :: Proxy api -> (StdMethod, String)

instance (Method m) => HasPath (Verb m s ct a) where
    getPath _ = (method (Proxy @m), "")

instance (KnownSymbol path, HasPath sub) => HasPath (path :> sub) where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in (verb, "/" <> symbolVal (Proxy :: Proxy path) <> sub)

instance (KnownSymbol param, HasPath sub) => HasPath (Capture param t :> sub)
  where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in (verb, "/{" <> symbolVal (Proxy :: Proxy param) <> "}" <> sub)

instance HasPath sub => HasPath (ReqBody a b :> sub) where
    getPath _ = getPath (Proxy @sub)

instance HasPath sub => HasPath (QueryParam a b :> sub) where
    getPath _ = getPath (Proxy @sub)

instance HasPath sub => HasPath (Header' opts name ty :> sub) where
    getPath _ = getPath (Proxy @sub)

-- A way to demote 'StdMethod' back to the world of values. Servant provides a
-- 'reflectMethod' that does just that, but demote types to raw 'ByteString' for
-- an unknown reason :/
instance Method 'GET where method _ = GET
instance Method 'POST where method _ = POST
instance Method 'PUT where method _ = PUT
instance Method 'DELETE where method _ = DELETE
instance Method 'PATCH where method _ = PATCH
class Method (m :: StdMethod) where
    method :: Proxy m -> StdMethod

atMethod :: StdMethod -> Lens' PathItem (Maybe Operation)
atMethod = \case
    GET -> get
    POST -> post
    PUT -> put
    DELETE -> delete
    PATCH -> patch
    m -> error $ "atMethod: unsupported method: " <> show m

{-------------------------------------------------------------------------------
            Generating Golden Test Vectors For Address Encoding
-------------------------------------------------------------------------------}

-- SPENDINGKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
-- DELEGATIONKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
--
-- SPENDINGKEYBYTES=$(echo $SPENDINGKEY | jcli key to-bytes)
-- DELEGATIONKEYBYTES=$(echo $DELEGATIONKEY | jcli key to-bytes)
--
-- MAINNETSINGLE=$(jcli address single $SPENDINGKEY --prefix addr)
-- TESTNETSINGLE=$(jcli address single $SPENDINGKEY --testing --prefix addr)
--
-- MAINNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY --prefix addr)
-- TESTNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY --testing --prefix addr)
--
-- TESTVECTOR=test_vector_$(date +%s)
-- touch $TESTVECTOR
-- echo "spending key:        $SPENDINGKEYBYTES" >> $TESTVECTOR
-- echo "\ndelegation key:    $DELEGATIONKEYBYTES" >> $TESTVECTOR
-- echo "\nsingle (mainnet):  $MAINNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (mainnet): $MAINNETGROUPED" >> $TESTVECTOR
-- echo "\nsingle (testnet):  $TESTNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (testnet): $TESTNETGROUPED" >> $TESTVECTOR
--
-- echo -e $(cat $TESTVECTOR)
-- echo "Saved as $TESTVECTOR."
