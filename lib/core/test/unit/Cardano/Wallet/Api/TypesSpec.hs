{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Cardano.Pool.Metadata
    ( StakePoolMetadata (..), StakePoolTicker )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , ApiAccountPublicKey (..)
    , ApiAddress (..)
    , ApiBlockReference (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiByronWalletMigrationInfo (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionInput (..)
    , ApiEpochInfo (..)
    , ApiEpochNumber (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiNetworkTip (..)
    , ApiNtpStatus (..)
    , ApiSelectCoinsData (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiT (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiUtxoStatistics (..)
    , ApiWallet (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletPassphrase (..)
    , ByronWalletPostData (..)
    , ByronWalletStyle (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Iso8601Time (..)
    , NtpSyncingStatus (..)
    , PostExternalTransactionData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , SomeByronWalletPostData (..)
    , WalletBalance (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Gen
    ( genMnemonic, genPercentage, shrinkPercentage )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChainCode (..)
    , DelegationAddress (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PaymentAddress (..)
    , SomeMnemonic (..)
    , WalletKey (..)
    , XPub (..)
    , networkDiscriminantVal
    , passphraseMaxLength
    , passphraseMinLength
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ( genAddress, genLegacyAddress )
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
    , entropyToMnemonic
    , mkEntropy
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
    , PoolOwner (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotNo (..)
    , SortOrder (..)
    , StartTime (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , computeUtxoStatistics
    , log10
    , walletNameMaxLength
    , walletNameMinLength
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Control.Lens
    ( at, (.~), (^.) )
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
    ( isAlphaNum, toLower )
import Data.FileEmbed
    ( embedFile, makeRelativeToProject )
import Data.Function
    ( (&) )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Swagger
    ( Definitions
    , NamedSchema (..)
    , Referenced (..)
    , Schema
    , SwaggerType (..)
    , ToSchema (..)
    , enum_
    , properties
    , required
    , type_
    )
import Data.Swagger.Declare
    ( Declare )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime )
import Data.Typeable
    ( Typeable, splitTyConApp, tyConName, typeRep )
import Data.Word
    ( Word32, Word8 )
import Data.Word.Odd
    ( Word31 )
import GHC.TypeLits
    ( KnownSymbol, natVal, symbolVal )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>)
    , (:>)
    , Capture
    , Header'
    , JSON
    , PostNoContent
    , QueryParam
    , ReqBody
    , StdMethod (..)
    , Verb
    )
import Servant.API.Verbs
    ( NoContentVerb )
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
    ( Spec, SpecWith, describe, expectationFailure, it, runIO, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , arbitrarySizedBoundedIntegral
    , choose
    , elements
    , frequency
    , oneof
    , property
    , scale
    , shrinkIntegral
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Time
    ( genUniformTime )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Prelude

spec :: Spec
spec = do
    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            jsonRoundtripAndGolden $ Proxy @(ApiAddress 'Testnet)
            jsonRoundtripAndGolden $ Proxy @ApiEpochInfo
            jsonRoundtripAndGolden $ Proxy @(ApiSelectCoinsData 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiCoinSelection 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiCoinSelectionInput 'Testnet)
            jsonRoundtripAndGolden $ Proxy @ApiTimeReference
            jsonRoundtripAndGolden $ Proxy @ApiNetworkTip
            jsonRoundtripAndGolden $ Proxy @ApiBlockReference
            jsonRoundtripAndGolden $ Proxy @ApiNetworkInformation
            jsonRoundtripAndGolden $ Proxy @ApiNetworkParameters
            jsonRoundtripAndGolden $ Proxy @ApiNetworkClock
            jsonRoundtripAndGolden $ Proxy @ApiWalletDelegation
            jsonRoundtripAndGolden $ Proxy @ApiWalletDelegationStatus
            jsonRoundtripAndGolden $ Proxy @ApiWalletDelegationNext
            jsonRoundtripAndGolden $ Proxy @(ApiT (Hash "Genesis"))
            jsonRoundtripAndGolden $ Proxy @ApiStakePool
            jsonRoundtripAndGolden $ Proxy @(AddressAmount 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiTransaction 'Testnet)
            jsonRoundtripAndGolden $ Proxy @ApiWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWalletBalance
            jsonRoundtripAndGolden $ Proxy @ApiByronWalletMigrationInfo
            jsonRoundtripAndGolden $ Proxy @ApiWalletPassphrase
            jsonRoundtripAndGolden $ Proxy @ApiUtxoStatistics
            jsonRoundtripAndGolden $ Proxy @ApiFee
            jsonRoundtripAndGolden $ Proxy @ApiStakePoolMetrics
            jsonRoundtripAndGolden $ Proxy @ApiTxId
            jsonRoundtripAndGolden $ Proxy @(PostTransactionData 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(PostTransactionFeeData 'Testnet)
            jsonRoundtripAndGolden $ Proxy @WalletPostData
            jsonRoundtripAndGolden $ Proxy @AccountPostData
            jsonRoundtripAndGolden $ Proxy @WalletOrAccountPostData
            jsonRoundtripAndGolden $ Proxy @SomeByronWalletPostData
            jsonRoundtripAndGolden $ Proxy @WalletPutData
            jsonRoundtripAndGolden $ Proxy @WalletPutPassphraseData
            jsonRoundtripAndGolden $ Proxy @(ApiT (Hash "Tx"))
            jsonRoundtripAndGolden $ Proxy @(ApiT (Passphrase "encryption"))
            jsonRoundtripAndGolden $ Proxy @(ApiT Address, Proxy 'Testnet)
            jsonRoundtripAndGolden $ Proxy @(ApiT AddressPoolGap)
            jsonRoundtripAndGolden $ Proxy @(ApiT Direction)
            jsonRoundtripAndGolden $ Proxy @(ApiT TxStatus)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletBalance)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletId)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletName)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletPassphraseInfo)
            jsonRoundtripAndGolden $ Proxy @(ApiT SyncProgress)
            jsonRoundtripAndGolden $ Proxy @StakePoolMetadata

    describe "Textual encoding" $ do
        describe "Can perform roundtrip textual encoding & decoding" $ do
            textRoundtrip $ Proxy @Iso8601Time
            textRoundtrip $ Proxy @SortOrder
            textRoundtrip $ Proxy @ApiEpochNumber

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
        \has compatible ToJSON and ToSchema instances using validateToJSON." $ do
        validateEveryToJSON
            (Proxy :: Proxy (Api 'Testnet))
        -- NOTE See (ToSchema WalletOrAccountPostData)
        validateEveryToJSON
            (Proxy :: Proxy (
                ReqBody '[JSON] AccountPostData :> PostNoContent
              :<|>
                ReqBody '[JSON] WalletPostData  :> PostNoContent
            ))

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
            |] `shouldBe` (Left @String @(ApiMnemonicT '[12]) msg)

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
            let msg = "Error in $: parsing Integer failed, unexpected floating number\
                    \ 2.5"
            Aeson.parseEither parseJSON [aesonQQ|
                2.5
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT (Hash \"Tx\")" $ do
            let msg = "Error in $: Invalid tx hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
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
            let msg = "Error in $.amount.quantity: parsing Natural failed, \
                    \unexpected negative number -14"
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
            let msg = "Error in $: Invalid stake pool id: expecting a \
                      \hex-encoded value that is 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT PoolId" $ do
            let msg = "Error in $: Invalid stake pool id: expecting a \
                      \hex-encoded value that is 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "4c43d68b21921034519c36d2475f5adba989bb4465ec"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT (Hash \"Genesis\")" $ do
            let msg = "Error in $: Invalid genesis hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "-----"
            |] `shouldBe` (Left @String @(ApiT (Hash "Genesis")) msg)

        describe "StakePoolMetadata" $ do
            let msg = "Error in $.ticker: stake pool ticker length must be \
                      \3-5 characters"

            let testInvalidTicker :: Text -> SpecWith ()
                testInvalidTicker txt =
                    it ("Invalid ticker length: " ++ show (T.length txt)) $ do
                        Aeson.parseEither parseJSON [aesonQQ|
                            {
                                "owner": "ed25519_pk1afhcpw2tg7nr2m3wr4x8jaa4dv7d09gnv27kwfxpjyvukwxs8qdqwg85xp",
                                "homepage": "https://12345",
                                "ticker": #{txt},
                                "pledge_address": "ed25519_pk15vz9yc5c3upgze8tg5kd7kkzxqgqfxk5a3kudp22hdg0l2za00sq2ufkk7",
                                "name": "invalid"
                            }
                        |] `shouldBe` (Left @String @StakePoolMetadata msg)

            forM_ ["too long", "sh", ""] testInvalidTicker

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
        negativeTest proxy "ca1qv8qurswpc8qurswpc8qurs7xnyen"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("This type of address is not supported: [107].")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("This type of address is not supported: [107].")
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
        negativeTest proxy "ta1sv8qurswpc8qurswpc8qurs2l0ech"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("This type of address is not supported: [107].")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("This type of address is not supported: [107].")
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
        it "ApiEpochInfo" $ property $ \x ->
            let
                x' = ApiEpochInfo
                    { epochNumber = epochNumber (x :: ApiEpochInfo)
                    , epochStartTime = epochStartTime (x :: ApiEpochInfo)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiSelectCoinsData" $ property $ \x ->
            let
                x' = ApiSelectCoinsData
                    { payments = payments (x :: ApiSelectCoinsData 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiCoinSelection" $ property $ \x ->
            let
                x' = ApiCoinSelection
                    { inputs = inputs (x :: ApiCoinSelection 'Testnet)
                    , outputs = outputs (x :: ApiCoinSelection 'Testnet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiCoinSelectionInput" $ property $ \x ->
            let
                x' = ApiCoinSelectionInput
                    { id = id (x :: ApiCoinSelectionInput 'Testnet)
                    , index = index (x :: ApiCoinSelectionInput 'Testnet)
                    , address = address (x :: ApiCoinSelectionInput 'Testnet)
                    , amount = amount (x :: ApiCoinSelectionInput 'Testnet)
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
                    , nextEpoch = nextEpoch (x :: ApiNetworkInformation)
                    , nodeTip = nodeTip (x :: ApiNetworkInformation)
                    , networkTip = networkTip (x :: ApiNetworkInformation)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiNetworkClock" $ property $ \x ->
            let
                x' = ApiNetworkClock
                    { ntpStatus = ntpStatus (x :: ApiNetworkClock)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiNetworkParameters" $ property $ \x ->
            let
                x' = ApiNetworkParameters
                    { genesisBlockHash =
                            genesisBlockHash (x :: ApiNetworkParameters)
                    , blockchainStartTime =
                            blockchainStartTime (x :: ApiNetworkParameters)
                    , slotLength =
                            slotLength (x :: ApiNetworkParameters)
                    , epochLength =
                            epochLength (x :: ApiNetworkParameters)
                    , epochStability =
                            epochStability (x :: ApiNetworkParameters)
                    , activeSlotCoefficient =
                            activeSlotCoefficient (x :: ApiNetworkParameters)
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
            CustomDirectoryName ($(getTestData) </> "Cardano" </> "Wallet" </> "Api")
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

instance Arbitrary ApiEpochInfo where
    arbitrary = ApiEpochInfo <$> arbitrary <*> genUniformTime
    shrink _ = []

instance Arbitrary (ApiSelectCoinsData n) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ApiCoinSelection n) where
    arbitrary = ApiCoinSelection
        <$> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary (ApiCoinSelectionInput n) where
    arbitrary = ApiCoinSelectionInput
        <$> arbitrary
        <*> arbitrary
        <*> fmap (, Proxy @n) arbitrary
        <*> arbitrary
    shrink _ = []

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
            [ (10, genAddress @network)
            , (1, genLegacyAddress (30, 100))
            ]
        return (addr, proxy)

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    shrink (Quantity p) = Quantity <$> shrinkPercentage p
    arbitrary = Quantity <$> genPercentage

instance Arbitrary ApiWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWalletBalance where
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

instance Arbitrary WalletOrAccountPostData where
    arbitrary = do
        let walletPostDataGen = arbitrary :: Gen WalletPostData
        let accountPostDataGen = arbitrary :: Gen AccountPostData
        oneof [ WalletOrAccountPostData . Left <$> walletPostDataGen
              , WalletOrAccountPostData . Right <$> accountPostDataGen ]

instance Arbitrary AccountPostData where
    arbitrary = do
        wName <- ApiT <$> arbitrary
        seed <- SomeMnemonic <$> genMnemonic @15
        let rootXPrv = generateKeyFromSeed (seed, Nothing) mempty
        let accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv minBound
        pure $ AccountPostData wName (ApiAccountPublicKey $ ApiT accXPub) Nothing

instance Arbitrary WalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SomeByronWalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[12]) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[15]) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[12,15,18,21,24]) where
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

instance Arbitrary WalletDelegationStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletDelegationStatus where
    arbitrary = genericArbitrary

instance Arbitrary ApiWalletDelegationNext where
    arbitrary = oneof
        [ ApiWalletDelegationNext Api.Delegating
            <$> fmap Just arbitrary
            <*> fmap Just arbitrary
        , ApiWalletDelegationNext Api.NotDelegating
            <$> pure Nothing
            <*> fmap Just arbitrary
        ]

instance Arbitrary ApiWalletDelegation where
    arbitrary = ApiWalletDelegation
        <$> fmap (\x -> x { changesAt = Nothing }) arbitrary
        <*> oneof [ vectorOf i arbitrary | i <- [0..2 ] ]

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
        <*> choose (0.0, 5.0)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> choose (0.0, 100.0)
        <*> choose (0.0, 2.0)

instance Arbitrary StakePoolMetadata where
    arbitrary = StakePoolMetadata
        <$> arbitrary
        <*> arbitrary
        <*> arbitraryText 50
        <*> arbitraryMaybeText 255
        <*> arbitraryText 100
        <*> arbitraryText 50
      where
        arbitraryText maxLen = do
            len <- choose (1, maxLen)
            T.pack <$> vectorOf len arbitrary
        arbitraryMaybeText maxLen = frequency
            [ (9, Just <$> arbitraryText maxLen)
            , (1, pure Nothing) ]

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . BS.pack <$> vectorOf 32 arbitrary

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
    => Arbitrary (ApiMnemonicT (mw ': '[]))
  where
    arbitrary = do
        ent <- arbitrary @(Entropy n)
        return
            . ApiMnemonicT
            . SomeMnemonic
            $ entropyToMnemonic ent

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , Arbitrary (ApiMnemonicT rest)
    )
    => Arbitrary (ApiMnemonicT (mw ': rest))
  where
    arbitrary = do
        ApiMnemonicT x <- arbitrary @(ApiMnemonicT '[mw])
        ApiMnemonicT y <- arbitrary @(ApiMnemonicT rest)
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

instance Arbitrary ApiNtpStatus where
    arbitrary = do
        o <- Quantity <$> (arbitrary @Integer)
        elements
            [ ApiNtpStatus NtpSyncingStatusUnavailable Nothing
            , ApiNtpStatus NtpSyncingStatusPending Nothing
            , ApiNtpStatus NtpSyncingStatusAvailable (Just o)
            ]

instance Arbitrary ApiNetworkClock where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)

instance Arbitrary (Quantity "slot" Word32) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary StartTime where
    arbitrary = StartTime <$> genUniformTime

instance Arbitrary (Quantity "second" NominalDiffTime) where
    shrink (Quantity 0.0) = []
    shrink _ = [Quantity 0.0]
    arbitrary = Quantity . fromInteger <$> choose (0, 10000)

instance Arbitrary (Quantity "percent" Double) where
    shrink (Quantity 0.0) = []
    shrink _ = [Quantity 0.0]
    arbitrary = Quantity <$> choose (0,100)

instance Arbitrary ApiNetworkParameters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiEpochNumber where
    arbitrary = do
        let lowerBound = fromIntegral (minBound @Word31)
        let upperBound = fromIntegral (maxBound @Word31)
        epochN <- choose (lowerBound :: Int, upperBound)
        elements
            [ ApiEpochNumberLatest
            , ApiEpochNumber (EpochNo (fromIntegral epochN))
            ]

instance Arbitrary SlotId where
    arbitrary = SlotId <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

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
            <*> Test.QuickCheck.scale (`mod` 3) arbitrary
            <*> Test.QuickCheck.scale (`mod` 3) arbitrary
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
specification :: Aeson.Value
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

instance ToSchema (ApiSelectCoinsData n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSelectCoinsData"

instance ToSchema (ApiCoinSelection n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiCoinSelection"

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

instance ToSchema AccountPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAccountPostData"

instance ToSchema WalletOrAccountPostData where
    declareNamedSchema _ = do
        -- NOTE
        -- 'WalletOrAccountPostData' makes use of the 'oneOf' operator from
        -- Swagger 3.0.0. We don't have that in Swagger 2.0 so we kinda have to
        -- "fake it".
        -- Therefore, we validate that any JSON matches with the union of the
        -- schema, modulo the "required" properties that have to be different,
        -- and we also validate both 'WalletPostData' and 'AccountPostData'
        -- independently with 'validateEveryToJSON'.
        NamedSchema _ accountPostData <- declareNamedSchema (Proxy @AccountPostData)
        NamedSchema _ walletPostData  <- declareNamedSchema (Proxy @WalletPostData)
        pure $ NamedSchema Nothing $ mempty
            & type_ .~ Just SwaggerObject
            & required .~ ["name"]
            & properties .~ mconcat
                [ accountPostData ^. properties
                , walletPostData ^. properties
                ]

instance ToSchema (ByronWalletPostData '[12]) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletRandomPostData"

instance ToSchema (ByronWalletPostData '[15]) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletIcarusPostData"

instance ToSchema (ByronWalletPostData '[12,15,18,21,24]) where
    -- NOTE ApiByronWalletLedgerPostData works too. Only the description differs.
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletTrezorPostData"

instance ToSchema SomeByronWalletPostData where
    declareNamedSchema _ = do
        NamedSchema _ schema <- declareNamedSchema (Proxy @(ByronWalletPostData '[12,15,18,21,24]))
        let props = schema ^. properties
        pure $ NamedSchema Nothing $ schema
            & properties .~ (props & at "style" .~ Just (Inline styleSchema))
      where
        styleSchema = mempty
            & type_ .~ Just SwaggerString
            & enum_ .~ Just (toJSON . toText <$> [Random, Icarus, Trezor, Ledger])

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

instance ToSchema ApiNetworkClock where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkClock"

instance ToSchema ApiNetworkParameters where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkParameters"

instance ToSchema ApiNetworkTip where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkTip"

instance ToSchema ApiWalletDelegationStatus where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegationStatus"

instance ToSchema ApiWalletDelegationNext where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegationNext"

instance ToSchema ApiWalletDelegation where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegation"

-- | Utility function to provide an ad-hoc 'ToSchema' instance for a definition:
-- we simply look it up within the Swagger specification.
declareSchemaForDefinition :: Text -> Declare (Definitions Schema) NamedSchema
declareSchemaForDefinition ref = do
    let json = foldl' unsafeLookupKey specification ["components","schemas",ref]
    case Aeson.eitherDecode' (Aeson.encode json) of
        Left err -> error $
            "unable to decode schema for definition '" <> T.unpack ref <> "': " <> show err
        Right schema ->
            return $ NamedSchema (Just ref) schema

unsafeLookupKey :: Aeson.Value -> Text -> Aeson.Value
unsafeLookupKey json k = case json of
    Aeson.Object m -> fromMaybe bombMissing (HM.lookup k m)
    m -> bombNotObject m
  where
    bombNotObject m =
        error $ "given JSON value is NOT an object: " <> show m
    bombMissing =
        error $ "no value found in map for key: " <> T.unpack k

-- | Verify that all servant endpoints are present and match the specification
class ValidateEveryPath api where
    validateEveryPath :: Proxy api -> Spec

instance {-# OVERLAPS #-} HasPath a => ValidateEveryPath a where
    validateEveryPath proxy = do
        let (verb, path) = getPath proxy
        let verbStr = toLower <$> show verb
        it (verbStr <> " " <> path <> " exists in specification") $ do
            case foldl' unsafeLookupKey specification ["paths", T.pack path] of
                Aeson.Object m -> case HM.lookup (T.pack verbStr) m of
                    Just{}  -> return @IO ()
                    Nothing -> fail "couldn't find path in specification"
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

instance (Method m) => HasPath (NoContentVerb m) where
    getPath _ = (method (Proxy @m), "")

instance (KnownSymbol path, HasPath sub) => HasPath (path :> sub) where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in (verb, "/" <> symbolVal (Proxy :: Proxy path) <> sub)

instance (KnownSymbol param, HasPath sub) => HasPath (Capture param t :> sub)
  where
    getPath _ =
        let (verb, sub) = getPath (Proxy @sub)
        in case symbolVal (Proxy :: Proxy param) of
            sym | sym == "*" -> (verb, "/" <> sym <> sub)
            sym -> (verb, "/{" <> sym <> "}" <> sub)

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
