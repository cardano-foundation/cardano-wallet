{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
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

import Cardano.Mnemonic
    ( CheckSumBits
    , ConsistentEntropy
    , Entropy
    , EntropySize
    , MnemonicException (..)
    , SomeMnemonic (..)
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToMnemonic
    , mkEntropy
    )
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
    , ApiCoinSelection (..)
    , ApiCoinSelectionInput (..)
    , ApiEpochInfo (..)
    , ApiFee (..)
    , ApiMnemonicT (..)
    , ApiNetworkClock (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiNetworkTip (..)
    , ApiNtpStatus (..)
    , ApiPostRandomAddressData
    , ApiPutAddressesData (..)
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
    , ApiWalletDiscovery (..)
    , ApiWalletMigrationInfo (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalPostData (..)
    , ByronWalletFromXPrvPostData (..)
    , ByronWalletPostData (..)
    , ByronWalletPutPassphraseData (..)
    , ByronWalletStyle (..)
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
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
    ( HardDerivation (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , WalletKey (..)
    , passphraseMaxLength
    , passphraseMinLength
    )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , ChimericAccount (..)
    , Coin (..)
    , Direction (..)
    , EpochNo (..)
    , Hash (..)
    , HistogramBar (..)
    , PoolId (..)
    , PoolOwner (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SortOrder (..)
    , StakePoolMetadata (..)
    , StakePoolTicker
    , StartTime (..)
    , TxIn (..)
    , TxIn (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , UTxOStatistics (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletName (..)
    , computeUtxoStatistics
    , log10
    , walletNameMaxLength
    , walletNameMinLength
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText, unsafeXPrv )
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
import Data.Char
    ( toLower )
import Data.FileEmbed
    ( embedFile, makeRelativeToProject )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
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
    , QueryFlag
    , QueryParam
    , ReqBody
    , StdMethod (..)
    , Verb
    )
import Servant.API.Stream
    ( Stream )
import Servant.API.Verbs
    ( NoContentVerb )
import Servant.Swagger.Test
    ( validateEveryToJSON )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , applyArbitrary2
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
    , vector
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Roundtrip
    ( httpApiDataRoundtrip )
import Test.Utils.Time
    ( genUniformTime )
import Web.HttpApiData
    ( FromHttpApiData (..) )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Prelude
import qualified Test.Utils.Roundtrip as Utils

spec :: Spec
spec = do
    let jsonRoundtripAndGolden = Utils.jsonRoundtripAndGolden
            ($(getTestData) </> "Cardano" </> "Wallet" </> "Api")

    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            jsonRoundtripAndGolden $ Proxy @ApiEpochInfo
            jsonRoundtripAndGolden $ Proxy @(ApiSelectCoinsData ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @(ApiCoinSelection ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @(ApiCoinSelectionInput ('Testnet 0))
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
            jsonRoundtripAndGolden $ Proxy @ApiStakePoolMetrics
            jsonRoundtripAndGolden $ Proxy @(AddressAmount (ApiT Address, Proxy ('Testnet 0)))
            jsonRoundtripAndGolden $ Proxy @(ApiTransaction ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @(ApiPutAddressesData ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @ApiWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWallet
            jsonRoundtripAndGolden $ Proxy @ApiByronWalletBalance
            jsonRoundtripAndGolden $ Proxy @ApiWalletMigrationInfo
            jsonRoundtripAndGolden $ Proxy @(ApiWalletMigrationPostData ('Testnet 0) "lenient")
            jsonRoundtripAndGolden $ Proxy @(ApiWalletMigrationPostData ('Testnet 0) "raw")
            jsonRoundtripAndGolden $ Proxy @ApiWalletPassphrase
            jsonRoundtripAndGolden $ Proxy @ApiUtxoStatistics
            jsonRoundtripAndGolden $ Proxy @ApiFee
            jsonRoundtripAndGolden $ Proxy @ApiStakePoolMetrics
            jsonRoundtripAndGolden $ Proxy @ApiTxId
            jsonRoundtripAndGolden $ Proxy @(PostTransactionData ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @(PostTransactionFeeData ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @WalletPostData
            jsonRoundtripAndGolden $ Proxy @AccountPostData
            jsonRoundtripAndGolden $ Proxy @WalletOrAccountPostData
            jsonRoundtripAndGolden $ Proxy @SomeByronWalletPostData
            jsonRoundtripAndGolden $ Proxy @ByronWalletFromXPrvPostData
            jsonRoundtripAndGolden $ Proxy @WalletPutData
            jsonRoundtripAndGolden $ Proxy @WalletPutPassphraseData
            jsonRoundtripAndGolden $ Proxy @ByronWalletPutPassphraseData
            jsonRoundtripAndGolden $ Proxy @(ApiT (Hash "Tx"))
            jsonRoundtripAndGolden $ Proxy @(ApiT (Passphrase "raw"))
            jsonRoundtripAndGolden $ Proxy @(ApiT (Passphrase "lenient"))
            jsonRoundtripAndGolden $ Proxy @(ApiT Address, Proxy ('Testnet 0))
            jsonRoundtripAndGolden $ Proxy @(ApiT AddressPoolGap)
            jsonRoundtripAndGolden $ Proxy @(ApiT Direction)
            jsonRoundtripAndGolden $ Proxy @(ApiT TxStatus)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletBalance)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletId)
            jsonRoundtripAndGolden $ Proxy @(ApiT WalletName)
            jsonRoundtripAndGolden $ Proxy @ApiWalletPassphraseInfo
            jsonRoundtripAndGolden $ Proxy @(ApiT SyncProgress)
            jsonRoundtripAndGolden $ Proxy @(ApiT StakePoolMetadata)
            jsonRoundtripAndGolden $ Proxy @ApiPostRandomAddressData

    describe "Textual encoding" $ do
        describe "Can perform roundtrip textual encoding & decoding" $ do
            textRoundtrip $ Proxy @Iso8601Time
            textRoundtrip $ Proxy @SortOrder
            textRoundtrip $ Proxy @Coin

    describe "AddressAmount" $ do
        it "fromText \"22323\"" $
            let err =
                    "Parse error. " <>
                    "Expecting format \"<amount>@<address>\" but got \"22323\""
            in
                fromText @(AddressAmount Text) "22323"
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
            (Proxy :: Proxy (Api ('Testnet 0) ApiStakePool))
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
        validateEveryPath (Proxy :: Proxy (Api ('Testnet 0) ApiStakePool))

    describe "verify JSON parsing failures too" $ do
        it "ApiT (Passphrase \"raw\") (too short)" $ do
            let minLength = passphraseMinLength (Proxy :: Proxy "raw")
            let msg = "Error in $: passphrase is too short: \
                    \expected at least " <> show minLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|"patate"|]
                `shouldBe` (Left @String @(ApiT (Passphrase "raw")) msg)

        it "ApiT (Passphrase \"raw\") (too long)" $ do
            let maxLength = passphraseMaxLength (Proxy :: Proxy "raw")
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show maxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*maxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "raw")) msg)

        it "ApiT (Passphrase \"lenient\") (too long)" $ do
            let maxLength = passphraseMaxLength (Proxy :: Proxy "lenient")
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show maxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*maxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "lenient")) msg)

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
                { "address": "<addr>"
                , "amount": {"unit":"lovelace","quantity":-14}
                }
            |] `shouldBe` (Left @String @(AddressAmount (ApiT Address, Proxy ('Testnet 0))) msg)

        it "AddressAmount (too big)" $ do
            let msg = "Error in $: invalid coin value: value has to be lower \
                    \than or equal to " <> show (getCoin maxBound)
                    <> " lovelace."
            Aeson.parseEither parseJSON [aesonQQ|
                { "address": "<addr>"
                , "amount":
                    { "unit":"lovelace"
                    ,"quantity":#{getCoin maxBound + 1}
                    }
                }
            |] `shouldBe` (Left @String @(AddressAmount (ApiT Address, Proxy ('Testnet 0))) msg)

        it "ApiT PoolId" $ do
            let msg = "Error in $: Invalid stake pool id: expecting a \
                      \hex-encoded value that is 28 or 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT PoolId" $ do
            let msg = "Error in $: Invalid stake pool id: expecting a \
                      \hex-encoded value that is 28 or 32 bytes in length."
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
                        |] `shouldBe` (Left @String @(ApiT StakePoolMetadata) msg)

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

    describe "pointless tests to trigger coverage for record accessors" $ do
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
                    { payments = payments (x :: ApiSelectCoinsData ('Testnet 0))
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiCoinSelection" $ property $ \x ->
            let
                x' = ApiCoinSelection
                    { inputs = inputs (x :: ApiCoinSelection ('Testnet 0))
                    , outputs = outputs (x :: ApiCoinSelection ('Testnet 0))
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiCoinSelectionInput" $ property $ \x ->
            let
                x' = ApiCoinSelectionInput
                    { id = id (x :: ApiCoinSelectionInput ('Testnet 0))
                    , index = index (x :: ApiCoinSelectionInput ('Testnet 0))
                    , address = address (x :: ApiCoinSelectionInput ('Testnet 0))
                    , amount = amount (x :: ApiCoinSelectionInput ('Testnet 0))
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
                    , discovery = discovery (x :: ApiByronWallet)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiWalletMigrationInfo" $ property $ \x ->
            let
                x' = ApiWalletMigrationInfo
                    { migrationCost =
                        migrationCost (x :: ApiWalletMigrationInfo)
                    , leftovers =
                        leftovers (x :: ApiWalletMigrationInfo)
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiWalletMigrationPostData lenient" $ property $ \x ->
            let
                x' = ApiWalletMigrationPostData
                    { passphrase =
                        passphrase (x :: ApiWalletMigrationPostData ('Testnet 0) "lenient")
                    , addresses =
                        addresses (x :: ApiWalletMigrationPostData ('Testnet 0) "lenient")
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiWalletMigrationPostData raw" $ property $ \x ->
            let
                x' = ApiWalletMigrationPostData
                    { passphrase =
                        passphrase (x :: ApiWalletMigrationPostData ('Testnet 0) "raw")
                    , addresses =
                        addresses (x :: ApiWalletMigrationPostData ('Testnet 0) "raw")
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
                    { estimatedMin = estimatedMin (x :: ApiFee)
                    , estimatedMax = estimatedMax (x :: ApiFee)
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
        it "ByronWalletPutPassphraseData" $ property $ \x ->
            let
                x' = ByronWalletPutPassphraseData
                    { oldPassphrase = oldPassphrase (x :: ByronWalletPutPassphraseData)
                    , newPassphrase = newPassphrase (x :: ByronWalletPutPassphraseData)
                    }
            in
                x' === x .&&. show x' === show x
        it "PostTransactionData" $ property $ \x ->
            let
                x' = PostTransactionData
                    { payments = payments (x :: PostTransactionData ('Testnet 0))
                    , passphrase = passphrase (x :: PostTransactionData ('Testnet 0))
                    , withdrawal = withdrawal (x :: PostTransactionData ('Testnet 0))
                    }
            in
                x' === x .&&. show x' === show x
        it "PostTransactionFeeData" $ property $ \x ->
            let
                x' = PostTransactionFeeData
                    { payments = payments (x :: PostTransactionFeeData ('Testnet 0))
                    , withdrawal = withdrawal (x :: PostTransactionFeeData ('Testnet 0))
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
                    { id = id (x :: ApiTransaction ('Testnet 0))
                    , amount = amount (x :: ApiTransaction ('Testnet 0))
                    , insertedAt = insertedAt (x :: ApiTransaction ('Testnet 0))
                    , pendingSince = pendingSince (x :: ApiTransaction ('Testnet 0))
                    , depth = depth (x :: ApiTransaction ('Testnet 0))
                    , direction = direction (x :: ApiTransaction ('Testnet 0))
                    , inputs = inputs (x :: ApiTransaction ('Testnet 0))
                    , outputs = outputs (x :: ApiTransaction ('Testnet 0))
                    , status = status (x :: ApiTransaction ('Testnet 0))
                    , withdrawals = withdrawals (x :: ApiTransaction ('Testnet 0))
                    }
            in
                x' === x .&&. show x' === show x
        it "ApiPutAddressesData" $ property $ \x ->
            let
                x' = ApiPutAddressesData
                    { addresses = addresses (x :: ApiPutAddressesData ('Testnet 0))
                    }
            in
                x' === x .&&. show x' === show x
        it "AddressAmount" $ property $ \x ->
            let
                x' = AddressAmount
                    { address = address (x :: AddressAmount (ApiT Address, Proxy ('Testnet 0)))
                    , amount = amount (x :: AddressAmount (ApiT Address, Proxy ('Testnet 0)))
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
            let x' = ApiNetworkParameters
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
                    , decentralizationLevel =
                        decentralizationLevel (x :: ApiNetworkParameters)
                    , desiredPoolNumber =
                        desiredPoolNumber (x :: ApiNetworkParameters)
                    , minimumUtxoValue =
                        minimumUtxoValue (x :: ApiNetworkParameters)
                    , hardforkAt =
                        hardforkAt (x :: ApiNetworkParameters)
                    }
            in
            x' === x .&&. show x' === show x

{-------------------------------------------------------------------------------
                              Address Encoding
-------------------------------------------------------------------------------}

-- Dummy instances
instance EncodeAddress ('Testnet 0) where
    encodeAddress = const "<addr>"

instance DecodeAddress ('Testnet 0) where
    decodeAddress "<addr>" = Right $ Address "<addr>"
    decodeAddress _ = Left $ TextDecodingError "invalid address"

-- Dummy instances
instance EncodeStakeAddress ('Testnet 0) where
    encodeStakeAddress = const "<stake-addr>"

instance DecodeStakeAddress ('Testnet 0) where
    decodeStakeAddress "<stake-addr>" = Right $ ChimericAccount "<stake-addr>"
    decodeStakeAddress _ = Left $ TextDecodingError "invalid stake address"


{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Proxy (n :: NetworkDiscriminant)) where
    shrink _ = []
    arbitrary = pure (Proxy @n)

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
    arbitrary = applyArbitrary2 ApiCoinSelection
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
    arbitrary = pure $ Address "<addr>"

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

instance Arbitrary ApiWalletDiscovery where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletMigrationInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Passphrase purpose) =>
         Arbitrary (ApiWalletMigrationPostData n purpose) where
    arbitrary = do
        n <- choose (1,255)
        pwd <- arbitrary
        addr <- vector n
        pure $ ApiWalletMigrationPostData pwd ((, Proxy @n) <$> addr)

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
        pure $ AccountPostData wName (ApiAccountPublicKey $ ApiT $ getKey accXPub) Nothing

instance Arbitrary WalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ByronWalletFromXPrvPostData where
    arbitrary = do
        n <- arbitrary
        rootXPrv <- ApiT . unsafeXPrv . BS.pack <$> vector 128
        bytesNumber <- choose (64,100)
        h <- ApiT . Hash . B8.pack <$> replicateM bytesNumber arbitrary
        pure $ ByronWalletFromXPrvPostData n rootXPrv h

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

instance Arbitrary ByronWalletPutPassphraseData where
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
            Nothing . Just <$> arbitrary
        ]

instance Arbitrary (Passphrase "lenient") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "lenient"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "lenient"

instance Arbitrary ApiWalletDelegation where
    arbitrary = ApiWalletDelegation
        <$> fmap (\x -> x { changesAt = Nothing }) arbitrary
        <*> oneof [ vector i | i <- [0..2 ] ]

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 28 bytes

instance Arbitrary ApiStakePool where
    arbitrary = ApiStakePool
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary ApiStakePoolMetrics where
    arbitrary = ApiStakePoolMetrics
        <$> (Quantity . fromIntegral <$> choose (1::Integer, 1_000_000_000_000))
        <*> arbitrary
        <*> (choose (0.0, 5.0))
        <*> (Quantity . fromIntegral <$> choose (1::Integer, 22_600_000))

instance Arbitrary StakePoolMetadata where
    arbitrary = StakePoolMetadata
        <$> arbitrary
        <*> arbitraryText 50
        <*> arbitraryMaybeText 255
        <*> arbitraryText 100
      where
        arbitraryText maxLen = do
            len <- choose (1, maxLen)
            T.pack <$> vector len
        arbitraryMaybeText maxLen = frequency
            [ (9, Just <$> arbitraryText maxLen)
            , (1, pure Nothing) ]

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . BS.pack <$> vector 32

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

instance Arbitrary ApiWalletPassphraseInfo where
    arbitrary = ApiWalletPassphraseInfo <$> genUniformTime

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
                mkEntropy  @n . BA.convert . B8.pack <$> vector (size `quot` 8)
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
    arbitrary = Quantity . fromInteger <$> choose (0, 10_000)

instance Arbitrary (Quantity "percent" Double) where
    shrink (Quantity 0.0) = []
    shrink _ = [Quantity 0.0]
    arbitrary = Quantity <$> choose (0,100)

instance Arbitrary ApiNetworkParameters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = applyArbitrary2 SlotId
    shrink = genericShrink

instance Arbitrary SlotInEpoch where
    shrink (SlotInEpoch x) = SlotInEpoch <$> shrink x
    arbitrary = SlotInEpoch <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary a => Arbitrary (AddressAmount a) where
    arbitrary = applyArbitrary2 AddressAmount
    shrink _ = []

instance Arbitrary (PostTransactionData t) where
    arbitrary = PostTransactionData
        <$> arbitrary
        <*> arbitrary
        <*> elements [Just SelfWithdrawal, Nothing]

instance Arbitrary ApiWithdrawalPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ApiPutAddressesData t) where
    arbitrary = do
        n <- choose (1,255)
        addrs <- vector n
        pure $ ApiPutAddressesData ((, Proxy @t) <$> addrs)

instance Arbitrary (PostTransactionFeeData t) where
    arbitrary = PostTransactionFeeData
        <$> arbitrary
        <*> elements [Just SelfWithdrawal, Nothing]

instance Arbitrary PostExternalTransactionData where
    arbitrary = do
        count <- choose (0, 32)
        bytes <- BS.pack <$> replicateM count arbitrary
        return $ PostExternalTransactionData bytes
    shrink (PostExternalTransactionData bytes) =
        PostExternalTransactionData . BS.pack <$> shrink (BS.unpack bytes)

instance Arbitrary (ApiTransaction t) where
    shrink = filter outputsNonEmpty . genericShrink
      where
        outputsNonEmpty :: ApiTransaction t -> Bool
        outputsNonEmpty = (not . null) . view #outputs
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
            <*> genInputs
            <*> genOutputs
            <*> genWithdrawals
            <*> pure txStatus
      where
        genInputs =
            Test.QuickCheck.scale (`mod` 3) arbitrary
        -- Note that the generated list of outputs must be non-empty in order
        -- to be consistent with the specification.
        --
        -- Ideally, we should encode this restriction in the type system.
        --
        -- See https://jira.iohk.io/browse/ADP-400.
        genOutputs = (:)
            <$> arbitrary
            <*> Test.QuickCheck.scale (`mod` 3) arbitrary
        genWithdrawals =
            Test.QuickCheck.scale (`mod` 3) arbitrary

instance Arbitrary (ApiWithdrawal (t :: NetworkDiscriminant)) where
    arbitrary = ApiWithdrawal
        <$> fmap (, Proxy @t) arbitrary
        <*> arbitrary

instance Arbitrary ChimericAccount where
    arbitrary = ChimericAccount . BS.pack <$> vector 28

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 1_000_000_000_000_000)

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList utxo

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = applyArbitrary2 TxOut

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
    arbitrary = applyArbitrary2 ApiTxInput

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
    arbitrary = fmap (Quantity . fromIntegral) (arbitrary @Word32)

instance Arbitrary ApiPostRandomAddressData where
    arbitrary = genericArbitrary
    shrink = genericShrink

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

instance ToSchema (ApiPutAddressesData t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPutAddressesData"

instance ToSchema (ApiSelectCoinsData n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSelectCoinsData"

instance ToSchema (ApiCoinSelection n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiCoinSelection"

instance ToSchema ApiWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWallet"

instance ToSchema ApiByronWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWallet"

instance ToSchema ApiWalletMigrationInfo where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletMigrationInfo"

instance ToSchema (ApiWalletMigrationPostData t "lenient") where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiByronWalletMigrationPostData"

instance ToSchema (ApiWalletMigrationPostData t "raw") where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiShelleyWalletMigrationPostData"

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

instance ToSchema ByronWalletFromXPrvPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletRandomXPrvPostData"

instance ToSchema SomeByronWalletPostData where
    declareNamedSchema _ = do
        NamedSchema _ schema1 <- declareNamedSchema (Proxy @(ByronWalletPostData '[12,15,18,21,24]))
        let props1 = schema1 ^. properties
        NamedSchema _ schema2 <- declareNamedSchema (Proxy @ByronWalletFromXPrvPostData)
        let props2 = schema2 ^. properties
        NamedSchema _ accountPostData <- declareNamedSchema (Proxy @AccountPostData)
        pure $ NamedSchema Nothing $ mempty
            & properties .~ mconcat
            [ props1 & at "style" .~ Just (Inline styleSchema)
            , props2 & at "style" .~ Just (Inline styleSchema)
            , accountPostData ^. properties
            ]
      where
        styleSchema = mempty
            & type_ .~ Just SwaggerString
            & enum_ .~ Just (toJSON . toText <$> [Random, Icarus, Trezor, Ledger])

instance ToSchema WalletPutData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletPutData"

instance ToSchema WalletPutPassphraseData where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletPutPassphraseData"

instance ToSchema ByronWalletPutPassphraseData where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiByronWalletPutPassphraseData"

instance ToSchema (PostTransactionData t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostTransactionData"

instance ToSchema (PostTransactionFeeData t) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostTransactionFeeData"

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

instance ToSchema ApiPostRandomAddressData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostRandomAddressData"

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

instance (Method m) => HasPath (Stream m s f ct a) where
    getPath _ = (method (Proxy @m), "")

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

instance HasPath sub => HasPath (QueryFlag sym :> sub) where
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
