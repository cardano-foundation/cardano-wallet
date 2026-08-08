{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.DRep.MetadataSpec
    ( spec
    ) where

import Cardano.Wallet.DRep.Metadata
    ( FetchError (..)
    , defaultIpfsGatewayUrl
    , maxMetadataBytes
    , parseCip0119
    , readLimited
    , resolveUrl
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepMetaReference (..)
    , DRepMetadata (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Aeson.Types
    ( parseEither
    )
import Data.IORef
    ( atomicModifyIORef
    , newIORef
    )
import Data.List
    ( isInfixOf
    , isPrefixOf
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Cardano.Wallet.DRep.Metadata" $ do
    describe "resolveUrl" $ do
        it "passes https:// URLs through unchanged" $ do
            result <-
                runExceptT
                    $ resolveUrl
                        defaultIpfsGatewayUrl
                        "https://example.com/drep.jsonld"
            case result of
                Left e -> fail $ "Expected Right, got: " <> show e
                Right _ -> pure ()

        it "rewrites ipfs:// to the default Blockfrost gateway" $ do
            result <-
                runExceptT
                    $ resolveUrl
                        defaultIpfsGatewayUrl
                        "ipfs://QmTestCid123"
            case result of
                Left e -> fail $ "Expected Right, got: " <> show e
                Right uri ->
                    show uri
                        `shouldSatisfy` isPrefixOf
                            "https://ipfs.blockfrost.dev/ipfs/QmTestCid123"

        it "rewrites ipfs:// to a custom gateway" $ do
            result <-
                runExceptT
                    $ resolveUrl
                        "https://my-gateway.example.com/ipfs/"
                        "ipfs://QmTestCid123"
            case result of
                Left e -> fail $ "Expected Right, got: " <> show e
                Right uri ->
                    show uri
                        `shouldSatisfy` isPrefixOf
                            "https://my-gateway.example.com/ipfs/QmTestCid123"

        it "rejects strings that are not valid URIs" $ do
            result <-
                runExceptT
                    $ resolveUrl defaultIpfsGatewayUrl ":::not-a-uri:::"
            result `shouldBe` Left (FetchInvalidUri ":::not-a-uri:::")

    describe "parseCip0119" $ do
        it "parses a flat-layout document" $ do
            let val = Aeson.object ["givenName" Aeson..= ("Alice" :: String)]
            parseEither parseCip0119 val `shouldBe` Right (minimalMeta "Alice")

        it "parses a nested body-layout document" $ do
            let body = Aeson.object ["givenName" Aeson..= ("Bob" :: String)]
            let val = Aeson.object ["body" Aeson..= body]
            parseEither parseCip0119 val `shouldBe` Right (minimalMeta "Bob")

        it "fails when givenName is absent" $ do
            let val = Aeson.object []
            parseEither parseCip0119 val `shouldSatisfy` isLeft

        it "parses all optional fields when present" $ do
            let val =
                    Aeson.object
                        [ "givenName" Aeson..= ("Carol" :: String)
                        , "objectives" Aeson..= ("Promote decentralisation" :: String)
                        , "motivations" Aeson..= ("Long-time community member" :: String)
                        , "qualifications" Aeson..= ("10 years in DLT" :: String)
                        , "paymentAddress" Aeson..= ("addr1..." :: String)
                        , "doNotList" Aeson..= False
                        ]
            parseEither parseCip0119 val
                `shouldBe` Right
                    DRepMetadata
                        { drepMetaName = "Carol"
                        , drepMetaObjectives = Just "Promote decentralisation"
                        , drepMetaMotivations = Just "Long-time community member"
                        , drepMetaQualifications = Just "10 years in DLT"
                        , drepMetaPaymentAddress = Just "addr1..."
                        , drepMetaDoNotList = False
                        , drepMetaReferences = []
                        }

        it "defaults doNotList to false when absent" $ do
            let val = Aeson.object ["givenName" Aeson..= ("Dave" :: String)]
            case parseEither parseCip0119 val of
                Left e -> fail e
                Right m -> drepMetaDoNotList m `shouldBe` False

        it "reads doNotList: true correctly" $ do
            let val =
                    Aeson.object
                        [ "givenName" Aeson..= ("Eve" :: String)
                        , "doNotList" Aeson..= True
                        ]
            case parseEither parseCip0119 val of
                Left e -> fail e
                Right m -> drepMetaDoNotList m `shouldBe` True

        it "parses a references list" $ do
            let ref =
                    Aeson.object
                        [ "label" Aeson..= ("Website" :: String)
                        , "uri" Aeson..= ("https://eve.example.com" :: String)
                        ]
            let val =
                    Aeson.object
                        [ "givenName" Aeson..= ("Eve" :: String)
                        , "references" Aeson..= [ref]
                        ]
            case parseEither parseCip0119 val of
                Left e -> fail e
                Right m ->
                    drepMetaReferences m
                        `shouldBe` [ DRepMetaReference
                                        { drepMetaRefLabel = "Website"
                                        , drepMetaRefUri = "https://eve.example.com"
                                        }
                                   ]

        it "defaults references to empty list when absent" $ do
            let val = Aeson.object ["givenName" Aeson..= ("Frank" :: String)]
            case parseEither parseCip0119 val of
                Left e -> fail e
                Right m -> drepMetaReferences m `shouldBe` []

        it
            "reads references from top level in nested-body layout (CIP-0119 canonical)"
            $ do
                let ref =
                        Aeson.object
                            [ "label" Aeson..= ("Website" :: String)
                            , "uri" Aeson..= ("https://grace.example.com" :: String)
                            ]
                let body = Aeson.object ["givenName" Aeson..= ("Grace" :: String)]
                let val =
                        Aeson.object
                            [ "body" Aeson..= body
                            , "references" Aeson..= [ref]
                            ]
                case parseEither parseCip0119 val of
                    Left e -> fail e
                    Right m ->
                        drepMetaReferences m
                            `shouldBe` [ DRepMetaReference
                                            { drepMetaRefLabel = "Website"
                                            , drepMetaRefUri = "https://grace.example.com"
                                            }
                                       ]

    describe "readLimited" $ do
        it "accepts a body exactly at the limit" $ do
            let body = BS.replicate maxMetadataBytes 0x00
            ref <- newIORef body
            let br = atomicModifyIORef ref $ \b ->
                    if BS.null b
                        then (b, BS.empty)
                        else
                            let (chunk, rest) = BS.splitAt 4096 b
                            in  (rest, chunk)
            result <- readLimited br
            result `shouldBe` Right body

        it "rejects a body one byte over the limit" $ do
            let body = BS.replicate (maxMetadataBytes + 1) 0x00
            ref <- newIORef body
            let br = atomicModifyIORef ref $ \b ->
                    if BS.null b
                        then (b, BS.empty)
                        else
                            let (chunk, rest) = BS.splitAt 4096 b
                            in  (rest, chunk)
            result <- readLimited br
            case result of
                Left (FetchHttpError msg) ->
                    msg `shouldSatisfy` ("exceeds" `isInfixOf`)
                other ->
                    fail $ "Expected FetchHttpError, got: " <> show other

  where
    minimalMeta name =
        DRepMetadata
            { drepMetaName = name
            , drepMetaObjectives = Nothing
            , drepMetaMotivations = Nothing
            , drepMetaQualifications = Nothing
            , drepMetaPaymentAddress = Nothing
            , drepMetaDoNotList = False
            , drepMetaReferences = []
            }

    isLeft (Left _) = True
    isLeft (Right _) = False
