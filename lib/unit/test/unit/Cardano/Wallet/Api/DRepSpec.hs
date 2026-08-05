{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Api.DRepSpec
    ( spec
    ) where

import Cardano.Wallet.Api.Http.Shelley.Server
    ( getDRep
    , listDReps
    , suggestedDReps
    )
import Cardano.Wallet.Api.Types
    ( ApiDRepCredential (..)
    , ApiDRepInfo (..)
    , ApiDRepSpecifier (..)
    , DRepStatus (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepMetadata (..)
    , DRepRegistration (..)
    , encodeDRepIDBech32
    )
import Data.Either
    ( isLeft
    )
import Data.List
    ( nub
    )
import Data.Maybe
    ( isJust
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word8
    )
import Servant.Server
    ( ServerError (..)
    , runHandler
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Prelude

import qualified Cardano.Wallet.DRep.Layer as DRep
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "DRep HTTP handlers" $ do
    it "lists on-chain fields and the cached name without full metadata" $ do
        result <-
            runHandler $ listDReps $ layer [mkInfo 1 True $ Just metadata]

        case result of
            Left err -> fail $ show err
            Right [info] -> do
                drepInfoId info `shouldBe` encodeDRepIDBech32 (drepId 1)
                drepInfoCredential info
                    `shouldBe` ApiDRepCredential
                        { credentialType = "key_hash"
                        , credentialHash = hexBS $ BS.replicate 28 1
                        }
                drepInfoStatus info `shouldBe` Active
                drepInfoExpiryEpoch info `shouldBe` 500
                drepInfoVotingPower info `shouldBe` 1_000_000
                drepInfoDeposit info `shouldBe` 500_000_000
                drepInfoName info `shouldBe` Just "Test DRep"
                drepInfoMetadata info `shouldBe` Nothing
            Right infos -> fail $ "expected one DRep, got " <> show infos

    it "returns full cached metadata for a known DRep" $ do
        result <-
            runHandler
                $ getDRep
                    (layer [mkInfo 1 True $ Just metadata])
                    (SpecificDRep $ FromDRepID $ drepId 1)

        case result of
            Left err -> fail $ show err
            Right info -> do
                drepInfoId info `shouldBe` encodeDRepIDBech32 (drepId 1)
                drepInfoMetadata info `shouldSatisfy` isJust

    it "returns 404 for sentinel and unknown DRep identifiers" $ do
        sentinel <-
            runHandler
                $ getDRep
                    (layer [mkInfo 1 True $ Just metadata])
                    (SpecificDRep Abstain)
        noConfidence <-
            runHandler
                $ getDRep
                    (layer [mkInfo 1 True $ Just metadata])
                    (SpecificDRep NoConfidence)
        unknown <-
            runHandler
                $ getDRep
                    (layer [mkInfo 1 True $ Just metadata])
                    (SpecificDRep $ FromDRepID $ drepId 2)

        sentinel `shouldSatisfy` isLeft
        noConfidence `shouldSatisfy` isLeft
        unknown `shouldSatisfy` isLeft
        errorCode sentinel `shouldBe` Just 404
        errorCode noConfidence `shouldBe` Just 404
        errorCode unknown `shouldBe` Just 404

    it
        "excludes the top 35 and filters inactive, unidentified, and opted-out DReps"
        $ do
            let infos =
                    [ mkInfo n (n /= 2) $ metadataFor n
                    | n <- [1 .. 40]
                    ]
                metadataFor 1 = Just metadata{drepMetaDoNotList = True}
                metadataFor 3 = Nothing
                metadataFor _ = Just metadata
                expected = Set.fromList $ encodeDRepIDBech32 . drepId <$> [4, 5]

            result <- runHandler $ suggestedDReps (layer infos) $ Just 200

            case result of
                Left err -> fail $ show err
                Right suggestions -> do
                    Set.fromList (drepInfoId <$> suggestions) `shouldBe` expected
                    (drepInfoStatus <$> suggestions) `shouldSatisfy` all (== Active)
                    (drepInfoName <$> suggestions) `shouldSatisfy` all isJust

    it "applies the default count and caps every larger Word at 200" $ do
        let infos = [mkInfo n True $ Just metadata | n <- [1 .. 235]]

        defaultResult <- runHandler $ suggestedDReps (layer infos) Nothing
        cappedResult <- runHandler $ suggestedDReps (layer infos) $ Just 300
        largestWordResult <-
            runHandler $ suggestedDReps (layer infos) $ Just maxBound

        assertSampleSize 20 defaultResult
        assertSampleSize 200 cappedResult
        assertSampleSize 200 largestWordResult
  where
    assertSampleSize expected = \case
        Left err -> fail $ show err
        Right suggestions -> do
            length suggestions `shouldBe` expected
            length (nub $ drepInfoId <$> suggestions) `shouldBe` expected

    errorCode = \case
        Left err -> Just $ errHTTPCode err
        Right _ -> Nothing

layer :: [DRep.DRepInfo] -> DRep.DRepLayer IO
layer infos =
    DRep.DRepLayer
        { DRep.listDRepInfos = pure infos
        , DRep.getDRepMetadata = const $ pure Nothing
        }

mkInfo :: Word8 -> Bool -> Maybe DRepMetadata -> DRep.DRepInfo
mkInfo n active cachedMetadata =
    DRep.DRepInfo
        { DRep.drepInfoReg =
            DRepRegistration
                { drepRegId = drepId n
                , drepRegExpiryEpoch = 500
                , drepRegAnchor = Nothing
                , drepRegDeposit = Coin 500_000_000
                , drepRegVotingPower = Coin $ fromIntegral n * 1_000_000
                , drepRegIsActive = active
                }
        , DRep.drepInfoMetadata = cachedMetadata
        }

drepId :: Word8 -> DRepID
drepId n = DRepFromKeyHash $ DRepKeyHash $ BS.replicate 28 n

metadata :: DRepMetadata
metadata =
    DRepMetadata
        { drepMetaName = "Test DRep"
        , drepMetaObjectives = Just "Test objectives"
        , drepMetaMotivations = Just "Test motivations"
        , drepMetaQualifications = Just "Test qualifications"
        , drepMetaPaymentAddress = Just "addr_test1"
        , drepMetaDoNotList = False
        , drepMetaReferences = []
        }

hexBS :: BS.ByteString -> Text
hexBS = T.decodeUtf8 . BA.convertToBase BA.Base16
