{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.DRep.LayerSpec
    ( spec
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Pool.DB.MVar
    ( newDBLayer
    )
import Cardano.Wallet.DRep.Layer
    ( DRepInfo (..)
    , DRepLayer (..)
    , newDRepLayer
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyNetworkLayer
    , dummyTimeInterpreter
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepMetadata (..)
    , DRepRegistration (..)
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describe "Cardano.Wallet.DRep.Layer" $ do
    describe "listDRepInfos" $ do
        it "returns an empty list when the node is pre-Conway" $ do
            db <- newDBLayer dummyTimeInterpreter
            let nl = dummyNetworkLayer{listDReps = pure Nothing}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            infos `shouldBe` []

        it "returns empty list when network layer has no DReps" $ do
            db <- newDBLayer dummyTimeInterpreter
            let nl = dummyNetworkLayer{listDReps = pure (Just [])}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            infos `shouldBe` []

        it "returns DRepInfo records matching network layer registrations" $ do
            let regs = [testReg1, testReg2]
            db <- newDBLayer dummyTimeInterpreter
            let nl = dummyNetworkLayer{listDReps = pure (Just regs)}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            map drepInfoReg infos `shouldBe` regs

        it "gives Nothing metadata for a DRep with no anchor" $ do
            let reg = testReg1{drepRegAnchor = Nothing}
            db <- newDBLayer dummyTimeInterpreter
            let nl = dummyNetworkLayer{listDReps = pure (Just [reg])}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            case infos of
                [] -> fail "expected one DRepInfo"
                (info : _) -> drepInfoMetadata info `shouldBe` Nothing

        it "enriches a DRep with cached metadata when anchor hash matches" $ do
            let anchorHash = BS.replicate 32 0xAB
            let anchor =
                    DRepAnchor
                        { drepAnchorUrl = "https://example.com/drep.json"
                        , drepAnchorHash = anchorHash
                        }
            let reg = testReg1{drepRegAnchor = Just anchor}
            db <- newDBLayer dummyTimeInterpreter
            case db of
                DBLayer{atomically, putDRepMetadata} ->
                    atomically $ putDRepMetadata (hexBS anchorHash) testMeta
            let nl = dummyNetworkLayer{listDReps = pure (Just [reg])}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            case infos of
                [] -> fail "expected one DRepInfo"
                (info : _) -> drepInfoMetadata info `shouldBe` Just testMeta

        it "gives Nothing metadata when anchor hash has no cache entry" $ do
            let anchorHash = BS.replicate 32 0xCD
            let anchor =
                    DRepAnchor
                        { drepAnchorUrl = "https://example.com/drep.json"
                        , drepAnchorHash = anchorHash
                        }
            let reg = testReg1{drepRegAnchor = Just anchor}
            db <- newDBLayer dummyTimeInterpreter
            let nl = dummyNetworkLayer{listDReps = pure (Just [reg])}
            layer <- newDRepLayer nl db
            infos <- listDRepInfos layer
            case infos of
                [] -> fail "expected one DRepInfo"
                (info : _) -> drepInfoMetadata info `shouldBe` Nothing

  where
    testReg1 :: DRepRegistration
    testReg1 =
        DRepRegistration
            { drepRegId =
                DRepFromKeyHash (DRepKeyHash (BS.replicate 28 0x01))
            , drepRegExpiryEpoch = 500
            , drepRegAnchor = Nothing
            , drepRegDeposit = Coin 500000000
            , drepRegVotingPower = Coin 1000000
            , drepRegIsActive = True
            }

    testReg2 :: DRepRegistration
    testReg2 =
        DRepRegistration
            { drepRegId =
                DRepFromKeyHash (DRepKeyHash (BS.replicate 28 0x02))
            , drepRegExpiryEpoch = 600
            , drepRegAnchor = Nothing
            , drepRegDeposit = Coin 500000000
            , drepRegVotingPower = Coin 2000000
            , drepRegIsActive = True
            }

    testMeta :: DRepMetadata
    testMeta =
        DRepMetadata
            { drepMetaName = "Test DRep"
            , drepMetaObjectives = Nothing
            , drepMetaMotivations = Nothing
            , drepMetaQualifications = Nothing
            , drepMetaPaymentAddress = Nothing
            , drepMetaDoNotList = False
            , drepMetaReferences = []
            }

    hexBS :: BS.ByteString -> Text
    hexBS = T.decodeUtf8 . BA.convertToBase BA.Base16
