{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Pool.Jormungandr.Metadata
    ( ApiStakePool (..), ApiStakePoolMetrics (..), StakePoolMetadata (..) )
import Cardano.Wallet.Api
    ( ListStakePools )
import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountState (..)
    , ApiStakeDistribution (..)
    , ApiT (..)
    , JormungandrBinary
    , StakeApiResponse (..)
    )
import Cardano.Wallet.Jormungandr.Binary
    ( Block )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..), PoolOwner (..), StakePoolTicker (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromText, unsafeMkPercentage )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( FromJSON, ToJSON, eitherDecode )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Either
    ( isLeft )
import Data.FileEmbed
    ( embedFile, makeRelativeToProject )
import Data.List
    ( foldl' )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Swagger
    ( Definitions, NamedSchema (..), Schema, ToSchema (..) )
import Data.Swagger.Declare
    ( Declare )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Servant.API
    ( MimeUnrender (..) )
import Servant.Swagger.Test
    ( validateEveryToJSON )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import Test.Aeson.Internal.RoundtripSpecs
    ( roundtripSpecs )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..), Gen, applyArbitrary3, choose, frequency, vector )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Time
    ( genUniformTime )

import qualified Cardano.Wallet.Api.Types as W
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Test.Utils.Roundtrip as Utils

spec :: Spec
spec = do
    describe "Jormungandr Api" $ do
        describe "JSON roundtrip tests for API types" $ do
            roundtripSpecs $ Proxy @AccountState

        describe "Example account state objects are properly decoded" $ do

            let testBalance = 1000
            let testTransactionCount = 1
            let testPoolRatio1 = 1
            let testPoolRatio2 = 10

            it "With 0 stake pools" $ do

                let testAccountState = Aeson.encode [aesonQQ|
                        { "value": #{testBalance}
                        , "counter": #{testTransactionCount}
                        , "delegation": {"pools": []}
                        }|]
                eitherDecode testAccountState `shouldBe` Right AccountState
                    { currentBalance = Quantity testBalance
                    , totalTransactionCount = Quantity testTransactionCount
                    , stakePools = []
                    }

            it "With 1 stake pool" $ do

                let testAccountState = Aeson.encode [aesonQQ|
                        { "value": #{testBalance}
                        , "counter": #{testTransactionCount}
                        , "delegation":
                            { "pools":
                                [[#{toText testPoolId1}, #{testPoolRatio1}]]
                            }
                        }|]
                eitherDecode testAccountState `shouldBe` Right AccountState
                    { currentBalance = Quantity testBalance
                    , totalTransactionCount = Quantity testTransactionCount
                    , stakePools = [(testPoolId1, Quantity testPoolRatio1)]
                    }

            it "With n stake pools" $ do

                let testAccountState = Aeson.encode [aesonQQ|
                        { "value": #{testBalance}
                        , "counter": #{testTransactionCount}
                        , "delegation":
                            { "pools":
                                [ [#{toText testPoolId1}, #{testPoolRatio1}]
                                , [#{toText testPoolId2}, #{testPoolRatio2}]
                                ]
                            }
                        }|]
                eitherDecode testAccountState `shouldBe` Right AccountState
                    { currentBalance = Quantity testBalance
                    , totalTransactionCount = Quantity testTransactionCount
                    , stakePools =
                        [ (testPoolId1, Quantity testPoolRatio1)
                        , (testPoolId2, Quantity testPoolRatio2)
                        ]
                    }

        it "example stake endpoint response is properly decoded" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Right StakeApiResponse {
                   epoch = ApiT 252054,
                   stake = ApiStakeDistribution {
                       dangling = ApiT (Quantity 0),
                       pools = [((ApiT (PoolId "}t\158\244$P\DEL\184\SI\237\r\"\137\213\&5\169Ohp\173\208\207\139\&7L\254l\174\a\131 \236"))
                                 , (ApiT (Quantity 1)))],
                       unassigned = ApiT (Quantity 100100000000000)
                       }
                    }
            return ()

        it "example empty stake endpoint response is properly decoded" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Right StakeApiResponse {
                   epoch = ApiT 252054,
                   stake = ApiStakeDistribution {
                       dangling = ApiT (Quantity 0),
                       pools = [],
                       unassigned = ApiT (Quantity 100100000000000)
                       }
                    }
            return ()

        describe "MimeUnrender decoding" $ do
            it "returns 'Left' when encountering an invalid binary block" $ do
                mimeUnrender (Proxy @JormungandrBinary) ""
                    `shouldSatisfy` (isLeft @_ @Block)

    describe
        "verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using validateToJSON." $ do
        validateEveryToJSON
            (Proxy :: Proxy (ListStakePools ApiStakePool))


    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            jsonRoundtripAndGolden $ Proxy @ApiStakePool
            jsonRoundtripAndGolden $ Proxy @ApiStakePoolMetrics

  where
    decodeJSON = eitherDecode :: BL.ByteString -> Either String StakeApiResponse
    jsonRoundtripAndGolden
        :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
        => Proxy a
        -> Spec
    jsonRoundtripAndGolden = Utils.jsonRoundtripAndGolden
        ($(getTestData) </> "Cardano-jormungandr" </> "Wallet" </> "Api")

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

-- | Specification file, embedded at compile-time and decoded right away
specification :: Aeson.Value
specification =
    unsafeDecode bytes
  where
    bytes = $(
        let swaggerYaml = "./specifications/api/swagger.yaml"
        in liftIO (lookupEnv "SWAGGER_YAML") >>=
        maybe (makeRelativeToProject swaggerYaml) pure >>=
        embedFile
        )
    unsafeDecode =
        either (error . (msg <>) . show) Prelude.id . Yaml.decodeEither'
    msg = "Whoops! Failed to parse or find the api specification document: "

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

instance Arbitrary AccountState where
    arbitrary = applyArbitrary3 AccountState

instance Arbitrary PoolId where
    arbitrary = PoolId . BS.pack
        <$> replicateM 32 arbitrary

instance Arbitrary (Quantity "lovelace" Word64) where
    arbitrary = Quantity <$> arbitrary
    shrink (Quantity q) = Quantity <$> shrink q

instance Arbitrary (Quantity "stake-pool-ratio" Word64) where
    arbitrary = Quantity <$> arbitrary
    shrink (Quantity q) = Quantity <$> shrink q

instance Arbitrary (Quantity "transaction-count" Word64) where
    arbitrary = Quantity <$> arbitrary
    shrink (Quantity q) = Quantity <$> shrink q

instance Arbitrary a => Arbitrary (W.ApiT a) where
    arbitrary = W.ApiT <$> arbitrary
    shrink = fmap W.ApiT . shrink . W.getApiT

instance Arbitrary ApiStakePoolMetrics where
    arbitrary = do
        stakes <- Quantity . fromIntegral <$> choose (1::Integer, 1_000_000_000_000)
        blocks <- Quantity . fromIntegral <$> choose (1::Integer, 22_600_000)
        pure $ ApiStakePoolMetrics stakes blocks

instance Arbitrary (W.ApiListStakePools ApiStakePool) where
    arbitrary = W.ApiListStakePools <$> arbitrary <*> (pure Nothing)

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

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "percent" Percentage) where
    shrink _  = []
    arbitrary = Quantity <$> genPercentage
      where
        genPercentage = unsafeMkPercentage . fromRational . toRational <$> genDouble
          where
            genDouble :: Gen Double
            genDouble = choose (0, 1)

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
            T.pack <$> vector len
        arbitraryMaybeText maxLen = frequency
            [ (9, Just <$> arbitraryText maxLen)
            , (1, pure Nothing) ]

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . BS.pack <$> vector 32

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary W.Iso8601Time where
    arbitrary = W.Iso8601Time <$> genUniformTime

instance ToSchema ApiStakePool where
    declareNamedSchema _ = declareSchemaForDefinition "ApiJormungandrStakePool"

instance ToSchema ApiStakePoolMetrics where
    declareNamedSchema _ = declareSchemaForDefinition "ApiJormungandrStakePoolMetrics"

instance ToSchema (W.ApiListStakePools ApiStakePool) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiJormungandrListStakePools"

{-------------------------------------------------------------------------------
                                  Test data
-------------------------------------------------------------------------------}

testPoolId1 :: PoolId
testPoolId1 = unsafeFromText
    "c780f14f9782770014d8bcd514b1bc664653d15f73a7158254730c6e1aa9f356"

testPoolId2 :: PoolId
testPoolId2 = unsafeFromText
    "653f9aa1e6c0374528517a37f51d356466cb1b415dcb8d4100772879f41f087c"
