module Cardano.Wallet.Launch.Cluster.Monitoring.Http.APISpec
    ( spec
    , genObservation
    , genMonitorState
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( ApiT (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
    , RelayNode (..)
    )
import Control.Monitoring.Tracing
    ( MonitorState (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , Result (..)
    , ToJSON (..)
    , fromJSON
    )
import Data.OpenApi
    ( ToSchema
    , validateToJSON
    )
import Data.Time
    ( Day (ModifiedJulianDay)
    , UTCTime (UTCTime)
    , secondsToDiffTime
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , forAll
    , listOf
    , oneof
    )

jsonRoundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> IO ()
jsonRoundtrip a = fromJSON (toJSON a) `shouldBe` Success a

validate :: (ToJSON t, ToSchema t) => t -> Expectation
validate x = validateToJSON x `shouldBe` []

spec :: Spec
spec = do
    describe "observe endpoint" $ do
        it "json response roundtrips"
            $ forAll genObservation
            $ jsonRoundtrip . ApiT
        it "json response schema validates random data"
            $ forAll genObservation
            $ validate . ApiT
    describe "switch endpoint" $ do
        it "json response roundtrips"
            $ forAll genMonitorState
            $ jsonRoundtrip . ApiT
        it "json response validates random data"
            $ forAll genMonitorState
            $ validate . ApiT

genObservation :: Gen (History, MonitorState)
genObservation = do
    history' <-
        History  <$> listOf ((,) <$> genUTCTime <*> genPhase)
    state <- genMonitorState
    pure (history', state)

genMonitorState :: Gen MonitorState
genMonitorState = oneof [pure Wait, pure Step, pure Run]

genUTCTime :: Gen UTCTime
genUTCTime = do
    day <- ModifiedJulianDay <$> arbitrary
    seconds <- secondsToDiffTime . (`mod` (3600 * 24)) <$> arbitrary
    pure $ UTCTime day seconds

genPhase :: Gen Phase
genPhase =
    oneof
        [ pure RetrievingFunds
        , pure Metadata
        , pure Genesis
        , pure Pool0
        , pure Funding
        , pure Pools
        , pure Relay
        , Cluster
            <$> oneof
                [pure Nothing, Just <$> genRelayNodePath]
        ]

genRelayNodePath :: Gen RelayNode
genRelayNodePath =
    RelayNode <$> do
        oneof [pure "path1", pure "path1/path2", pure "/path3"]
