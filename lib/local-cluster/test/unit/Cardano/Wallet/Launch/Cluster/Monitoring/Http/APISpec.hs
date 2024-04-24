module Cardano.Wallet.Launch.Cluster.Monitoring.Http.APISpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.Monitoring.Http.API
    ( ApiT (..)
    )
import Cardano.Wallet.Launch.Cluster.Monitoring.Phase
    ( History (..)
    , Phase (..)
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
import Data.Time
    ( Day (ModifiedJulianDay)
    , UTCTime (UTCTime)
    , secondsToDiffTime
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , elements
    , forAll
    , listOf
    , oneof
    )

jsonRoundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> IO ()
jsonRoundtrip a = fromJSON (toJSON a) `shouldBe` Success a

spec :: Spec
spec = do
    describe "observe end-point json instances"
        $ it " roundtrips" $ forAll genObserveApiType jsonRoundtrip

genObserveApiType :: Gen (ApiT (History, MonitorState))
genObserveApiType = do
    history' <-History <$> listOf ((,) <$> genUTCTime <*> genPhase)
    state <- oneof [pure Wait, pure Step, pure Run]
    pure $ ApiT (history', state)

genUTCTime :: Gen UTCTime
genUTCTime = do
    day <- ModifiedJulianDay <$> arbitrary
    seconds <- secondsToDiffTime . (`mod` (3600 * 24)) <$> arbitrary
    pure $ UTCTime day seconds

genPhase :: Gen Phase
genPhase =
    elements
        [ RetrievingFunds
        , Metadata
        , Genesis
        , Pool0
        , Funding
        , Pools
        , Relay
        , Cluster Nothing
        ]
