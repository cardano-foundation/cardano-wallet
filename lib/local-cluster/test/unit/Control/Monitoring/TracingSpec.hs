module Control.Monitoring.TracingSpec
    ( spec
    )
where

import Prelude

import Control.Foldl
    ( Fold
    )
import Control.Monitoring.Folder
    ( mkTracingFromFold
    )
import Control.Monitoring.Tracing
    ( AnyTracing (AnyTracing)
    , StateS (..)
    , observe
    , step
    , switch
    , trace
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )

import qualified Control.Foldl as L

mkTracing :: StateS x -> Fold a b -> AnyTracing a b
mkTracing w f = AnyTracing w $ mkTracingFromFold f w

testObserve
    :: (Show b, Eq b)
    => StateS x
    -> Fold a b
    -> b
    -> (AnyTracing a b -> AnyTracing a b)
    -> Expectation
testObserve state fold target changes =
    observe (changes $ mkTracing state fold) `shouldBe` target

spec :: Spec
spec = do
    describe "a Tracing" $ do
        it "accept no trace in run mode" $ do
            testObserve RunS L.sum (0 :: Int) id
        it "accept all traces in run mode" $ do
            testObserve RunS L.sum (101 :: Int)
                $ trace 100 . trace 1
        it "ignore traces in step state" $ do
            testObserve StepS L.sum (0 :: Int)
                $ trace 100 . trace 1
        it "accept only one trace in wait mode" $ do
            testObserve WaitS L.sum (1 :: Int)
                $ trace 100 . trace 1
        it "accept multiple traces in wait mode via stepping" $ do
            testObserve WaitS L.sum (101 :: Int)
                $ trace 100 . step . trace 1
        it "can switch to run mode from step mode" $ do
            testObserve StepS L.sum (101 :: Int)
                $ trace 100 . trace 1 . switch
        it "can switch to step mode from run mode" $ do
            testObserve RunS L.sum (100 :: Int)
                $ trace 100 . step . trace 1 . switch
        it "can switch to run mode from wait mode" $ do
            testObserve WaitS L.sum (101 :: Int)
                $ trace 100 . trace 1 . switch
        it "can switch back and forth between step and run mode" $ do
            testObserve StepS L.sum (1001 :: Int)
                $ trace 1000
                    . step
                    . trace 100
                    . switch
                    . trace 1
                    . switch
