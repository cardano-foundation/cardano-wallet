{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Hspec.ExtraSpec (spec) where

import Prelude

import Control.Monad
    ( forM_
    , unless
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )
import Data.Bifunctor
    ( first
    )
import Data.Function
    ( on
    )
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List
    ( isPrefixOf
    , nubBy
    )
import Data.Maybe
    ( fromMaybe
    )
import Fmt
    ( (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import System.Exit
    ( ExitCode (..)
    )
import System.IO
    ( stderr
    , stdout
    )
import System.IO.Silently
    ( capture_
    , hCapture
    , silence
    )
import Test.Hspec
    ( ActionWith
    , Expectation
    , Spec
    , SpecWith
    , beforeAll
    , before_
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    )
import Test.Hspec.Core.Runner
    ( Summary (..)
    , defaultConfig
    , runSpec
    )
import Test.Hspec.Core.Spec
    ( runIO
    , sequential
    )
import Test.Hspec.Expectations.Lifted
    ( shouldReturn
    )
import Test.Hspec.Extra
    ( aroundAll
    , hspecMain
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , Property
    , counterexample
    , elements
    , listOf
    , oneof
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )
import Test.Utils.Env
    ( withEnv
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Environment
    ( lookupEnv
    , setEnv
    , withArgs
    )
import UnliftIO.Exception
    ( bracket
    , throwString
    , tryAny
    , tryDeep
    )
import UnliftIO.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , tryReadMVar
    , tryTakeMVar
    )

import qualified Test.Hspec.Extra as Extra

spec :: Spec
spec = do
    itSpec
    aroundAllSpec
    mainSpec

itSpec :: Spec
itSpec = describe "Extra.it" $ before_ (setEnv "TESTS_RETRY_FAILED" "y") $ do
    it "equals Hspec.it on success" $ do
        let test = 1 `shouldBe` (1 :: Int)
        test `shouldMatchHSpecIt` test

    it "equals Hspec.it on failure" $ do
        let test = (2 + 2) `shouldBe` (5 :: Int)
        test `shouldMatchHSpecIt` test

    describe "when first attempt fails due to flakiness" $ do
        describe "when the retry succeeds" $ do
            let flaky = expectationFailure "flaky test"
            let succeed = 1 `shouldBe` (1 :: Int)
            it "succeeds" $ do
                outcomes <- newIORef [flaky, succeed]
                (dynamically outcomes) `shouldMatchHSpecIt` succeed

        describe "when the retry also fails" $ do
            -- Some tests use limited resources and cannot be retried.
            -- On failures, we should make sure to show the first failure
            -- which is the interesting one.
            it "fails with the first error" $ do
                let failure = expectationFailure "failure"
                let noRetry = expectationFailure "test can't be retried"
                outcomes <- newIORef [failure, noRetry]
                (dynamically outcomes) `shouldMatchHSpecIt` failure
    it "can time out" $ do
        let micro = (1000 * 1000 *)
        let timeout = do
                threadDelay (micro 10)
                expectationFailure "should have timed out"
        res <- runIt (Extra.itWithCustomTimeout 2) timeout
        res `shouldContain` "timed out in 2 seconds"
  where
    -- \| lhs `shouldMatchHSpecIt` rhs asserts that the output of running
    -- (Extra.it "" lhs) and (Hspec.it "" rhs) are equal. Modulo random seed-
    -- and execution time-information.
    shouldMatchHSpecIt :: IO () -> IO () -> Expectation
    shouldMatchHSpecIt extraTest hspecTest = do
        extraRes <- runIt Extra.it extraTest
        hspecRes <- runIt it hspecTest
        extraRes `shouldBe` hspecRes

    runIt
        :: (String -> ActionWith () -> SpecWith ())
        -- \^ it version
        -> IO ()
        -- \^ test body
        -> IO String
    -- \^ hspec output
    runIt anyIt body =
        fmap stripTime
            $ capture_
            $ flip runSpec defaultConfig
            $ beforeAll (return ())
            $ anyIt "<test spec>" (const body)
      where
        -- \| Remove time and seed such that we can compare the captured stdout
        -- of two different hspec runs.
        stripTime :: String -> String
        stripTime =
            unlines
                . filter (not . ("Finished in" `isPrefixOf`))
                . filter (not . ("Randomized" `isPrefixOf`))
                . filter (not . ("retry:" `isPrefixOf`))
                . lines

    -- \| Returns an IO action that is different every time you run it!,
    -- according to the supplied IORef of outcomes.
    dynamically
        :: IORef [IO ()]
        -> IO ()
    dynamically outcomes = do
        outcome : rest <- readIORef outcomes
        writeIORef outcomes rest
        outcome

aroundAllSpec :: Spec
aroundAllSpec = sequential $ do
    let withMockResource :: (MonadUnliftIO m) => a -> (a -> m r) -> m r
        withMockResource a = bracket (pure a) (const $ pure ())

        withMVarResource :: (Show a, Eq a, MonadUnliftIO m) => a -> (MVar a -> m r) -> m r
        withMVarResource a = bracket (newMVar a) (takeMVarCheck a)

        takeMVarCheck :: (Show a, Eq a, MonadUnliftIO m) => a -> MVar a -> m ()
        takeMVarCheck a var = tryTakeMVar var `shouldReturn` Just a

        resourceA = 1 :: Int

    describe "Extra.aroundAll" $ do
        describe "trivial" $ aroundAll (withMockResource resourceA) $ do
            it
                "provides resource to first test"
                (`shouldBe` resourceA)
            it
                "provides resource to second test"
                (`shouldBe` resourceA)

        describe "basic" $ aroundAll (withMVarResource resourceA) $ do
            it "provides resource to first test" $ \var ->
                tryReadMVar @IO var `shouldReturn` Just resourceA

            it "provides resource to second test" $ \var ->
                tryReadMVar @IO var `shouldReturn` Just resourceA

        mvar <- runIO newEmptyMVar
        let withResource = bracket (putMVar mvar ()) (`takeMVarCheck` mvar)

        describe "lazy allocation" $ aroundAll withResource $ do
            before <- runIO $ tryReadMVar mvar
            it "not before the spec runs" $ \_ -> do
                before `shouldBe` Nothing
                tryReadMVar mvar `shouldReturn` Just ()

        describe "prompt release"
            $ it "after the spec runs"
            $ tryReadMVar @IO mvar
            `shouldReturn` Nothing

        describe "exceptions" $ do
            let trySpec =
                    fmap (first show)
                        . tryAny
                        . silence
                        . flip runSpec defaultConfig
            let bombBefore = bracket (throwString "bomb1") (const $ pure ())
            let bombAfter = bracket (pure ()) (const $ throwString "bomb2")

            it "while allocating resource" $ do
                a <-
                    trySpec
                        $ aroundAll bombBefore
                        $ it "should never happen"
                        $ const
                        $ False
                        `shouldBe` True
                a `shouldBe` Right (Summary 1 1)

            it "while releasing resource" $ do
                b <-
                    trySpec
                        $ aroundAll bombAfter
                        $ it "spec"
                        $ const
                        $ pure @IO ()
                b `shouldBe` Right (Summary 1 0)

mainSpec :: Spec
mainSpec = sequential $ describe "hspecMain" $ do
    prop "correctly sets environment variables" prop_hspecMain

prop_hspecMain
    :: [((NiceString, NiceString), ArgStyle)]
    -> NiceString
    -> HspecArgs
    -> HspecArgs
    -> Bool
    -> Property
prop_hspecMain vars (NiceString other) (HspecArgs argsBefore) (HspecArgs argsAfter) pass = monadicIO $ do
    monitor $ counterexample ("args = " +| args |+ "")
    ((err, out), res) <- run $ captureBoth $ tryDeep doTest
    monitor
        $ counterexample
        $ unlines
            [ "res = " +|| res ||+ ""
            , "error output:"
            , err
            , "other output:"
            , out
            , "success = " +|| success ||+ ""
            ]
    assert $ case res of
        Right () -> success
        Left exitCode -> case exitCode of
            ExitSuccess -> success
            ExitFailure 89 -> argsError
            ExitFailure _ -> failure
  where
    doTest = withEnv [] $ withArgs args $ hspecMain exampleSpec
    success = pass || null env
    failure = not pass
    argsError = any (\((n, _), _) -> null n || elem '=' n) env

    vars' = nubBy ((==) `on` (fst . fst)) vars
    env =
        [ ((n, v), s)
        | ((NiceString n, NiceString v), s) <- vars'
        , n /= other
        ]

    mainArgs =
        mconcat
            [fmtArg "-e" "--env" (n <> "=" <> v) s | ((n, v), s) <- env]
    args = argsBefore ++ mainArgs ++ argsAfter

    exampleSpec :: Spec
    exampleSpec = describe "example spec" $ do
        forM_ env $ \((n, v), _) ->
            it ("env " +| n |+ " set")
                $ lookupEnv @IO n
                `shouldReturn` if null v then Nothing else Just v
        unless (null other)
            $ it "env not set"
            $ lookupEnv @IO other
            `shouldReturn` Nothing
        it "should we pass?"
            $ True
            `shouldBe` pass

newtype NiceString = NiceString String deriving (Show, Eq)

instance Arbitrary NiceString where
    arbitrary =
        fmap NiceString
            $ listOf
            $ elements
            $ mconcat
                [['0' .. '9'], ['a' .. 'z'], ['A' .. 'Z'], "_."]
    shrink (NiceString s) = NiceString <$> shrink s

data ArgStyle = Short | LongSpace | LongEqual deriving (Show, Eq, Ord)

fmtArg :: String -> String -> String -> ArgStyle -> [String]
fmtArg s _ v Short = [s, v]
fmtArg _ l v LongSpace = [l, v]
fmtArg _ l v LongEqual = [l ++ "=" ++ v]

instance Arbitrary ArgStyle where
    arbitrary = elements [Short, LongSpace, LongEqual]

newtype HspecArgs = HspecArgs {getHspecArgs :: [String]}
    deriving (Show, Eq)

instance Arbitrary HspecArgs where
    arbitrary = HspecArgs . mconcat . nubArgs <$> listOf (oneof args)
      where
        args =
            [ genArg (Just "-j") "--jobs" =<< num
            , pure <$> genOpt "fail-fast"
            , pure <$> genOpt "randomize"
            , genArg Nothing "--seed" =<< num
            ]
        num = show @Int . getPositive <$> arbitrary

        nubArgs = nubBy ((==) `on` head)

        genOpt :: String -> Gen String
        genOpt name = elements $ map ("--" ++) [name, "no-" ++ name]

        genArg :: Maybe String -> String -> String -> Gen [String]
        genArg short long val = fmtArg short' long val <$> elements styles
          where
            short' = fromMaybe "" short
            styles = maybe id (const (Short :)) short [LongSpace, LongEqual]

-- | Capture both @(stderr, stdout)@ from an IO action.
captureBoth :: IO a -> IO ((String, String), a)
captureBoth = fmap reorder . hCapture [stderr] . hCapture [stdout]
  where
    reorder (e, (s, a)) = ((e, s), a)
