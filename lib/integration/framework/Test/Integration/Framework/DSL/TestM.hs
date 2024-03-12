{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Integration.Framework.DSL.TestM
    ( TestM
    , itM
    , Over
    , check
    , over
    , request
    , partialRight
    , pattern Partial
    , clientError
    , errStatusIs
    , errResponseBody
    , fieldIs
    ) where

import Control.Lens
    ( (^.)
    )
import Control.Monad
    ( unless
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , ReaderT (..)
    , ask
    , lift
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , runResourceT
    )
import Data.Aeson
    ( Value (..)
    , eitherDecode
    )
import Data.String
    ( IsString (..)
    )
import Prelude hiding
    ( lookup
    )
import Servant.Client
    ( ClientError (..)
    , ClientM
    )
import Test.Hspec
    ( SpecWith
    , shouldBe
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.Context
    ( Context
    , runClientRequest
    )

import qualified Data.Aeson.KeyMap as Aeson

pattern Partial :: Show a => b -> Either a b
pattern Partial a <- (partialRight -> a)

type TestM = ReaderT Context (ResourceT IO)

itM :: String -> TestM () -> SpecWith Context
itM d f = it d $ \ctx -> do
    runResourceT $ runReaderT f ctx

request :: ClientM a -> TestM (Either ClientError a)
request f = do
    ctx <- ask
    lift $ runClientRequest ctx f

type Over v = ReaderT v TestM

check :: (v -> TestM ()) -> Over v ()
check f = ask >>= lift . f

over :: v -> Over v a -> TestM a
over k f = runReaderT f k

partialRight :: (Show a) => Either a b -> b
partialRight (Right b) = b
partialRight (Left x) = error $ "partialRight: " <> show x

clientError :: Either ClientError a -> (Over ClientError ()) -> TestM ()
clientError (Left e) f = runReaderT f e
clientError Right{} _ = error "clientError: expected Left, but got Right"

errStatusIs :: Int -> Over ClientError ()
errStatusIs expected = do
    err <- ask
    let actual = case err of
            FailureResponse _ res -> res ^. #responseStatusCode . #statusCode
            _ -> error "errStatusIs: expected a FailureResponse"
    liftIO $ actual `shouldBe` expected

errResponseBody :: (Value -> Bool) -> Over ClientError ()
errResponseBody expected = do
    err <- ask
    let actual = case err of
            FailureResponse _ res -> res ^. #responseBody
            _ -> error "errResponseBody: expected a FailureResponse"
    case eitherDecode actual of
        Left e -> error $ "errResponseBody: " <> e
        Right v ->
            unless (expected v)
                $ error
                $ "errResponseBody: expected " <> show v

fieldIs :: String -> Value -> Value -> Bool
fieldIs field expected = \case
    Object o -> case Aeson.lookup (fromString field) o of
        Just actual -> actual == expected
        Nothing -> False
    _ -> False
