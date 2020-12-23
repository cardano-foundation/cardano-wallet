{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Middleware.LoggingSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..) )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Wallet.Api.Server
    ( Listen (..), withListeningSocket )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Monad
    ( forM_, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.STM
    ( atomically )
import Control.Tracer
    ( Tracer )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager
    , RequestBody (..)
    , defaultManagerSettings
    , httpLbs
    , method
    , newManager
    , parseRequest
    , requestBody
    , requestHeaders
    )
import Network.HTTP.Types.Header
    ( hContentType )
import Network.Socket
    ( Socket )
import Network.Wai.Handler.Warp
    ( runSettingsSocket, setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog (..)
    , ApiLoggerSettings
    , HandlerLog (..)
    , RequestId (..)
    , newApiLoggerSettings
    , obfuscateKeys
    , withApiLogger
    )
import Servant
    ( (:<|>) (..)
    , (:>)
    , Application
    , DeleteNoContent
    , Get
    , JSON
    , NoContent (..)
    , OctetStream
    , PostCreated
    , ReqBody
    , Server
    , err400
    , err500
    , err503
    , serve
    , throwError
    )
import Servant.Server
    ( Handler )
import Test.Hspec
    ( Spec, after, afterAll, beforeAll, describe, it, shouldBe, shouldContain )
import Test.QuickCheck
    ( Arbitrary (..), choose, counterexample, property, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor )
import UnliftIO.Async
    ( Async, async, cancel, mapConcurrently, replicateConcurrently_ )
import UnliftIO.MVar
    ( newEmptyMVar, putMVar, readMVar )

import qualified Data.Aeson as Aeson
import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "Logging Middleware"
    $ beforeAll setup $ after clearLogs $ afterAll tearDown $ do
    it "GET, 200, no query" $ \ctx -> do
        get ctx "/get"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /get")
            , (Debug, "")
            , (Info, "200 OK")
            , (Debug, "14")
            , (Debug, "LogRequestFinish")
            ]

    it "GET, 200, with query" $ \ctx -> do
        get ctx "/get?query=patate"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /get?query=patate")
            , (Debug, "")
            , (Info, "200 OK")
            , (Debug, "14")
            , (Debug, "LogRequestFinish")
            ]

    it "GET, 200, not json" $ \ctx -> do
        get ctx "/not-json"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /not-json")
            , (Debug, "")
            , (Info, "200 OK")
            , (Debug, "\NUL\NUL\NUL")
            , (Debug, "LogRequestFinish")
            ]

    it "POST, 201, with sensitive fields" $ \ctx -> do
        post ctx "/post" (MkJson { field = "patate", sensitive = 14 })
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[POST] /post")
            , (Debug, "{\"sensitive\":\"*****\",\"field\":\"patate\"}")
            , (Info, "201 Created")
            , (Debug, "{\"status\":\"ok\",\"whatever\":42}")
            , (Debug, "LogRequestFinish")
            ]

    it "POST, 400, invalid payload (not json)" $ \ctx -> do
        postIlled ctx "/post" "\NUL\NUL\NUL"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[POST] /post")
            , (Debug, "Invalid payload: not JSON")
            , (Info, "400 Bad Request")
            , (Debug, "Failed reading: not a valid json value")
            , (Debug, "LogRequestFinish")
            ]

    it "DELETE, 202, no query" $ \ctx -> do
        delete ctx "/delete"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[DELETE] /delete")
            , (Debug, "")
            , (Info, "204 No Content")
            , (Debug, "")
            , (Debug, "LogRequestFinish")
            ]

    it "GET, 400" $ \ctx -> do
        get ctx "/error400"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /error400")
            , (Debug, "")
            , (Info, "400 Bad Request")
            , (Debug, "")
            , (Debug, "LogRequestFinish")
            ]

    it "GET, 500" $ \ctx -> do
        get ctx "/error500"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /error500")
            , (Debug, "")
            , (Error, "500 Internal Server Error")
            , (Debug, "")
            , (Debug, "LogRequestFinish")
            ]

    it "GET, 503" $ \ctx -> do
        get ctx "/error503"
        expectLogs ctx
            [ (Debug, "LogRequestStart")
            , (Info, "[GET] /error503")
            , (Debug, "")
            , (Warning, "503 Service Unavailable")
            , (Debug, "")
            , (Debug, "LogRequestFinish")
            ]

    it "different request ids" $ \ctx ->
        property $ \(NumberOfRequests n) -> monadicIO $ do
            entries <- liftIO $ do
                replicateConcurrently_ n (get ctx "/get")
                skipPrevLogs <$> takeLogs ctx
            let getReqId (ApiLog (RequestId rid) _) = rid
            let uniqueReqIds = L.nubBy (\l1 l2 -> getReqId l1 == getReqId l2)
            let numUniqueReqIds = length (uniqueReqIds entries)
            monitor $ counterexample $ unlines $
                [ "Number of log entries: " ++ show (length entries)
                , "Number of unique req ids: " ++ show numUniqueReqIds
                , ""
                , "All the logs:" ] ++ map show entries
            assert $ numUniqueReqIds == n

    it "correct time measures" $ \ctx -> withMaxSuccess 10 $
        property $ \(NumberOfRequests n, RandomIndex i) -> monadicIO $ do
            entries <- liftIO $ do
                let reqs = mconcat
                        [ replicate i (get ctx "/get")
                        , [ get ctx "/long" ]
                        , replicate (n - i) (get ctx "/get")
                        ]
                void $ mapConcurrently id reqs
                waitForServerToComplete
                takeLogs ctx
            let index = mapMaybe captureTime entries
            let numLongReqs = length $ filter (> (200*ms)) index
            monitor $ counterexample $ unlines
                [ "Number of log entries: " ++ show (length entries)
                , "Number of long requests: " ++ show numLongReqs
                ]
            assert $ numLongReqs == 1
  where
    setup :: IO Context
    setup = do
        let listen = ListenOnRandomPort
        tvar <- newTVarIO []
        mvar <- newEmptyMVar
        mngr <- newManager defaultManagerSettings
        handle <- async $ withListeningSocket "127.0.0.1" listen $ \case
            Right (p, socket) -> do
                logSettings <- newApiLoggerSettings
                    <&> obfuscateKeys (\r -> r `seq` ["sensitive"])
                let warpSettings = Warp.defaultSettings
                        & setBeforeMainLoop (putMVar mvar p)
                start logSettings warpSettings (traceInTVarIO tvar) socket
            Left e -> error (show e)
        p <- readMVar mvar
        return $ Context
            { logs = tvar
            , manager = mngr
            , port = p
            , server = handle
            }

    tearDown :: Context -> IO ()
    tearDown = cancel . server

data Context = Context
    { logs :: TVar [ApiLog]
    , manager :: Manager
    , port :: Int
    , server :: Async ()
    }

-- | Read collected logs which were traced with 'traceInTVarIO'.
takeLogs :: Context -> IO [ApiLog]
takeLogs ctx = reverse <$> readTVarIO (logs ctx)

-- | Remove any partial logs which have come from the previous property test
-- run. These can occur because once the requests have completed, the property
-- finishes, but the response side may still be finishing writing its log
-- messages.
skipPrevLogs :: [ApiLog] -> [ApiLog]
skipPrevLogs = dropWhile (notLogRequestStart . logMsg)
  where
    notLogRequestStart LogRequestStart = False
    notLogRequestStart _ = True

-- | Give server time to finish off its request handler - the intention is to
-- ensure that /all/ the response logs are captured before checking assertions.
waitForServerToComplete :: IO ()
waitForServerToComplete = threadDelay 500_000

clearLogs :: Context -> IO ()
clearLogs = atomically . flip writeTVar [] . logs

{-------------------------------------------------------------------------------
                                Test Helpers
-------------------------------------------------------------------------------}

get :: Context -> String -> IO ()
get ctx path = do
    req <- parseRequest ("http://localhost:" <> show (port ctx) <> path)
    void $ httpLbs req (manager ctx)

delete :: Context -> String -> IO ()
delete ctx path = do
    req <- parseRequest ("http://localhost:" <> show (port ctx) <> path)
    void $ httpLbs (req { method = "DELETE" }) (manager ctx)

post :: ToJSON a => Context -> String -> a -> IO ()
post ctx path json = do
    let body = RequestBodyLBS $ Aeson.encode json
    req <- parseRequest ("http://localhost:" <> show (port ctx) <> path)
    void $ httpLbs (req
        { method = "POST"
        , requestBody = body
        , requestHeaders = [(hContentType, "application/json")]
        }) (manager ctx)

postIlled :: Context -> String -> ByteString -> IO ()
postIlled ctx path body = do
    req <- parseRequest ("http://localhost:" <> show (port ctx) <> path)
    void $ httpLbs (req
        { method = "POST"
        , requestBody = RequestBodyBS body
        , requestHeaders = [(hContentType, "application/json")]
        }) (manager ctx)

expectLogs :: Context -> [(Severity, String)] -> IO ()
expectLogs ctx expectations = do
    waitForServerToComplete

    entries <- takeLogs ctx

    when (length entries /= length expectations) $
        fail $ "Expected exactly " <> show (length expectations)
            <> " log entries but got " <> show (length entries) <> ": "
            <> show entries

    forM_ (zip entries expectations) $ \(l, (sev, str)) -> do
        (getSeverityAnnotation l) `shouldBe` sev
        case logMessage l of
            Just txt ->
                T.unpack txt `shouldContain` str
            Nothing ->
                fail $ "Given log object isn't a log message but: " <> show l

-- | Extract the message from a 'LogObject'. Returns 'Nothing' if it's not
-- a log message.
logMessage :: ApiLog -> Maybe Text
logMessage (ApiLog _ theMsg) = case theMsg of
    LogRequestStart -> Just "LogRequestStart"
    LogRequestFinish -> Just "LogRequestFinish"
    LogResponse _ Nothing -> Nothing
    _ -> Just (toText theMsg)

-- | Extract the execution time, in microseconds, from a log request.
-- Returns 'Nothing' if the log line doesn't contain any time indication.
captureTime :: ApiLog -> Maybe Int
captureTime (ApiLog _ theMsg) = case theMsg of
    LogResponse time _ ->
        Just $ round $ toMicro $ realToFrac @_ @Double time
    _ ->
        Nothing
  where
    toMicro = (* 1000000)

-- | Number of microsecond in one millisecond
ms :: Int
ms = 1000

{-------------------------------------------------------------------------------
                            Arbitrary instances
-------------------------------------------------------------------------------}

newtype NumberOfRequests = NumberOfRequests Int deriving Show

instance Arbitrary NumberOfRequests where
    shrink (NumberOfRequests n) =
        fmap NumberOfRequests $ filter (> 0) $ shrink n
    arbitrary = NumberOfRequests <$> choose (1, 100)

newtype RandomIndex = RandomIndex Int deriving Show

-- | Give a random number of request 'n' and a random index 'i' such that:
--
--     0 <= i < n
--
-- This allows for crafting `n+1` requests where `n` requests are "fast" and one
-- is "long", ensuring that the time is correctly measured for that long request,
-- regardless of the interleaving.
instance {-# OVERLAPS #-} Arbitrary (NumberOfRequests, RandomIndex) where
    shrink (NumberOfRequests n, RandomIndex i) =
        [ (NumberOfRequests n', RandomIndex i')
        | n' <- shrink n, n' > 0
        , i' <- shrink i, i' > 0 && i' < n'
        ]
    arbitrary = do
        n <- choose (1, 10)
        i <- choose (0, n - 1)
        return (NumberOfRequests n, RandomIndex i)

{-------------------------------------------------------------------------------
                                mock server
-------------------------------------------------------------------------------}

data MkJson = MkJson
    { field :: String
    , sensitive :: Int
    } deriving Generic
instance ToJSON MkJson
instance FromJSON MkJson

data ResponseJson = ResponseJson
    { status :: String
    , whatever :: Int
    } deriving Generic
instance ToJSON ResponseJson
instance FromJSON ResponseJson

start
    :: ApiLoggerSettings
    -> Warp.Settings
    -> Tracer IO ApiLog
    -> Socket
    -> IO ()
start logSettings warpSettings trace socket = do
    runSettingsSocket warpSettings socket
        $ withApiLogger trace logSettings
        application
  where
    application :: Application
    application = serve (Proxy @Api) handler
    handler :: Server Api
    handler =
        hGet
        :<|> hDelete
        :<|> hPost
        :<|> hJson
        :<|> hLong
        :<|> hErr400
        :<|> hErr500
        :<|> hErr503
      where
        hGet = return 14 :: Handler Int
        hDelete = return NoContent :: Handler NoContent
        hPost _ = return (ResponseJson "ok" 42) :: Handler ResponseJson
        hJson = return "\NUL\NUL\NUL" :: Handler ByteString
        hLong = liftIO (threadDelay $ 200*ms) $> 14 :: Handler Int
        hErr400 = throwError err400 :: Handler ()
        hErr500 = throwError err500 :: Handler ()
        hErr503 = throwError err503 :: Handler ()

type Api =
    "get" :> Get '[JSON] Int
    :<|> "delete" :> DeleteNoContent
    :<|> "post" :> ReqBody '[JSON] MkJson :> PostCreated '[JSON] ResponseJson
    :<|> "not-json" :> Get '[OctetStream] ByteString
    :<|> "long" :> Get '[JSON] Int
    :<|> "error400" :> Get '[JSON] ()
    :<|> "error500" :> Get '[JSON] ()
    :<|> "error503" :> Get '[JSON] ()
