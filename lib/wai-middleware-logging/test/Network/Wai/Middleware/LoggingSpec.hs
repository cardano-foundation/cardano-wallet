{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Middleware.LoggingSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..)
    )
import Cardano.BM.Trace
    ( traceInTVarIO
    )
import Control.Monad
    ( forM_
    , void
    , when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Tracer
    ( Tracer
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( ($>)
    , (<&>)
    )
import Data.List
    ( isInfixOf
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Streaming.Network
    ( HostPreference
    , bindPortTCP
    , bindRandomPortTCP
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( toText
    )
import GHC.Generics
    ( Generic
    )
import Network.HTTP.Client
    ( Manager
    , Request
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
    ( hContentType
    )
import Network.Socket
    ( Socket
    , close
    )
import Network.Wai.Handler.Warp
    ( Port
    , runSettingsSocket
    , setBeforeMainLoop
    )
import Network.Wai.Middleware.Logging
    ( ApiLog (..)
    , ApiLoggerSettings
    , HandlerLog (..)
    , newApiLoggerSettings
    , obfuscateKeys
    , withApiLogger
    )
import Servant
    ( Application
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
    , (:<|>) (..)
    , (:>)
    )
import Servant.Server
    ( Handler
    )
import System.IO.Error
    ( ioeGetErrorType
    , isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    , isUserError
    )
import Test.Hspec
    ( Spec
    , after
    , before
    , describe
    , it
    , shouldBe
    , shouldContain
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , choose
    )
import UnliftIO
    ( IOException
    , bracket
    , tryJust
    )
import UnliftIO.Async
    ( Async
    , async
    , cancel
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( onException
    , throwString
    )
import UnliftIO.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , tryPutMVar
    )
import UnliftIO.STM
    ( TVar
    , newTVarIO
    , readTVarIO
    )

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "Logging Middleware" $ do
    before setup $ after tearDown $ do
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
                , (Debug, "{\"field\":\"patate\",\"sensitive\":\"*****\"}")
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
  where
    setup :: IO Context
    setup = do
        tvar <- newTVarIO []
        let tr = traceInTVarIO tvar
        mvar <- newEmptyMVar
        task <- async
            (run tr (putMVar mvar) `onException` tryPutMVar mvar (Left Nothing))
        Context tvar
            <$> newManager defaultManagerSettings
            <*> (readMVar mvar >>= either bomb (pure . id))
            <*> pure task

    run tr cb = withListeningSocket "127.0.0.1" ListenOnRandomPort $ \case
        Right (p, socket) -> do
            logSettings <- newApiLoggerSettings
                <&> obfuscateKeys (`seq` ["sensitive"])
            let warpSettings = Warp.defaultSettings &
                    setBeforeMainLoop (cb (Right p))
            start logSettings warpSettings tr socket
        Left e -> cb (Left (Just e))

    bomb err = throwString $ case err of
        Just e -> "Error setting up warp server: " ++ show e
        Nothing -> "Some error when starting the warp server"

    tearDown :: Context -> IO ()
    tearDown = cancel . server

data Context = Context
    { logs :: TVar [ApiLog]
    , manager :: Manager
    , port :: Port
    , server :: Async ()
    }

-- | Read collected logs which were traced with 'traceInTVarIO'.
takeLogs :: Context -> IO [ApiLog]
takeLogs ctx = reverse <$> readTVarIO (logs ctx)

-- | Give server time to finish off its request handler - the intention is to
-- ensure that /all/ the response logs are captured before checking assertions.
waitForServerToComplete :: IO ()
waitForServerToComplete = threadDelay 500_000

{-------------------------------------------------------------------------------
                                Test Helpers
-------------------------------------------------------------------------------}

baseRequest :: Context -> String -> IO Request
baseRequest ctx path = parseRequest $
    "http://localhost:" <> show (port ctx) <> path

get :: Context -> String -> IO ()
get ctx path = do
    req <- baseRequest ctx path
    void $ httpLbs req (manager ctx)

delete :: Context -> String -> IO ()
delete ctx path = do
    req <- baseRequest ctx path
    void $ httpLbs (req { method = "DELETE" }) (manager ctx)

post :: ToJSON a => Context -> String -> a -> IO ()
post ctx path json = do
    let body = RequestBodyLBS $ Aeson.encode json
    req <- baseRequest ctx path
    void $ httpLbs (req
        { method = "POST"
        , requestBody = body
        , requestHeaders = [(hContentType, "application/json")]
        }) (manager ctx)

postIlled :: Context -> String -> ByteString -> IO ()
postIlled ctx path body = do
    req <- baseRequest ctx path
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
    LogResponse _ _ Nothing -> Nothing
    _ -> Just (toText theMsg)

-- | Number of microsecond in one millisecond
ms :: Int
ms = 1_000

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

-- | How the server should listen for incoming requests.
data Listen
    = ListenOnPort Port
      -- ^ Listen on given TCP port
    | ListenOnRandomPort
      -- ^ Listen on an unused TCP port, selected at random
    deriving (Show, Eq)

withListeningSocket
    :: HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ Whether to listen on a given port, or random port.
    -> (Either ListenError (Port, Socket) -> IO a)
    -- ^ Action to run with listening socket.
    -> IO a
withListeningSocket hostPreference portOpt = bracket acquire release
  where
    acquire = tryJust handleErr bindAndListen
    -- Note: These Data.Streaming.Network functions also listen on the socket,
    -- even though their name just says "bind".
    bindAndListen = case portOpt of
        ListenOnPort p -> (p,) <$> bindPortTCP p hostPreference
        ListenOnRandomPort -> bindRandomPortTCP hostPreference
    release (Right (_, socket)) = liftIO $ close socket
    release (Left _) = pure ()
    handleErr = ioToListenError hostPreference portOpt

data ListenError
    = ListenErrorAddressAlreadyInUse (Maybe Port)
    | ListenErrorOperationNotPermitted
    | ListenErrorHostDoesNotExist HostPreference
    | ListenErrorInvalidAddress HostPreference
    deriving (Show, Eq)

ioToListenError :: HostPreference -> Listen -> IOException -> Maybe ListenError
ioToListenError hostPreference portOpt e
    -- A socket is already listening on that address and port
    | isAlreadyInUseError e =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    -- Usually caused by trying to listen on a privileged port
    | isPermissionError e =
        Just ListenErrorOperationNotPermitted
    -- Bad hostname -- Linux and Darwin
    | isDoesNotExistError e =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Bad hostname -- Windows
    -- WSAHOST_NOT_FOUND, WSATRY_AGAIN, or bind: WSAEOPNOTSUPP
    | isUserError e && any hasDescription ["11001", "11002", "10045"] =
        Just (ListenErrorHostDoesNotExist hostPreference)
    -- Address is valid, but can't be used for listening -- Linux
    | show (ioeGetErrorType e) == "invalid argument" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Darwin
    | show (ioeGetErrorType e) == "unsupported operation" =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Address is valid, but can't be used for listening -- Windows
    | isOtherError e && any hasDescription ["WSAEINVAL", "WSAEADDRNOTAVAIL"] =
        Just (ListenErrorInvalidAddress hostPreference)
    -- Listening on an unavailable or privileged port -- Windows
    | isOtherError e && hasDescription "WSAEACCESS" =
        Just (ListenErrorAddressAlreadyInUse (listenPort portOpt))
    | otherwise =
        Nothing
  where
    listenPort (ListenOnPort p) = Just p
    listenPort ListenOnRandomPort = Nothing

    isOtherError ex = show (ioeGetErrorType ex) == "failed"
    hasDescription text = text `isInfixOf` show e
