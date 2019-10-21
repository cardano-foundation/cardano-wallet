{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Middleware.LoggingSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LOMeta (..), LogObject (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, traceInTVarIO )
import Cardano.Wallet.Api
    ( Any )
import Cardano.Wallet.Api.Server
    ( Listen (..), withListeningSocket )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel, mapConcurrently )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Monad
    ( forM_, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.STM
    ( atomically )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Maybe
    ( catMaybes )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
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
    ( ApiLoggerSettings, newApiLoggerSettings, obfuscateKeys, withApiLogger )
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
    , serve
    , throwError
    )
import Servant.Server
    ( Handler )
import Test.Hspec
    ( Spec, after, afterAll, beforeAll, describe, it, shouldBe, shouldContain )
import Test.QuickCheck
    ( Arbitrary (..), choose, property )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "Logging Middleware"
    $ beforeAll setup $ after clearLogs $ afterAll tearDown $ do
    it "GET, 200, no query" $ \ctx -> do
        get ctx "/get"
        expectLogs ctx
            [ (Info, "[GET] /get")
            , (Info, "200 OK")
            , (Debug, "14")
            ]

    it "GET, 200, with query" $ \ctx -> do
        get ctx "/get?query=patate"
        expectLogs ctx
            [ (Info, "[GET] /get?query=patate")
            , (Info, "200 OK")
            , (Debug, "14")
            ]

    it "GET, 200, not json" $ \ctx -> do
        get ctx "/not-json"
        expectLogs ctx
            [ (Info, "[GET] /not-json")
            , (Info, "200 OK")
            , (Debug, "\NUL\NUL\NUL")
            ]

    it "POST, 201, with sensitive fields" $ \ctx -> do
        post ctx "/post" (MkJson { field = "patate", sensitive = 14 })
        expectLogs ctx
            [ (Info, "[POST] /post")
            , (Debug, "{\"sensitive\":\"*****\",\"field\":\"patate\"}")
            , (Info, "201 Created")
            , (Debug, "{\"status\":\"ok\",\"whatever\":42}")
            ]

    it "POST, 400, invalid payload (not json)" $ \ctx -> do
        postIlled ctx "/post" "\NUL\NUL\NUL"
        expectLogs ctx
            [ (Info, "[POST] /post")
            , (Debug, "Invalid payload: not JSON")
            , (Info, "400 Bad Request")
            , (Debug, "Failed reading: not a valid json value")
            ]

    it "DELETE, 202, no query" $ \ctx -> do
        delete ctx "/delete"
        expectLogs ctx
            [ (Info, "[DELETE] /delete")
            , (Info, "204 No Content")
            ]

    it "GET, 400" $ \ctx -> do
        get ctx "/error400"
        expectLogs ctx
            [ (Info, "[GET] /error400")
            , (Info, "400 Bad Request")
            ]

    it "GET, 500" $ \ctx -> do
        get ctx "/error500"
        expectLogs ctx
            [ (Info, "[GET] /error500")
            , (Error, "500 Internal Server Error")
            ]

    it "different request ids" $ \ctx -> property $ \(NumberOfRequests n) ->
        monadicIO $ liftIO $ do
            void $ mapConcurrently (const (get ctx "/get")) (replicate n ())
            entries <- readTVarIO (logs ctx)
            let index = Map.fromList [ (loName l, l) | l <- entries ]
            Map.size index `shouldBe` n

    it "correct time measures" $ \ctx -> property $ \(nReq, ix) ->
        monadicIO $ liftIO $ do
            let (NumberOfRequests n, RandomIndex i) = (nReq, ix)
            let reqs = mconcat
                    [ replicate i (get ctx "/get")
                    , [ get ctx "/long" ]
                    , replicate (n - i) (get ctx "/get")
                    ]
            void $ mapConcurrently id reqs
            entries <- readTVarIO (logs ctx)
            let index = Map.fromList
                    $ catMaybes [ (loName l,) <$> captureTime l | l <- entries ]
            Map.size (Map.filter (> (200*ms)) index) `shouldBe` 1
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

    clearLogs :: Context -> IO ()
    clearLogs = atomically . flip writeTVar [] . logs

    tearDown :: Context -> IO ()
    tearDown = cancel . server

data Context = Context
    { logs :: TVar [LogObject Text]
    , manager :: Manager
    , port :: Int
    , server :: Async ()
    }

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
    entries <- reverse <$> readTVarIO (logs ctx)

    when (length entries /= length expectations) $
        fail $ "Expected exactly " <> show (length expectations)
            <> " log entries but got " <> show (length entries) <> ": "
            <> show entries

    forM_ (zip entries expectations) $ \(l, (sev, str)) -> do
        (severity . loMeta $ l) `shouldBe` sev
        case logMessage l of
            Just txt ->
                T.unpack txt `shouldContain` str
            Nothing ->
                fail $ "Given log object isn't a log message but: " <> show l

-- | Extract the message from a 'LogObject'. Returns 'Nothing' if it's not
-- a log message.
logMessage :: LogObject Text -> Maybe Text
logMessage l = case loContent l of
    LogMessage txt ->
        Just txt
    _ ->
        Nothing

-- | Extract the execution time, in microseconds, from a log request.
-- Returns 'Nothing' if the log line doesn't contain any time indication.
captureTime :: LogObject Text -> Maybe Int
captureTime l = case loContent l of
    LogMessage txt -> let str = T.unpack txt in case words str of
        ["200", "OK", "in", time] ->
            Just $ round $ toMicro $ read @Double $ init time
        _ ->
            Nothing
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
    -> Trace IO Text
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
      where
        hGet = return 14 :: Handler Int
        hDelete = return NoContent :: Handler NoContent
        hPost _ = return (ResponseJson "ok" 42) :: Handler ResponseJson
        hJson = return "\NUL\NUL\NUL" :: Handler ByteString
        hLong = liftIO (threadDelay $ 200*ms) $> 14 :: Handler Int
        hErr400 = throwError err400 :: Handler ()
        hErr500 = throwError err500 :: Handler ()

type Api =
    "get" :> Get '[JSON] Int
    :<|> "delete" :> DeleteNoContent '[Any] NoContent
    :<|> "post" :> ReqBody '[JSON] MkJson :> PostCreated '[JSON] ResponseJson
    :<|> "not-json" :> Get '[OctetStream] ByteString
    :<|> "long" :> Get '[JSON] Int
    :<|> "error400" :> Get '[JSON] ()
    :<|> "error500" :> Get '[JSON] ()
