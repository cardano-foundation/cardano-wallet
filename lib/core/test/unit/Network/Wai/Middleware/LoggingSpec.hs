{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Concurrent.Async
    ( Async, async, cancel, mapConcurrently )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Monad
    ( forM_, void, when )
import Control.Monad.STM
    ( atomically )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
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

    it "different request ids" $ \ctx -> do
        let n = 100
        void $ mapConcurrently (const (get ctx "/get")) (replicate n ())
        entries <- readTVarIO (logs ctx)
        let index = Map.fromList [ (loName l, l) | l <- entries ]
        Map.size index `shouldBe` n
  where
    setup :: IO Context
    setup = do
        let listen = ListenOnRandomPort
        tvar <- newTVarIO []
        mvar <- newEmptyMVar
        mngr <- newManager defaultManagerSettings
        handle <- async $ withListeningSocket listen $ \(p, socket) -> do
            logSettings <- newApiLoggerSettings
                <&> obfuscateKeys (\r -> r `seq` ["sensitive"])
            let warpSettings = Warp.defaultSettings
                    & setBeforeMainLoop (putMVar mvar p)
            start logSettings warpSettings (traceInTVarIO tvar) socket
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
        case loContent l of
            LogMessage txt ->
                T.unpack txt `shouldContain` str
            _ ->
                fail $ "Given log object isn't a log message but: " <> show l

{-------------------------------------------------------------------------------
                                mock server
-------------------------------------------------------------------------------}

data MkJson = MkJson
    { field :: String
    , sensitive :: Int
    } deriving Generic

instance ToJSON MkJson where
    toJSON = genericToJSON Aeson.defaultOptions
instance FromJSON MkJson where
    parseJSON = genericParseJSON Aeson.defaultOptions

data ResponseJson = ResponseJson
    { status :: String
    , whatever :: Int
    } deriving Generic

instance ToJSON ResponseJson where
    toJSON = genericToJSON Aeson.defaultOptions
instance FromJSON ResponseJson where
    parseJSON = genericParseJSON Aeson.defaultOptions

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
    handler = hGet :<|> hDelete :<|> hPost :<|> hJson :<|> hErr400 :<|> hErr500
      where
        hGet = return 14 :: Handler Int
        hDelete = return NoContent :: Handler NoContent
        hPost _ = return (ResponseJson "ok" 42) :: Handler ResponseJson
        hJson = return "\NUL\NUL\NUL" :: Handler ByteString
        hErr400 = throwError err400 :: Handler ()
        hErr500 = throwError err500 :: Handler ()

type Api =
    "get" :> Get '[JSON] Int
    :<|> "delete" :> DeleteNoContent '[Any] NoContent
    :<|> "post" :> ReqBody '[JSON] MkJson :> PostCreated '[JSON] ResponseJson
    :<|> "not-json" :> Get '[OctetStream] ByteString
    :<|> "error400" :> Get '[JSON] ()
    :<|> "error500" :> Get '[JSON] ()
