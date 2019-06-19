{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    ( Async, async, cancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Monad
    ( forM_, void, when )
import Control.Monad.STM
    ( atomically )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager
    , Response
    , defaultManagerSettings
    , httpLbs
    , method
    , newManager
    , parseRequest
    )
import Network.Socket
    ( Socket )
import Network.Wai.Handler.Warp
    ( runSettingsSocket, setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLoggerSettings, newApiLoggerSettings, withApiLogger )
import Servant
    ( (:<|>) (..)
    , (:>)
    , Application
    , DeleteNoContent
    , Get
    , JSON
    , NoContent (..)
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

  where
    setup :: IO Context
    setup = do
        let listen = ListenOnRandomPort
        tvar <- newTVarIO []
        mvar <- newEmptyMVar
        mngr <- newManager defaultManagerSettings
        handle <- async $ withListeningSocket listen $ \(p, socket) -> do
            logSettings <- newApiLoggerSettings
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
    handler = hGet :<|> hDelete :<|> hErr400 :<|> hErr500
      where
        hGet = return 14 :: Handler Int
        hDelete = return NoContent :: Handler NoContent
        hErr400 = throwError err400 :: Handler ()
        hErr500 = throwError err500 :: Handler ()

type Api =
    "get" :> Get '[JSON] Int
    :<|> "delete" :> DeleteNoContent '[Any] NoContent
    :<|> "error400" :> Get '[JSON] ()
    :<|> "error500" :> Get '[JSON] ()
