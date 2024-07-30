{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Test.Integration.Framework.Logging
    ( TestsLog (..)
    , withTracers
    , bracketTracer'
    )
where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.BM.Extra
    ( BracketLog
    , bracketTracer
    , trMessageText
    )
import Cardano.BM.Plugin
    ( loadPlugin
    )
import Cardano.BM.Trace
    ( appendName
    )
import Cardano.Wallet.Application.CLI
    ( LogOutput (..)
    , ekgEnabled
    , withLogging
    )
import Cardano.Wallet.Application
    ( Tracers
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Launch.Cluster
    ( ClusterLog
    , clusterEraFromEnv
    , clusterEraToString
    , testLogDirFromEnv
    , testMinSeverityFromEnv
    , walletMinSeverityFromEnv
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , mkRelDirOf
    , toFilePath
    )
import Control.Monad
    ( when
    )
import Control.Tracer
    ( Tracer (..)
    , contramap
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Network.URI
    ( URI
    )
import System.Path
    ( relFile
    , (</>)
    )
import Test.Integration.Framework.Context
    ( PoolGarbageCollectionEvent (..)
    )
import UnliftIO.Exception
    ( SomeException (..)
    , isAsyncException
    )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Data.Text as T

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl URI Text Text Text
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    | MsgPoolGarbageCollectionEvent PoolGarbageCollectionEvent
    | MsgServerError SomeException
    | MsgRewardWalletDelegationFailed Text
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl walletUrl ekgUrl prometheusUrl smashUrl ->
            T.unlines
                [ "Wallet url: " <> T.pack (show walletUrl)
                , "EKG url: " <> ekgUrl
                , "Prometheus url: " <> prometheusUrl
                , "SMASH url: " <> smashUrl
                ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg
        MsgPoolGarbageCollectionEvent e ->
            mconcat
                [ "Intercepted pool garbage collection event for epoch "
                , toText (poolGarbageCollectionEpochNo e)
                , ". "
                , case poolGarbageCollectionCertificates e of
                    [] -> "No pools were removed from the database."
                    ps ->
                        mconcat
                            [ "The following pools were removed from the database: "
                            , T.unwords (T.pack . show <$> ps)
                            ]
                ]
        MsgServerError e
            | isAsyncException (SomeException e)
                -> "Server thread cancelled: " <> T.pack (show e)
            | otherwise -> T.pack (show e)
        MsgRewardWalletDelegationFailed msg ->
            T.unlines
                [ "Reward wallet delegation failed."
                , "Details: " <> toText msg
                ]

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl{} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
        MsgPoolGarbageCollectionEvent _ -> Info
        MsgServerError e
            | isAsyncException e -> Notice
            | otherwise -> Warning
        MsgRewardWalletDelegationFailed{} -> Error

withTracers
    :: DirOf "cluster"
    -> ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers testDir action = do
    let getLogOutputs getMinSev name = do
            minSev <- getMinSev
            eraStr <- clusterEraToString <$> clusterEraFromEnv
            mLogDir <- testLogDirFromEnv $ Just $ mkRelDirOf eraStr
            let logDir = case mLogDir of
                    Just d -> absDirOf d
                    Nothing -> absDirOf testDir -- re-purpose the "cluster" dir
            pure
                [ LogToFile (toFilePath $ logDir </> name) (min minSev Info)
                , LogToStdStreams minSev
                ]

    walletLogOutputs <- getLogOutputs walletMinSeverityFromEnv
        $ relFile "wallet.log"
    testLogOutputs <- getLogOutputs testMinSeverityFromEnv $ relFile "test.log"

    withLogging walletLogOutputs $ \(sb, (cfg, walTr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg walTr sb >>= loadPlugin sb)
        withLogging testLogOutputs $ \(_, (_, testTr)) -> do
            let trTests = appendName "integration" testTr
            let tracers = setupTracers (tracerSeverities (Just Debug)) walTr
            action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)
