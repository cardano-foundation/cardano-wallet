{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Prelude

import Buildkite.API
    ( Artifact (filename, job_id)
    , Build (branch, jobs, number)
    , GetArtifact
    , Job (finished_at)
    , Time (Time)
    , WithAuthPipeline
    )
import Buildkite.Client
    ( BuildAPI
    , BuildJobsMap
    , Query (..)
    , getArtifacts
    , getArtifactsContent
    , getBuildsOfBranch
    )
import Cardano.Wallet.Benchmarks.Charting
    ( renderHarmonizedHistoryChartSVG
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Result (..)
    )
import Cardano.Wallet.Benchmarks.History
    ( History
    , IndexedSemantic (IndexedSemantic)
    , harmonizeHistory
    , pastDays
    , renderHarmonizedHistoryCsv
    )
import Control.Monad
    ( when
    )
import Data.Csv
    ( decodeByName
    , encodeByName
    )
import Data.Data
    ( Proxy (..)
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Semigroup
    ( First (..)
    )
import Data.Text
    ( Text
    , isSuffixOf
    )
import Data.Time
    ( Day
    , UTCTime (utctDay)
    )
import Network.HTTP.Client
    ( Manager
    , ManagerSettings (managerModifyRequest)
    , Request (shouldStripHeaderOnRedirect)
    , newManager
    )
import Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import Network.HTTP.Media
    ( (//)
    )
import Network.HTTP.Types.Status
    ( status410
    )
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , strOption
    )
import Servant.API
    ( Accept (..)
    , MimeUnrender
    )
import Servant.API.ContentTypes
    ( MimeRender (..)
    , MimeUnrender (..)
    )
import Servant.Client
    ( BaseUrl (..)
    , ClientEnv
    , ClientError (FailureResponse)
    , ClientM
    , ResponseF (..)
    , Scheme (..)
    , client
    , mkClientEnv
    , runClientM
    )
import Streaming
    ( Of (..)
    , Stream
    )
import System.Environment
    ( getEnv
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

data CSV

instance Accept CSV where
    contentType _ = "text" // "csv"

instance Show a => MimeRender CSV a where
    mimeRender _ val = BL8.pack $ show val

instance MimeUnrender CSV BL8.ByteString where
    mimeUnrender _ = Right

organizationSlug :: Text
organizationSlug = "cardano-foundation"

pipelineSlug :: Text
pipelineSlug = "cardano-wallet"

buildkiteDomain :: String
buildkiteDomain = "api.buildkite.com"

buildkitePort :: Int
buildkitePort = 443

withAuthWallet :: String -> WithAuthPipeline a -> a
withAuthWallet apiToken f =
    f (Just $ T.pack apiToken) organizationSlug pipelineSlug

fetchArtifactContent
    :: WithAuthPipeline (Int -> Text -> Text -> ClientM BL8.ByteString)
fetchArtifactContent = client (Proxy :: Proxy (GetArtifact CSV BL8.ByteString))

queryBuildkite ::
    (forall a . HandleClientError a -> ClientM a -> IO (Maybe a))
    -> (forall a . WithAuthPipeline a -> a)
    -> Day -> IO History
queryBuildkite q w d0 = do
    let skip410Q = Query (q skip410) w
        bailoutQ = Query (q bailout) w
    S.foldMap_ Prelude.id
        $ flip
            S.for
            ( \case
                Right h -> S.yield h
                Left e -> error e
            )
        $ flip S.for historyPoints
        $ flip S.for (\(a, j) -> getArtifactsContent
            skip410Q
            fetchArtifactContent j a)
        $ S.chain
            ( \(_, b) ->
                putStrLn
                    $ "Build number: "
                        <> show (number b)
                        <> ", branch: "
                        <> T.unpack (branch b)
            )

        $ S.filter (\(a, _) -> "bench-results.csv" `isSuffixOf` filename a)
        $ S.map (\(b, a) -> (a, b))
        $ flip S.for (getArtifacts bailoutQ)
        $ getReleaseCandidateBuilds bailoutQ d0

mkReleaseCandidateName :: Day -> String
mkReleaseCandidateName d = "release-candidate/v" ++ show d

getReleaseCandidateBuilds :: Query -> Day -> S.Stream (S.Of BuildAPI) IO ()
getReleaseCandidateBuilds q d = S.effect $ do
    ds <- pastDays d
    pure
        $ flip S.for (getBuildsOfBranch q . mkReleaseCandidateName)
        $ S.each ds

getToken :: IO String
getToken = do
    token <- getEnv "BUILDKITE_API_TOKEN"
    pure $ "Bearer " ++ token

data Options = Options
    { _optSinceDate :: Day
    , _outputDir :: FilePath
    }

parseOptDate :: Parser Day
parseOptDate =
    option auto
        $ long "since"
            <> metavar "YYYY-MM-DD"
            <> help "The date to start collecting data from"

parseOptChartsDir :: Parser FilePath
parseOptChartsDir =
    strOption
        $ long "charts-dir"
            <> metavar "DIR"
            <> help "The directory to save the charts to"

optionsParser :: ParserInfo Options
optionsParser =
    info
        (Options <$> parseOptDate <*> parseOptChartsDir)
        ( fullDesc
            <> progDesc "Collect and process benchmark history data."
            <> header "benchmark-history - a tool for benchmark data analysis"
        )

type HandleClientError a = IO (Either ClientError a) -> IO (Maybe a)

main :: IO ()
main = do
    bkToken <- getToken
    Options sinceDay outputDir <- execParser optionsParser
    manager <- newManager $ specialSettings False
    let env = buildkiteEnv manager
        runQuery :: HandleClientError a -> ClientM a -> IO (Maybe a)
        runQuery f action = f $ runClientM action env
    result <- queryBuildkite runQuery (withAuthWallet bkToken) sinceDay
    let eHarmonized = harmonizeHistory result
    case eHarmonized of
        Left rs -> error $ "Failed to harmonize history: " ++ show rs
        Right harmonized -> do
            putStrLn $ "Harmonized history: " <> show harmonized
            let csv = uncurry encodeByName $ renderHarmonizedHistoryCsv harmonized
            BL8.writeFile (outputDir </> "benchmark_history" <.> "csv") csv
            renderHarmonizedHistoryChartSVG outputDir harmonized

bailout :: HandleClientError a
bailout = handle (error . show)

handle :: (ClientError -> IO (Maybe a)) -> HandleClientError a
handle g f = do
    res <- f
    case res of
        Left e -> g e
        Right a -> pure $ Just a

skip410 :: HandleClientError a
skip410 = handle $ \case
    FailureResponse _ (Response s _ _ _)
        | s == status410 -> pure Nothing
    e -> error $ show e

buildkiteEnv :: Manager -> ClientEnv
buildkiteEnv manager =
    mkClientEnv manager
        $ BaseUrl Https buildkiteDomain buildkitePort ""

specialSettings :: Bool -> ManagerSettings
specialSettings logs =
    tlsManagerSettings
        { managerModifyRequest = \req -> do
            let req' =
                    req
                        { shouldStripHeaderOnRedirect =
                            \case
                                "Authorization" -> True
                                _ -> False
                        }
            when logs
                $ putStrLn
                $ "Querying: " ++ show req'
            pure req'
        }

parseResults :: BL8.ByteString -> Either String ([(IndexedSemantic, Result)])
parseResults = fmap (fmap f . zip [0 ..] . toList . snd) . decodeByName
  where
    f (i, (Benchmark s r)) = (IndexedSemantic s i, r)

historyPoints
    :: (BuildJobsMap, Artifact, BL8.ByteString)
    -> Stream (Of (Either String History)) IO ()
historyPoints (b, a, r) =
    let
        rs = parseResults r
        t = finished_at =<< Map.lookup (job_id a) (jobs b)
    in
        case rs of
            Left e -> S.yield $ Left e
            Right rs' -> case t of
                Nothing -> pure ()
                Just (Time t') -> S.yield $ Right $ fold $ do
                    (i, r') <- rs'
                    pure
                        $ MMap.singleton i
                        $ MMap.singleton (utctDay t')
                        $ First r'
