{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Options.Applicative
import Prelude hiding
    ( FilePath )
import Turtle hiding
    ( opt, option )

import Control.Concurrent.Async
    ( race )
import Control.Exception
    ( IOException, catch )
import Control.Monad
    ( filterM, forM_ )
import Control.Monad.Extra
    ( whenM )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Data.ByteString
    ( ByteString )
import Data.List.Extra
    ( dropSuffix )
import Data.Maybe
    ( catMaybes, maybeToList )
import Safe
    ( headMay, readMay )
import System.Exit
    ( exitWith )

import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified Turtle.Bytes as TB

data RebuildOpts = RebuildOpts
    { optBuildDirectory :: Maybe FilePath
    , optCacheDirectory :: Maybe FilePath
    } deriving (Show)

data Command = Build | CleanupCache | PurgeCache deriving (Show)

rebuildOpts :: Parser RebuildOpts
rebuildOpts = RebuildOpts <$> optional buildDir <*> optional cacheName
  where
    buildDir = option (FP.decodeString <$> str) (long "build-dir" <> metavar "DIR" <> help "Copy sources to directory before building")
    cacheName = option (FP.decodeString <$> str) (long "cache-dir" <> metavar "DIR" <> help "Location of project's cache")

parseOpts :: IO (RebuildOpts, Command)
parseOpts = execParser opts
  where
    opts = info (cmdOpts <**> helper)
           ( fullDesc <> progDesc "Build cardano-wallet with stack in Buildkite" )
    cmdOpts = (,) <$> rebuildOpts <*> (cmd <|> pure Build)
    cmd = subparser
          ( command "build" (info (pure Build) idm)
            <> command "cleanup-cache" (info (pure CleanupCache) idm)
            <> command "purge-cache" (info (pure PurgeCache) idm) )

main :: IO ()
main = do
  (RebuildOpts{..}, cmd) <- parseOpts
  bk <- getBuildkiteEnv
  cacheConfig <- getCacheConfig bk optCacheDirectory
  case cmd of
      Build -> do
          doMaybe setupBuildDirectory optBuildDirectory
          cacheGetStep cacheConfig
          buildResult <- buildStep Nothing
          cachePutStep cacheConfig
          -- uploadCoverageStep
          weederStep
          exitWith buildResult
      CleanupCache -> cleanupCacheStep cacheConfig optBuildDirectory
      PurgeCache -> purgeCacheStep cacheConfig optBuildDirectory

data BuildOpt = Opt | Fast deriving (Show, Eq)

buildStep :: Maybe [Text] -> IO ExitCode
buildStep testArgs =
    echo "--- Build LTS Snapshot" *> buildSnapshot .&&.
    echo "--- Build dependencies" *> buildDeps .&&.
    -- echo "+++ Build and test" *> buildAndTest
    echo "+++ Build" *> build .&&.
    echo "+++ Test" *> timeout 30 test
  where
    cfg = ["--dump-logs", "--color", "always"]
    buildArgs =
        [ "--bench"
        , "--no-run-benchmarks"
        , "--haddock"
        , "--haddock-internal"
        , "--no-haddock-deps"
        ]
    testArgs' = maybe [] ("--ta" :) testArgs

    stackBuild opt args = run "stack" $ concat [cfg, ["build"], fastOpt opt, args]
    buildSnapshot = stackBuild Opt  $ buildArgs ++ ["--only-snapshot"]
    buildDeps     = stackBuild Opt  $ buildArgs ++ ["--only-dependencies"]
    buildAndTest  = stackBuild Fast $ ["--test"] ++ buildArgs ++ testArgs'
    build         = stackBuild Fast $ ["--test", "--no-run-tests"] ++ buildArgs
    test          = stackBuild Fast $ ["--test"] ++ testArgs'

    fastOpt Opt = []
    fastOpt Fast = ["--fast"]

-- Stack with caching needs a build directory that is the same across
-- all BuildKite agents. The build directory option can be used to
-- ensure this is the case.
setupBuildDirectory :: FilePath -> IO ()
setupBuildDirectory buildDir = do
    removeDirectory buildDir
    src <- pwd
    printf ("Copying source tree "%fp%" -> "%fp%"\n") src buildDir
    cptree src buildDir
    cd buildDir

----------------------------------------------------------------------------
-- Buildkite
-- https://buildkite.com/docs/pipelines/environment-variables

-- | A selection of relevant pipeline and build information from Buildkite.
data BuildkiteEnv = BuildkiteEnv
    { bkBuildNum      :: Int
    -- ^ The Buildkite build number.
    , bkPipeline      :: Text
    -- ^ The pipeline slug on Buildkite as used in URLs.
    , bkBranch        :: Text
    -- ^ The branch being built.
    , bkBaseBranch    :: Maybe Text
    -- ^ The base branch that the pull request is targeting, if this
    -- build is for a pull request.
    , bkDefaultBranch :: Text
    -- ^ The default branch for the pipeline (e.g. master).
    , bkTag           :: Maybe Text
    -- ^ The name of the tag being built, if this build was triggered from a tag.
    } deriving (Show)

-- | Fetch build parameters from the environment.
getBuildkiteEnv :: IO (Maybe BuildkiteEnv)
getBuildkiteEnv = runMaybeT $ do
    bkBuildNum      <- MaybeT $ needRead "BUILDKITE_BUILD_NUMBER"
    bkPipeline      <- MaybeT $ need "BUILDKITE_PIPELINE_SLUG"
    bkBranch        <- MaybeT $ need "BUILDKITE_BRANCH"
    bkBaseBranch    <- lift   $ want "BUILDKITE_PULL_REQUEST_BASE_BRANCH"
    bkDefaultBranch <- MaybeT $ need "BUILDKITE_PIPELINE_DEFAULT_BRANCH"
    bkTag           <- lift   $ want "BUILDKITE_TAG"
    pure BuildkiteEnv {..}

----------------------------------------------------------------------------
-- Weeder - uses contents of .stack-work to determine unused dependencies

weederStep :: IO ()
weederStep = do
    echo "--- Weeder"
    procs "weeder" [] empty

----------------------------------------------------------------------------
-- Stack Haskell Program Coverage and upload to Coveralls

uploadCoverageStep :: IO ()
uploadCoverageStep = do
    echo "--- Upload Test Coverage"
    sh $ do
        -- Ignore modules that are full of Template Haskell auto-generated code
        pushd "lib/core"
        localMixDir <- pure ".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/hpc/" -- fixme: find it
        void $ proc "sed" ["-i", "s/.*hpc\\/\\(.*\\).mix/module \"\\1\" {}/", "overlay.hpc"] empty
        let ignoredTix = "Cardano.Wallet.DB.Sqlite.TH.tix"
        inproc "hpc" ["overlay", "--hpcdir", localMixDir, "overlay.hpc"] empty &
            output ignoredTix
        void $ proc "sed" ["-i", "s/0,/1,/g", format fp ignoredTix] empty
    void $
        (proc "stack" ["hpc", "report", "**/*.tix"] empty) .&&.
        (proc "shc" ["combined", "custom"] empty)

----------------------------------------------------------------------------
-- Stack root and .stack-work caching.
--
-- This will only operate when the @STACK_ROOT@ environment variable is set.
--
-- It also needs to be running under Buildkite with a project cache location
-- supplied.

-- | Information required for caching the stack root and @.stack-work@
-- directories.
data CICacheConfig = CICacheConfig
    { ccCacheDir  :: FilePath
    -- ^ Per-project directory to store cache files.
    , ccStackRoot :: FilePath
    -- ^ Absolute location of the @.stack@ directory.
    , ccBranches  :: [Text]
    -- ^ A list of branches to source caches from. The branches will be tried in
    -- order until one is found. When saving caches, the first branch in the list
    -- is used.
    } deriving (Show)

-- | Sets up the 'CICacheConfig' info, or provides a reason why caching can't be
-- done.
getCacheConfig :: Maybe BuildkiteEnv -> Maybe FilePath -> IO (Either Text CICacheConfig)
getCacheConfig Nothing _ = pure (Left "BUILDKITE_* environment variables are not set")
getCacheConfig _ Nothing = pure (Left "--cache-dir argument was not provided")
getCacheConfig (Just bk) (Just ccCacheDir) =
    (fmap FP.fromText <$> need "STACK_ROOT") >>= \case
        Just ccStackRoot -> pure (Right CICacheConfig{ccBranches=cacheBranches bk,..})
        Nothing -> pure (Left "STACK_ROOT environment variable is not set")

-- | Create the list of branches to source caches from.
--   1. Build branch;
--   2. PR base branch;
--   3. Repo default branch.
cacheBranches :: BuildkiteEnv -> [Text]
cacheBranches BuildkiteEnv{..} = [bkBranch] ++ maybeToList bkBaseBranch ++ [bkDefaultBranch]

cacheGetStep :: Either Text CICacheConfig -> IO ()
cacheGetStep cacheConfig = do
    echo "--- CI Cache Restore"
    case cacheConfig of
        Right cfg -> restoreCICache cfg `catch`
            \(ex :: IOException) ->
                eprintf ("Failed to download CI cache: "%w%"\nContinuing anyway...\n") ex
        Left ex -> eprintf ("Not using CI cache because "%s%"\n") ex

cachePutStep :: Either Text CICacheConfig -> IO ()
cachePutStep cacheConfig = do
    echo "--- CI Cache Save"
    case cacheConfig of
        Right cfg -> saveCICache cfg `catch`
            \(ex :: IOException) ->
                eprintf ("Failed to upload CI cache: "%w%"\n") ex
        Left _ -> printf "CI cache not configured.\n"

getCacheArchive :: MonadIO io => CICacheConfig -> FilePath -> io (Maybe FilePath)
getCacheArchive CICacheConfig{..} ext = do
    let caches = catMaybes $ map (getCacheName ccCacheDir) ccBranches
    headMay <$> filterM testfile (map (</> ext) caches)

-- | The cache directory for a given branch name
getCacheName :: FilePath -> Text -> Maybe FilePath
getCacheName base branch
    | ".." `T.isInfixOf` branch = Nothing
    | otherwise = Just (base </> FP.fromText branch)

putCacheName :: CICacheConfig -> FilePath -> Maybe FilePath
putCacheName CICacheConfig{..} ext = (</> ext) <$> getCacheName ccCacheDir (head ccBranches)

restoreCICache :: CICacheConfig -> IO ()
restoreCICache cfg = do
    restoreStackRoot cfg
    restoreStackWork cfg

saveCICache :: CICacheConfig -> IO ()
saveCICache cfg = do
    saveStackRoot cfg
    saveStackWork cfg

stackRootCache :: FilePath
stackRootCache = "stack-root.tar.lz4"

stackWorkCache :: FilePath
stackWorkCache = "stack-work.tar.lz4"

restoreStackRoot :: CICacheConfig -> IO ()
restoreStackRoot cfg@CICacheConfig{..} = restoreZippedCache stackRootCache cfg $ \tar -> do
    whenM (testpath ccStackRoot) $ rmtree ccStackRoot
    mktree ccStackRoot
    TB.procs "tar" ["-C", "/", "-x"] tar

restoreStackWork :: CICacheConfig -> IO ()
restoreStackWork cfg = restoreZippedCache stackWorkCache cfg (TB.procs "tar" ["-x"])

restoreZippedCache :: FilePath -> CICacheConfig -> (Shell ByteString -> IO ()) -> IO ()
restoreZippedCache ext cfg act = getCacheArchive cfg ext >>= \case
    Just tarfile -> do
        size <- du tarfile
        printf ("Restoring cache "%fp%" ("%sz%") ... ") tarfile size
        act $ TB.inproc "lz4cat" ["-d"] (TB.input tarfile)
        printf "done.\n"
    Nothing -> printf ("No "%fp%" cache found.\n") ext

saveStackRoot :: CICacheConfig -> IO ()
saveStackRoot cfg@CICacheConfig{..} = saveZippedCache stackRootCache cfg tar
  where
    tar = TB.inproc "tar" ["-C", "/", "-c", format fp ccStackRoot] empty

saveStackWork :: CICacheConfig -> IO ()
saveStackWork cfg = saveZippedCache stackWorkCache cfg tar
  where
    nullTerminate = (<> "\0") . FP.encode
    dirs = nullTerminate <$> find (ends ".stack-work") "."
    tar = TB.inproc "tar" ["--null", "-T", "-", "-c"] dirs

saveZippedCache :: FilePath -> CICacheConfig -> Shell ByteString -> IO ()
saveZippedCache ext cfg@CICacheConfig{..} tar = case putCacheName cfg ext of
    Just tarfile -> sh $ do
        printf ("Saving cache "%fp%" ... ") tarfile
        mktree (directory tarfile)
        tmp <- using (mktempfile ccCacheDir (format fp ext))
        TB.output tmp $ TB.inproc "lz4cat" ["-z"] tar
        mv tmp tarfile
        du tarfile >>= printf ("wrote "%sz%".\n")
    Nothing -> printf ("Could not determine "%fp%" cache name.\n") ext

cleanupCacheStep :: Either Text CICacheConfig -> Maybe FilePath -> IO ()
cleanupCacheStep cacheConfig buildDir = do
    echo "--- Cleaning up CI cache"
    case cacheConfig of
        Right CICacheConfig{..} -> do
            getBranches >>= cleanupCache ccCacheDir
            -- Remove the stack root left by the previous build.
            removeDirectory ccStackRoot
            -- Remove the build directory left by the previous build.
            doMaybe removeDirectory buildDir
        Left ex -> eprintf ("Not cleaning up CI cache because: "%s%"\n") ex

purgeCacheStep :: Either Text CICacheConfig -> Maybe FilePath -> IO ()
purgeCacheStep cacheConfig buildDir = do
    echo "--- Deleting all CI caches"
    case cacheConfig of
        Right CICacheConfig{..} -> do
            removeDirectory ccCacheDir
            removeDirectory ccStackRoot
            doMaybe removeDirectory buildDir
        Left ex -> eprintf ("Not purging CI cache because: "%s%"\n") ex

-- | Remove all files and directories that do not belong to an active branch cache.
cleanupCache :: FilePath -> [Text] -> IO ()
cleanupCache cacheDir activeBranches = do
    let branchCaches = catMaybes . map (getCacheName cacheDir) $ activeBranches
    files <- fold (lstree cacheDir) (Fold.revList)
    forM_ files $ \cf -> do
        st <- stat cf
        if isDirectory st
            then unless (cf `elem` branchCaches) $ do
                printf ("Removing "%fp%".\n") cf
                rmdir cf
            else unless (directoryWithoutSlash cf `elem` branchCaches) $ do
                printf ("Removing "%fp%".\n") cf
                rm cf

removeDirectory :: FilePath -> IO ()
removeDirectory dir = whenM (testpath dir) $ do
    printf ("Removing directory "%fp%".\n") dir
    rmtree dir

-- | Ask the origin git remote for its list of branches.
getBranches :: MonadIO io => io [Text]
getBranches = T.lines <$> strict (sed branchPat (grep branchPat git))
  where
    remote = "origin"
    git = inproc "git" ["ls-remote", "--heads", remote] empty
    branchPat = plus alphaNum *> spaces1 *> "refs/heads/" *> plus anyChar

----------------------------------------------------------------------------
-- Utils

needRead :: (MonadIO io, Read a) => Text -> io (Maybe a)
needRead v = (>>= readMay) . fmap T.unpack <$> need v

want :: MonadIO io => Text -> io (Maybe Text)
want = fmap (>>= nullToNothing) . need
  where
    nullToNothing "" = Nothing
    nullToNothing a = Just a

doMaybe :: Monad m => (a -> m ()) -> Maybe a -> m ()
doMaybe = maybe (pure ())

run :: Text -> [Text] -> IO ExitCode
run cmd args = do
    printf (s % " " % s % "\n") cmd (T.unwords args)
    res <- proc cmd args empty
    case res of
        ExitSuccess      -> pure ()
        ExitFailure code -> eprintf
            ("error: Command exited with code "%d%"!\nContinuing...\n")
            code
    pure res

directoryWithoutSlash :: FilePath -> FilePath
directoryWithoutSlash = decodeString . dropSuffix "/" . encodeString . directory

-- | Run an action, but cancel it if it doesn't complete within the given number
-- of minutes.
timeout :: Int -> IO ExitCode -> IO ExitCode
timeout mins act = race (sleep (fromIntegral mins * 60)) act >>= \case
    Right r -> pure r
    Left () -> do
        eprintf ("\nTimed out after "%d%" minutes.\n") mins
        pure (ExitFailure 124)
