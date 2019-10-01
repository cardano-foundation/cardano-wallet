{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Script for building cardano-wallet with Stack under Buildkite.
--
-- You can almost exactly reproduce the build by copying the commands from
-- pipeline.yml. Thes will have exact same versions of all build tools, etc.
--
-- To see what would be built, without actually running the build,
-- use the --dry-run option.
--
-- To work on this script under GHCi, with Haskell dependencies provided, run:
--     nix-shell .buildkite --run "ghci .buildkite/rebuild.hs"

import Prelude hiding
    ( FilePath )

import Options.Applicative
import Turtle hiding
    ( arg, match, opt, option, skip )

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
import Data.Char
    ( isSpace )
import Data.Maybe
    ( mapMaybe, maybeToList )
import Safe
    ( headMay, readMay )
import System.Exit
    ( exitWith )

import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified Turtle.Bytes as TB

main :: IO ()
main = do
    (RebuildOpts{..}, cmd) <- parseOpts
    bk <- getBuildkiteEnv
    cacheConfig <- getCacheConfig bk optCacheDirectory
    case cmd of
        Build -> do
            doMaybe (setupBuildDirectory optDryRun) optBuildDirectory
            whenRun optDryRun $ cacheGetStep cacheConfig
            buildResult <- buildStep optDryRun bk
            when (shouldUploadCoverage bk) $ uploadCoverageStep optDryRun
            whenRun optDryRun $ cachePutStep cacheConfig
            void $ weederStep optDryRun
            exitWith buildResult
        CleanupCache ->
            cleanupCacheStep optDryRun cacheConfig optBuildDirectory
        PurgeCache ->
            purgeCacheStep optDryRun cacheConfig optBuildDirectory

data BuildOpt = Standard | Fast deriving (Show, Eq)

data RebuildOpts = RebuildOpts
    { optBuildDirectory :: Maybe FilePath
    , optCacheDirectory :: Maybe FilePath
    , optDryRun :: DryRun
    } deriving (Show)

data Command = Build | CleanupCache | PurgeCache deriving (Show)

data DryRun = Run | DryRun deriving (Show, Eq)

data QA = QuickTest | FullTest deriving (Show, Eq)

data Jobs = Serial | Parallel deriving (Show, Eq)

rebuildOpts :: Parser RebuildOpts
rebuildOpts = RebuildOpts
    <$> optional buildDir
    <*> optional cacheName
    <*> dryRun
  where
    buildDir = option
        (FP.decodeString <$> str)
        (  long "build-dir"
        <> metavar "DIR"
        <> help "Copy sources to directory before building"
        )
    cacheName = option
        (FP.decodeString <$> str)
        (  long "cache-dir"
        <> metavar "DIR"
        <> help "Location of project's cache"
        )
    dryRun = flag Run DryRun
        (  long "dry-run"
        <> help "Print what build commands would be run, without executing them"
        )

parseOpts :: IO (RebuildOpts, Command)
parseOpts = execParser opts
  where
    opts = info
        (cmdOpts <**> helper)
        (fullDesc <> progDesc "Build cardano-wallet with stack in Buildkite")
    cmdOpts = (,)
        <$> rebuildOpts
        <*> (cmd <|> pure Build)
    cmd = subparser
        (  command "build" (info (pure Build) idm)
        <> command "cleanup-cache" (info (pure CleanupCache) idm)
        <> command "purge-cache" (info (pure PurgeCache) idm)
        )

buildStep :: DryRun -> Maybe BuildkiteEnv -> IO ExitCode
buildStep dryRun bk =
    echo "--- Build LTS Snapshot"
        *> build Standard ["--only-snapshot"]  .&&.
    echo "--- Build dependencies"
        *> build Standard ["--only-dependencies"] .&&.
    echo "+++ Build"
        *> build Fast ["--test", "--no-run-tests"] .&&.
    echo "+++ Test"
        *> timeout 30 (test Fast Serial .&&. test Fast Parallel)
  where
    build opt args =
        run dryRun "stack" $ concat
            [ color "always"
            , [ "build" ]
            , [ "--bench" ]
            , [ "--no-run-benchmarks" ]
            , [ "--haddock" ]
            , [ "--haddock-internal" ]
            , [ "--no-haddock-deps" ]
            , [ "--coverage" ]
            , fast opt
            , args
            ]

    test opt behavior =
        run dryRun "stack" $ concat
            [ color "always"
            , [ "test" ]
            , [ "--coverage" ]
            -- FIXME
            -- Figure out what's going on with http-bridge cluster setup...
            -- maybe...
            , skip "http-bridge-integration"
            , fast opt
            , case qaLevel bk of
                QuickTest -> skip "integration"
                FullTest -> []
            , case behavior of
                Serial ->
                    ta (match serialTests ++ jobs 1) ++ jobs 1
                Parallel ->
                    ta (skip serialTests)
            ]

    color arg = ["--color", arg]
    fast  arg = case arg of Standard -> []; Fast -> ["--fast"]
    jobs  arg = ["--jobs", T.pack (show @Int arg)]
    skip  arg = ["--skip", arg]
    match arg = ["--match", arg]
    ta    arg = ["--ta", T.unwords arg]

    serialTests = "SERIAL"

-- Stack with caching needs a build directory that is the same across
-- all BuildKite agents. The build directory option can be used to
-- ensure this is the case.
setupBuildDirectory :: DryRun -> FilePath -> IO ()
setupBuildDirectory dryRun buildDir = do
    removeDirectory dryRun buildDir
    src <- pwd
    printf ("Copying source tree "%fp%" -> "%fp%"\n") src buildDir
    whenRun dryRun $ do
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

-- | Whether we are building the repo's default branch.
onDefaultBranch :: BuildkiteEnv -> Bool
onDefaultBranch BuildkiteEnv{..} = bkBranch == bkDefaultBranch

-- | Whether we are building for Bors, based on the branch name.
isBorsBuild :: BuildkiteEnv -> Bool
isBorsBuild bk = "bors/" `T.isPrefixOf` bkBranch bk

-- | How much time to spend executing tests.
qaLevel :: Maybe BuildkiteEnv -> QA
qaLevel = maybe QuickTest level
  where
    level bk
        | isBorsBuild bk = FullTest
        | onDefaultBranch bk = QuickTest
        | otherwise = QuickTest

-- | Whether to upload test coverage information to coveralls.io.
shouldUploadCoverage :: Maybe BuildkiteEnv -> Bool
shouldUploadCoverage bk = qaLevel bk == FullTest

----------------------------------------------------------------------------
-- Weeder - uses contents of .stack-work to determine unused dependencies

weederStep :: DryRun -> IO ExitCode
weederStep dryRun = do
    echo "--- Weeder"
    run dryRun "weeder" []

----------------------------------------------------------------------------
-- Stack Haskell Program Coverage and upload to Coveralls

-- | Upload coverage information to coveralls.
uploadCoverageStep :: DryRun -> IO ()
uploadCoverageStep dryRun = do
    echo "--- Upload Test Coverage"
    need var >>= \case
        Nothing -> do
            eprintf ("Environment variable "%s%" not set.\n") var
            eprintf "Not uploading coverage information.\n"
        Just repoToken ->
            (findTix >>= generate) .&&. upload repoToken >>= \case
                ExitSuccess -> echo "Coverage information upload successful."
                ExitFailure _ -> echo "Coverage information upload failed."
  where
    var = "CARDANO_WALLET_COVERALLS_REPO_TOKEN"
    findTix = fold (find (suffix ".tix") "lib") Fold.list
    generate tixFiles = run dryRun "stack"
        ([ "hpc"
        , "report"
        , "--all"
        ] ++ map (format fp) tixFiles)
    upload repoToken = do
        let shcArgs = ["combined", "custom"]
        logCommand "shc" shcArgs
        whenRun' dryRun ExitSuccess $
            proc "shc" (["--repo-token", repoToken] ++ shcArgs) empty

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
getCacheConfig Nothing _ =
    pure (Left "BUILDKITE_* environment variables are not set")
getCacheConfig _ Nothing =
    pure (Left "--cache-dir argument was not provided")
getCacheConfig (Just bk) (Just ccCacheDir) =
    (fmap FP.fromText <$> need "STACK_ROOT") >>= \case
        Just ccStackRoot ->
            pure (Right CICacheConfig{ccBranches=cacheBranches bk,..})
        Nothing ->
            pure (Left "STACK_ROOT environment variable is not set")

-- | Create the list of branches to source caches from.
--   1. Build branch;
--   2. PR base branch;
--   3. Repo default branch.
cacheBranches :: BuildkiteEnv -> [Text]
cacheBranches BuildkiteEnv{..} =
    [bkBranch] ++ maybeToList bkBaseBranch ++ [bkDefaultBranch]

cacheGetStep :: Either Text CICacheConfig -> IO ()
cacheGetStep cacheConfig = do
    echo "--- CI Cache Restore"
    case cacheConfig of
        Right cfg -> restoreCICache cfg `catch` \(ex :: IOException) ->
            eprintf ("Failed to download CI cache: "%w%"\nContinuing anyway...\n") ex
        Left ex ->
            eprintf ("Not using CI cache because "%s%"\n") ex

cachePutStep :: Either Text CICacheConfig -> IO ()
cachePutStep cacheConfig = do
    echo "--- CI Cache Save"
    case cacheConfig of
        Right cfg -> saveCICache cfg `catch` \(ex :: IOException) ->
            eprintf ("Failed to upload CI cache: "%w%"\n") ex
        Left _ ->
            printf "CI cache not configured.\n"

getCacheArchive :: MonadIO io => CICacheConfig -> FilePath -> io (Maybe FilePath)
getCacheArchive CICacheConfig{..} ext = do
    let caches = mapMaybe (getCacheName ccCacheDir) ccBranches
    headMay <$> filterM testfile (map (</> ext) caches)

-- | The cache directory for a given branch name. This filepath always has a
-- trailing slash.
getCacheName :: FilePath -> Text -> Maybe FilePath
getCacheName base branch
    | ".." `T.isInfixOf` branch = Nothing
    | otherwise = Just (base </> FP.fromText branch </> "")

-- | The filename for a given branch and cache name.
putCacheName :: CICacheConfig -> FilePath -> Maybe FilePath
putCacheName CICacheConfig{..} ext =
    (</> ext) <$> getCacheName ccCacheDir (head ccBranches)

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
restoreStackRoot cfg@CICacheConfig{..} =
    restoreZippedCache stackRootCache cfg $ \tar -> do
        whenM (testpath ccStackRoot) $ rmtree ccStackRoot
        mktree ccStackRoot
        TB.procs "tar" ["-C", "/", "-x"] tar

restoreStackWork :: CICacheConfig -> IO ()
restoreStackWork cfg =
    restoreZippedCache stackWorkCache cfg (TB.procs "tar" ["-x"])

restoreZippedCache
    :: FilePath
    -> CICacheConfig
    -> (Shell ByteString -> IO ())
    -> IO ()
restoreZippedCache ext cfg act = getCacheArchive cfg ext >>= \case
    Just tarfile -> do
        size <- du tarfile
        printf ("Restoring cache "%fp%" ("%sz%") ... ") tarfile size
        act $ TB.inproc "lz4cat" ["-d"] (TB.input tarfile)
        printf "done.\n"
    Nothing ->
        printf ("No "%fp%" cache found.\n") ext

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

cleanupCacheStep :: DryRun -> Either Text CICacheConfig -> Maybe FilePath -> IO ()
cleanupCacheStep dryRun cacheConfig buildDir = do
    echo "--- Cleaning up CI cache"
    case cacheConfig of
        Right CICacheConfig{..} -> do
            getBranches >>= cleanupCache dryRun ccCacheDir
            -- Remove the stack root left by the previous build.
            removeDirectory dryRun ccStackRoot
            -- Remove the build directory left by the previous build.
            doMaybe (removeDirectory dryRun) buildDir
        Left ex ->
            eprintf ("Not cleaning up CI cache because: "%s%"\n") ex

purgeCacheStep :: DryRun -> Either Text CICacheConfig -> Maybe FilePath -> IO ()
purgeCacheStep dryRun cacheConfig buildDir = do
    echo "--- Deleting all CI caches"
    case cacheConfig of
        Right CICacheConfig{..} -> do
            removeDirectory dryRun ccCacheDir
            removeDirectory dryRun ccStackRoot
            doMaybe (removeDirectory dryRun) buildDir
        Left ex ->
            eprintf ("Not purging CI cache because: "%s%"\n") ex

-- | Remove all files and directories that do not belong to an active branch cache.
cleanupCache :: DryRun -> FilePath -> [Text] -> IO ()
cleanupCache dryRun cacheDir activeBranches = do
    let branchCaches = mapMaybe (getCacheName cacheDir) activeBranches
        isCache cf = any (\dir -> format fp cf `T.isPrefixOf` format fp dir)
    files <- fold (lstree cacheDir) (Fold.revList)
    forM_ files $ \cf -> do
        st <- stat cf
        if isDirectory st
            then unless (isCache cf branchCaches) $ do
                printf ("Removing directory "%fp%"\n") cf
                whenRun dryRun $ rmdir cf
            else unless (directory cf `elem` branchCaches) $ do
                printf ("Removing file "%fp%"\n") cf
                whenRun dryRun $ rm cf

removeDirectory :: DryRun -> FilePath -> IO ()
removeDirectory dryRun dir = whenM (testpath dir) $ do
    printf ("Removing directory "%fp%".\n") dir
    whenRun dryRun $ rmtree dir

-- | Ask the origin git remote for its list of branches.
getBranches :: MonadIO io => io [Text]
getBranches =
    T.lines <$> strict (sed branchPat (grep branchPat git))
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

run :: MonadIO io => DryRun -> Text -> [Text] -> io ExitCode
run dryRun cmd args = do
    logCommand cmd args
    whenRun' dryRun ExitSuccess $ do
        res <- proc cmd args empty
        case res of
            ExitSuccess ->
                pure ()
            ExitFailure code ->
                eprintf
                    ("error: Command exited with code "%d%"!\nContinuing...\n")
                    code
        pure res

logCommand :: MonadIO io => Text -> [Text] -> io ()
logCommand cmd args = printf (s % " " % s % "\n") cmd args'
  where
    args' = T.unwords $ map quote args
    -- simple quoting, just for logging
    quote arg | T.any isSpace arg = "'" <> arg <> "'"
              | otherwise = arg

-- | Runs an action when not in --dry-run mode.
whenRun :: Applicative m => DryRun -> m a -> m ()
whenRun dry = whenRun' dry () . void

-- | Runs an action when not in --dry-run mode, with alternative return value.
whenRun' :: Applicative m => DryRun -> a -> m a -> m a
whenRun' DryRun a _ = pure a
whenRun' Run _ ma = ma

-- | Run an action, but cancel it if it doesn't complete within the given number
-- of minutes.
timeout :: Int -> IO ExitCode -> IO ExitCode
timeout mins act = race (sleep (fromIntegral mins * 60)) act >>= \case
    Right r ->
        pure r
    Left () -> do
        eprintf ("\nTimed out after "%d%" minutes.\n") mins
        pure (ExitFailure 124)
