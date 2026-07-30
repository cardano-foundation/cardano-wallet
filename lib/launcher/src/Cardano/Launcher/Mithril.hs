{-# LANGUAGE OverloadedStrings #-}

module Cardano.Launcher.Mithril
    ( downloadLatestSnapshot
    , downloadMithril
    , downloadMithrilWith
    , MithrilExePath (..)
    )
where

import Control.Monad
    ( unless
    )
import Network.HTTP.Simple
    ( getResponseBody
    , httpBS
    , parseRequest
    )
import System.Directory
    ( doesFileExist
    , findExecutable
    , withCurrentDirectory
    )
import System.FilePath
    ( (</>)
    )
import System.Info
    ( arch
    , os
    )
import System.Process
    ( callProcess
    )
import Prelude

import qualified Data.ByteString as BS

newtype MithrilExePath = MithrilExePath {mithrilExePath :: FilePath}

-- | Download the latest snapshot node db into /db relative to the supplied dir.
downloadLatestSnapshot :: FilePath -> MithrilExePath -> IO ()
downloadLatestSnapshot outputParentDir (MithrilExePath mithril) = do
    callProcess
        mithril
        [ "cdb"
        , "download"
        , "latest"
        , "--include-ancillary"
        , "--download-dir"
        , outputParentDir
        ]

-- | Prefer PATH, then fall back to downloading the Mithril client.
--
-- Looks up @mithril-client@ via 'findExecutable'. When present, returns that
-- path without downloading. When absent, downloads the release package into
-- the supplied working directory.
--
-- May interactively ask how to handle conflicts if items in the supplied
-- working directory conflict with items in the mithril release archive.
downloadMithril :: FilePath -> IO MithrilExePath
downloadMithril =
    downloadMithrilWith findExecutable downloadMithrilFromGitHub

-- | PATH-first Mithril client resolution with an injectable finder and
-- download action (for unit tests).
--
-- * When @finder "mithril-client"@ yields @Just path@, logs and returns that
--   path without calling @download@.
-- * When the finder yields @Nothing@, logs and calls @download workingDir@.
downloadMithrilWith
    :: (String -> IO (Maybe FilePath))
    -- ^ Executable lookup (production: 'findExecutable')
    -> (FilePath -> IO MithrilExePath)
    -- ^ Download fallback
    -> FilePath
    -- ^ Working directory for downloads
    -> IO MithrilExePath
downloadMithrilWith findExe download workingDir = do
    mPath <- findExe "mithril-client"
    case mPath of
        Just path -> do
            putStrLn $ "Using mithril-client from PATH: " <> path
            pure $ MithrilExePath path
        Nothing -> do
            putStrLn "mithril-client not found on PATH, downloading..."
            download workingDir

-- | Download the latest Mithril release package into @workingDir@ and return
-- the path to the extracted client.
downloadMithrilFromGitHub :: FilePath -> IO MithrilExePath
downloadMithrilFromGitHub workingDir = withCurrentDirectory workingDir $ do
    putStrLn $ "Downloading " <> mithrilPackage <> " from " <> downloadUrl
    -- On Windows, crypton-x509-system reads the current user's ROOT
    -- certificate store which may be empty when running as SYSTEM.
    -- Use curl.exe which uses the Local Machine store instead.
    if isWindows
        then do
            callProcess
                "curl.exe"
                ["-L", "-o", mithrilPackage, downloadUrl]
        else do
            req <- parseRequest downloadUrl
            response <- httpBS req
            BS.writeFile mithrilPackage (getResponseBody response)
    downloaded <- doesFileExist mithrilPackage
    unless downloaded
        $ fail
        $ "Failed to download " <> mithrilPackage
    putStrLn $ "Downloaded " <> mithrilPackage

    -- Extract the tar.gz archive in one step.
    -- Windows 10+ ships BSD tar with gzip support, so this works
    -- cross-platform without requiring 7z.
    putStrLn $ "Extracting " <> mithrilPackage <> "..."
    callProcess "tar" ["xzf", mithrilPackage]

    let clientPath =
            workingDir </> ("mithril-client" <> if isWindows then ".exe" else "")
    mithrilClientExists <- doesFileExist clientPath
    unless mithrilClientExists
        $ fail
        $ unwords
            [ "downloadLatest: didn't find"
            , clientPath
            , "in mithril archive"
            ]

    putStrLn $ "Mithril client available at: " <> clientPath
    return $ MithrilExePath clientPath
  where
    isWindows = os == "mingw32"

    -- Define the platform and version.
    platform = osTag <> "-" <> osArch
      where
        osTag :: String
        osTag = case os of
            "darwin" -> "macos"
            "mingw32" -> "windows"
            other -> other

        osArch :: String
        osArch = case arch of
            "x86_64" -> "x64"
            "aarch64" -> "arm64"
            other -> other

    version = "2603.1"
    mithrilPackage = "mithril-" <> version <> "-" <> platform <> ".tar.gz"
    downloadUrl =
        "https://github.com/input-output-hk/mithril/releases/download/"
            <> version
            <> "/"
            <> mithrilPackage
