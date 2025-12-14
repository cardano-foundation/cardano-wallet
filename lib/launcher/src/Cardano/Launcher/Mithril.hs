{-# LANGUAGE OverloadedStrings #-}

module Cardano.Launcher.Mithril
    ( downloadLatestSnapshot
    , downloadMithril
    , MithrilExePath (..)
    )
    where

import Prelude

import qualified Data.ByteString as BS

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

newtype MithrilExePath = MithrilExePath { mithrilExePath :: FilePath }

-- | Download the latest snapshot node db into /db relative to the supplied dir.
downloadLatestSnapshot :: FilePath -> MithrilExePath -> IO ()
downloadLatestSnapshot outputParentDir (MithrilExePath mithril) = do
    callProcess mithril ["cdb", "download", "latest", "--include-ancillary", "--download-dir", outputParentDir]

-- | Downloads the latest Mithril release package,
-- extracts it, and returns the path to the mithril client executable.
--
-- May interactively ask how to handle conflicts if items in the supplied
-- working directory conflict with items in the mithril release archive.
downloadMithril :: FilePath -> IO MithrilExePath
downloadMithril workingDir = withCurrentDirectory workingDir $ do
    putStrLn $ "Downloading " <> mithrilPackage <> " from " <> downloadUrl
    req <- parseRequest downloadUrl
    response <- httpBS req
    BS.writeFile mithrilPackage (getResponseBody response)
    putStrLn $ "Downloaded " <> mithrilPackage

    -- Extract the gz archive using 7z.
    putStrLn $ "Extracting " <> mithrilPackage <> " using 7z..."
    callProcess "7z" ["x", mithrilPackage]

    -- Extract the tar archive.
    putStrLn $ "Extracting " <> mithrilTar <> " using tar..."
    callProcess "tar" ["xf", mithrilTar]

    let clientPath = workingDir </> ("mithril-client" <> if isWindows then ".exe" else "")
    mithrilClientExists <- doesFileExist clientPath
    unless mithrilClientExists $
        fail $ unwords
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

    version = "2543.1-hotfix"
    mithrilTar    = "mithril-" <> version <> "-" <> platform <> ".tar"
    mithrilPackage = mithrilTar <> ".gz"
    downloadUrl   = "https://github.com/input-output-hk/mithril/releases/download/"
                   <> version <> "/" <> mithrilPackage
