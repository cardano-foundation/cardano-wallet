{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides the package version and git revision which this was compiled from.
--
-- It is assumed that all cardano-wallet packages have the same version, that of
-- the core package.
--
-- Stack builds will have the `git` command available to run during
-- compilation.
--
-- Nix builds will inject the git revision into the executables after
-- compiling. If the git revision has changed but the sources have
-- not, then no haskell packages will be rebuilt, but the embedded git
-- revision will be updated.

module Cardano.Wallet.Version
    ( -- * Values computed at compile-time
      version
    , gitRevision
    , GitRevision
    , Version

      -- * Displaying Versions
    , showVersion
    , showFullVersion
    ) where

import Prelude

import Cardano.Wallet.Version.TH
    ( gitRevFromGit )
import Data.FileEmbed
    ( dummySpaceWith )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Data.Text.Encoding
    ( decodeLatin1 )
import Data.Version
    ( Version, showVersion )
import Paths_cardano_wallet_core
    ( version )

import qualified Data.Text as T

newtype GitRevision = GitRevision Text deriving (Show, Eq)

-- | Like 'showVersion', but also show the git revision.
showFullVersion :: Version -> GitRevision -> String
showFullVersion v (GitRevision r) =
    showVersion v <> " (git revision: " <> T.unpack r <> ")"

-- | The Git revision ID (40 character hex string) of this build.
--
-- This requires @git@ do be available when building. Alternatively, the git
-- revision of the @cardano-wallet@ binary can be updated post-build using
-- "Data.FileEmbed.injectWith".
gitRevision :: GitRevision
gitRevision
    | gitRevEmbed /= zeroRev = GitRevision gitRevEmbed
    | T.null fromGit         = GitRevision zeroRev
    | otherwise              = GitRevision fromGit
  where
    -- Git revision embedded after compilation using
    -- Data.FileEmbed.injectWith. If nothing has been injected,
    -- this will be filled with 0 characters.
    gitRevEmbed :: Text
    gitRevEmbed = decodeLatin1 $(dummySpaceWith "gitrev" 40)

    -- Git revision found during compilation by running git. If
    -- git could not be run, then this will be empty.
    fromGit = T.strip (fromString $(gitRevFromGit))

    zeroRev :: Text
    zeroRev = "0000000000000000000000000000000000000000"
