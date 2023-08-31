{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.Temp.Extra
    ( withSystemTempDir
    , withTempDir
    , SkipCleanup (..)

      -- * Logging
    , TempDirLog (..)
    ) where

import qualified Data.Text as T

import Prelude

import Cardano.BM.Extra
    ( BracketLog, BracketLog' (BracketStart), bracketTracer )
import Cardano.BM.Tracer
    ( Tracer )
import Cardano.BM.Tracing
    ( HasPrivacyAnnotation
    , HasSeverityAnnotation (..)
    , Severity (Debug, Notice)
    )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Text.Class
    ( ToText (..) )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import UnliftIO
    ( MonadIO (liftIO), MonadUnliftIO, withTempDirectory )

newtype SkipCleanup = SkipCleanup Bool
    deriving stock (Show, Eq)

-- | Create a temporary directory
-- and optionally remove it after the given IO action has finished
withTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> FilePath
    -- ^ Parent directory
    -> String
    -- ^ Directory name template
    -> SkipCleanup
    -- ^ When true will not remove the directory after its usage.
    -> (FilePath -> m a)
    -- ^ Callback that can use the directory
    -> m a
withTempDir tr parent name (SkipCleanup skipCleanup) action =
    if skipCleanup
        then do
            dir <- liftIO $ createTempDirectory parent name
            bracketTracer (MsgNoCleanup dir >$< tr) (action dir)
        else withTempDirectory parent name action

withSystemTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> String
    -- ^ Directory name template
    -> SkipCleanup
    -- ^ When true will not remove the directory after its usage.
    -> (FilePath -> m a)
    -- ^ Callback that can use the directory
    -> m a
withSystemTempDir tr name skipCleanup action = do
    parent <- liftIO getCanonicalTemporaryDirectory
    withTempDir tr parent name skipCleanup action

data TempDirLog = MsgNoCleanup FilePath BracketLog deriving (Show)

instance ToText TempDirLog where
    toText = \case
        MsgNoCleanup _ BracketStart -> ""
        MsgNoCleanup dir _ ->
            "Not removing the temporary directory: " <> T.pack dir

instance HasPrivacyAnnotation TempDirLog
instance HasSeverityAnnotation TempDirLog where
    getSeverityAnnotation = \case
        MsgNoCleanup _ BracketStart -> Debug
        MsgNoCleanup _ _ -> Notice
