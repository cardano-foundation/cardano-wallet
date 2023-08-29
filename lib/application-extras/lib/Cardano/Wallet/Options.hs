{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Command-line option passing for cardano-wallet shelley.
module Cardano.Wallet.Options
    ( -- * Utils
      withSystemTempDir
    , withTempDir
    , SkipCleanup (..)
    , isEnvSet
    , envFromText
    , lookupEnvNonEmpty

      -- * Options
    , optionT
    , fromTextS

      -- * Logging
    , TempDirLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation, HasSeverityAnnotation (..) )
import Cardano.Wallet.Logging
    ( BracketLog, BracketLog' (BracketStart), bracketTracer )
import Control.Arrow
    ( left )
import Control.Monad.IO.Unlift
    ( MonadIO (liftIO), MonadUnliftIO )
import Control.Tracer
    ( Tracer )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Maybe
    ( isJust )
import Data.Text.Class
    ( FromText (..), TextDecodingError (getTextDecodingError), ToText (..) )
import "optparse-applicative" Options.Applicative
    ( Mod, OptionFields, Parser, eitherReader, option )
import System.Environment
    ( lookupEnv )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import UnliftIO.Temporary
    ( withTempDirectory )

import qualified Data.Text as T

-- | Helper for writing an option 'Parser' using a 'FromText' instance.
optionT :: FromText a => Mod OptionFields a -> Parser a
optionT = option (eitherReader fromTextS)

-- | Like 'fromText', but stringly-typed.
fromTextS :: FromText a => String -> Either String a
fromTextS = left getTextDecodingError . fromText . T.pack

{-------------------------------------------------------------------------------
                           Environment Variable Utils
-------------------------------------------------------------------------------}

-- | Looks up an environment variable, treating variables which are defined but
-- empty the same as variables which are undefined.
lookupEnvNonEmpty :: MonadUnliftIO m => String -> m (Maybe String)
lookupEnvNonEmpty = liftIO . fmap nonEmpty . lookupEnv
  where
    nonEmpty (Just "") = Nothing
    nonEmpty m = m

-- | Returns true iff an environment variable is defined and non-empty.
isEnvSet :: MonadUnliftIO m => String -> m Bool
isEnvSet = fmap isJust . lookupEnvNonEmpty

-- | Parses an environment variable using text-class.
envFromText
    :: (MonadUnliftIO m, FromText a)
    => String
    -> m (Maybe (Either TextDecodingError a))
envFromText = liftIO . fmap (fmap (fromText . T.pack)) . lookupEnv

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

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
