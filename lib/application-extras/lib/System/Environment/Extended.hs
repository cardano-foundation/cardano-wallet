module System.Environment.Extended
    ( module Reexport
    , lookupEnvNonEmpty
    , isEnvSet
    , envFromText
    ) where

import System.Environment as Reexport

import Prelude

import Control.Monad
    ( mfilter
    )
import Data.Maybe
    ( isJust
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError
    )
import UnliftIO
    ( MonadIO (liftIO)
    , MonadUnliftIO
    )

import qualified Data.Text as T

-- | Looks up an environment variable, treating variables which are defined but
-- empty the same as variables which are undefined.
lookupEnvNonEmpty :: MonadUnliftIO m => String -> m (Maybe String)
lookupEnvNonEmpty = liftIO . (mfilter (not . null) <$>) . lookupEnv

-- | Returns true iff an environment variable is defined and non-empty.
isEnvSet :: MonadUnliftIO m => String -> m Bool
isEnvSet = fmap isJust . lookupEnvNonEmpty

-- | Parses an environment variable using text-class.
envFromText
    :: (MonadUnliftIO m, FromText a)
    => String
    -> m (Maybe (Either TextDecodingError a))
envFromText = liftIO . fmap (fmap (fromText . T.pack)) . lookupEnv
