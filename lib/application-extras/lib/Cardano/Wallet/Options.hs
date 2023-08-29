{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
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
      isEnvSet
    , envFromText
    , lookupEnvNonEmpty

      -- * Options
    , optionT
    , fromTextS

    ) where

import Prelude

import Control.Arrow
    ( left )
import Control.Monad.IO.Unlift
    ( MonadIO (liftIO), MonadUnliftIO )
import Data.Maybe
    ( isJust )
import Data.Text.Class
    ( FromText (..), TextDecodingError (getTextDecodingError) )
import "optparse-applicative" Options.Applicative
    ( Mod, OptionFields, Parser, eitherReader, option )
import System.Environment
    ( lookupEnv )

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
