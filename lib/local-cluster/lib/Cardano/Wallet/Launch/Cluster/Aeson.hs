{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Launch.Cluster.Aeson
    ( withAddedKey
    , withObject
    , ChangeValue
    , decodeFileThrow
    )
where

import Prelude

import Data.Aeson
    ( FromJSON
    , ToJSON (toJSON)
    , Value
    , eitherDecodeFileStrict
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson

withAddedKey
    :: (MonadFail m, Aeson.ToJSON a)
    => Aeson.Key
    -> a
    -> Aeson.Value
    -> m Aeson.Value
withAddedKey k v = withObject (pure . Aeson.insert k (toJSON v))

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ ->
        fail
            "withObject: was given an invalid JSON. Expected an Object but got \
            \something else."

type ChangeValue = Value -> Value

decodeFileThrow :: FromJSON a => FilePath -> IO a
decodeFileThrow fp = do
    e <- eitherDecodeFileStrict fp
    case e of
        Left err -> fail err
        Right a -> return a
