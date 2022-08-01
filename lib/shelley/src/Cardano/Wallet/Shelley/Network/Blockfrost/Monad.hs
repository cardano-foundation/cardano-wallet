{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Monad where

import Prelude

import Cardano.Wallet.Network.Light
    ( Consensual (..) )
import Cardano.Wallet.Shelley.Network.Blockfrost.Error
    ( BlockfrostError (ClientError), throwBlockfrostError )
import Control.Monad.Base
    ( MonadBase )
import Control.Monad.Except
    ( ExceptT, MonadError (..), MonadIO (..) )
import Control.Monad.Reader
    ( MonadReader (ask), ReaderT (..), asks )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Data.Maybe
    ( fromMaybe )
import Network.HTTP.Types
    ( Status (statusCode) )
import Servant.Client
    ( runClientM )

import qualified Blockfrost.Client as BF
import qualified Servant.Client as Servant

newtype BFM a = BFM (ReaderT BF.ClientConfig (ExceptT BlockfrostError IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadBase IO
        , MonadBaseControl IO
        , MonadReader BF.ClientConfig
        , MonadError BlockfrostError
        )

instance BF.MonadBlockfrost BFM where
    getConf = ask
    liftBlockfrostClient act = BFM do
        env <- asks fst
        liftIO (runClientM act env) >>= either (throwError . ClientError) pure

newClientConfig :: BF.Project -> IO BF.ClientConfig
newClientConfig prj = (,prj) <$> BF.newEnvByProject prj

run :: BF.ClientConfig -> (forall a. BFM a -> IO a)
run cfg (BFM c) = throwBlockfrostError (runReaderT c cfg)

maybe404 :: BFM a -> BFM (Maybe a)
maybe404 = handleStatus Nothing Just 404

empty404 :: Monoid a => BFM a -> BFM a
empty404 = (fromMaybe mempty <$>) . maybe404

consensual404 :: BFM a -> BFM (Consensual a)
consensual404 = (maybe NotConsensual Consensual <$>) . maybe404

handleStatus :: b -> (a -> b) -> Int -> BFM a -> BFM b
handleStatus notMatched matched status bfm =
    (matched <$> bfm) `catchError` \case
        ClientError (Servant.FailureResponse _ (Servant.Response s  _ _ _))
            | statusCode s == status -> pure notMatched
        e -> throwError e

