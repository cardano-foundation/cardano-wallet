{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Monad where

import Prelude

import Cardano.Wallet.Shelley.Network.Blockfrost.Error
    ( BlockfrostError (ClientError)
    , BlockfrostException (BlockfrostException)
    )
import Control.Exception
    ( throwIO )
import Control.Monad.Base
    ( MonadBase )
import Control.Monad.Except
    ( ExceptT, MonadError (..), MonadIO (..), runExceptT, (<=<) )
import Control.Monad.Reader
    ( MonadReader (ask), ReaderT (..), asks )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Data.Maybe
    ( fromMaybe )
import Network.HTTP.Types
    ( status404 )
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

run :: BF.ClientConfig -> BFM a -> IO a
run cfg (BFM c) = handleBlockfrostError (runReaderT c cfg)

handleBlockfrostError :: ExceptT BlockfrostError IO a -> IO a
handleBlockfrostError =
    either (throwIO . BlockfrostException) pure <=< runExceptT

maybe404 :: BFM a -> BFM (Maybe a)
maybe404 bfm = (Just <$> bfm) `catchError` \case
    ClientError (Servant.FailureResponse _ (Servant.Response s _ _ _))
        | s == status404 -> pure Nothing
    e -> throwError e

empty404 :: Monoid a => BFM a -> BFM a
empty404 = (fromMaybe mempty <$>) . maybe404
