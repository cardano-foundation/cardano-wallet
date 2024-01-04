{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Faucet.FaucetM where

import Prelude

import qualified Servant

import Cardano.Faucet.Mnemonics
    ( MnemonicLength
    )
import Cardano.Faucet.Types
    ( IndexedMnemonic
    )
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Monad.Error.Class
    ( MonadError
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Reader
    ( MonadReader (ask)
    , ReaderT (..)
    )
import Control.Monad.State
    ( MonadState (..)
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map.Lazy
    ( Map
    )

newtype FaucetState = FaucetState
    { indexedMnemonics :: Map MnemonicLength (NonEmpty IndexedMnemonic)
    }

newtype FaucetM a = FaucetM (ReaderT (TVar FaucetState) Servant.Handler a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError Servant.ServerError
    , MonadReader (TVar FaucetState)
    )

runFaucetM :: FaucetState -> FaucetM a -> Servant.Handler a
runFaucetM s (FaucetM r) = runReaderT r =<< liftIO (newTVarIO s)

instance MonadState FaucetState FaucetM where
  get = FaucetM do
    ref <- ask
    liftIO $ readTVarIO ref

  put s = FaucetM do
    ref <- ask
    liftIO $ atomically $ writeTVar ref s

  state f = FaucetM do
    ref <- ask
    liftIO $ atomically $ do
      s <- readTVar ref
      let (a, s') = f s
      writeTVar ref s'
      pure a
