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
import Data.Tuple
    ( swap
    )
import GHC.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )

newtype FaucetState = FaucetState
    { indexedMnemonics :: Map MnemonicLength (NonEmpty IndexedMnemonic)
    }

newtype FaucetM a = FaucetM (ReaderT (IORef FaucetState) Servant.Handler a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError Servant.ServerError
    , MonadReader (IORef FaucetState)
    )

runFaucetM :: FaucetState -> FaucetM a -> Servant.Handler a
runFaucetM s (FaucetM r) = runReaderT r =<< liftIO (newIORef s)

instance MonadState FaucetState FaucetM where
  get = FaucetM do
    ref <- ask
    liftIO $ readIORef ref

  put s = FaucetM do
    ref <- ask
    liftIO $ writeIORef ref s

  state f = FaucetM do
    ref <- ask
    liftIO $ atomicModifyIORef' ref (swap . f)

modifyFaucetState :: (FaucetState -> FaucetM (FaucetState, a)) -> FaucetM a
modifyFaucetState f = do
  s <- get
  (s', a) <- f s
  put s'
  pure a
