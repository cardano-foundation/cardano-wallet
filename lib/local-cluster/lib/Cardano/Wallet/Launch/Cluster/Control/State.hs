{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Launch.Cluster.Control.State
    ( ControlState (..)
    , newState
    , phaseL
    , changeClusterPhase
    , withControlLayer
    , ControlLayer (..)
    , Phase (..)
    )
where

import Prelude

import Control.Lens
    ( over
    )
import Control.Monad.Cont
    ( ContT (..)
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import GHC.Generics
    ( Generic
    )
import UnliftIO.STM
    ( atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    )

data Phase = Metadata | Genesis | Pool0 | Funding | Pools | Relay | Cluster
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The state of the control server.
newtype ControlState = ControlState
    { genesisPhase :: Set Phase
    }

phaseL
    :: Functor f
    => (Set Phase -> f (Set Phase))
    -> ControlState
    -> f ControlState
phaseL f (ControlState gp) = ControlState <$> f gp

newState :: ControlState
newState = ControlState mempty

changeClusterPhase :: Phase -> ControlState -> ControlState
changeClusterPhase pc = over phaseL (Set.insert pc)

data ControlLayer m = ControlLayer
    { controlState :: m ControlState
    , changePhase :: Phase -> m ()
    }

withControlLayer :: MonadIO m => ContT a m (ControlLayer m)
withControlLayer = ContT $ \action -> do
    var <- newTVarIO newState
    let controlState = readTVarIO var
        changePhase pc = atomically
            $ modifyTVar' var
            $ \state -> changeClusterPhase pc state
    action ControlLayer{controlState, changePhase}
