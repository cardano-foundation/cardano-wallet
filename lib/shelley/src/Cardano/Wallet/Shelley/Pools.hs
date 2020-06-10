{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Haskell-node "shelley" implementation of the @StakePoolLayer@ abstraction,
-- i.e. some boring glue.
module Cardano.Wallet.Shelley.Pools where

import Prelude

import Cardano.Wallet.Api.Server
    ( LiftHandler (..), apiError )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (NotSynced), ApiT (..), defaultRecordTypeOptions )
import Cardano.Wallet.Primitive.Types
    ( PoolId )
import Cardano.Wallet.Shelley.Network
    ( StakePoolMetrics (..) )
import Control.Concurrent.MVar
    ( MVar, takeMVar )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.Quantity
    ( Quantity (..) )
import GHC.Generics
    ( Generic )
import Servant
    ( Handler, err503 )

--
-- Api Types
--

data ErrListStakePools = ErrNoStakeDistribution

instance LiftHandler ErrListStakePools where
    handler = \case
        ErrNoStakeDistribution ->
            apiError err503 NotSynced $ mconcat
                [ "I can't list stake pools yet because I haven't fetched them "
                , "from the node not yet."
                ]

data ApiStakePool = ApiStakePool
    { _id :: !(ApiT PoolId)
    , _stake :: Double
    , _nonMyopicMemberRewards :: Int
    } deriving (Eq, Show, Generic)

-- TODO: Add fields:
--  _nonMyopicMemberRewards :: !Double
--  _saturation :: !Double

instance FromJSON ApiStakePool where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance ToJSON ApiStakePool where
    toJSON = genericToJSON defaultRecordTypeOptions

--
-- Api Server Handler
--

listPools
    :: MVar [StakePoolMetrics]
    -> Handler [ApiStakePool]
listPools var = do
        liftIO $ map mkApiPool <$> takeMVar var
  where
    mkApiPool (StakePoolMetrics pid s r) = ApiStakePool
        { _id = ApiT pid
        , _stake = fromIntegral $ getQuantity s
        , _nonMyopicMemberRewards = fromIntegral $ getQuantity r
        }
