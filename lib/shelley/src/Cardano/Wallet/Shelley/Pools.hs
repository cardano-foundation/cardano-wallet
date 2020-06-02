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
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId )
import Control.Monad.Trans.Except
    ( withExceptT )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.Quantity
    ( Quantity (..) )
import GHC.Generics
    ( Generic )
import Servant
    ( Handler, err503 )

import qualified Data.Map as Map

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
    :: NetworkLayer IO t b
    -> Handler [ApiStakePool]
listPools nw = liftHandler $ do
        let dummyEpoch = error "todo: epoch not actually needed"
        distr <- withExceptT (const ErrNoStakeDistribution) $
            stakeDistribution nw dummyEpoch
        return $ map mkApiPool $ Map.toList distr
  where
    mkApiPool (pid, pstake) = ApiStakePool
        { _id = ApiT pid
        -- FIXME: Hack to workaround type signature of network layer.
        , _stake = fromIntegral (getQuantity pstake) / 1000000
        }
