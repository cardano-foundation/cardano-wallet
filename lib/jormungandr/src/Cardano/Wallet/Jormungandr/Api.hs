{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- A specification for the Jörmungandr REST API.
module Cardano.Wallet.Jormungandr.Api
    ( Api
    , GetAccountState
    , GetBlock
    , GetTipId
    , GetBlockDescendantIds
    , GetStakeDistribution
    , PostMessage
    , api
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountId
    , AccountState
    , BlockId
    , Hex
    , JormungandrBinary
    , StakeApiResponse
    )
import Cardano.Wallet.Jormungandr.Binary
    ( Block )
import Cardano.Wallet.Primitive.Types
    ( SealedTx (..) )
import Data.Proxy
    ( Proxy (..) )
import Servant.API
    ( (:<|>), (:>), Capture, Get, JSON, NoContent, Post, QueryParam, ReqBody )

api :: Proxy Api
api = Proxy

type Api =
    GetAccountState
    :<|> GetTipId
    :<|> GetBlock
    :<|> GetBlockDescendantIds
    :<|> PostMessage
    :<|> GetStakeDistribution

type GetAccountState
    = "api" :> "v0"
    :> "account"
    :> Capture "accountId" AccountId
    :> Get '[JSON] AccountState

-- | Retrieve a block by its id.
type GetBlock
    = "api" :> "v0"
    :> "block"
    :> Capture "blockHeaderHash" BlockId
    :> Get '[JormungandrBinary] Block

-- | Retrieve 'n' descendants of a given block, sorted from closest to
-- farthest.
--
-- There might also exist fewer than 'n' descendants.
--
-- For n=3 we might have:
--
-- > [genesis] ... -- [b] -- [b+1] -- [b+2] -- [b+3] -- ... -- [tip]
-- >                   \       \                  \
-- >                  parent    +--- descendants ---+
type GetBlockDescendantIds
    = "api" :> "v0"
    :> "block"
    :> Capture "blockId" BlockId
    :> "next_id"
    :> QueryParam "count" Word
    :> Get '[JormungandrBinary] [BlockId]

-- | Retrieve the header of the latest known block.
type GetTipId
    = "api" :> "v0"
    :> "tip"
    :> Get '[Hex] BlockId

type PostMessage
    = "api" :> "v0"
    :> "message"
    :> ReqBody '[JormungandrBinary] SealedTx
    :> Post '[NoContent] NoContent

-- | Retrieve stake distribution
type GetStakeDistribution
    = "api" :> "v0"
    :> "stake"
    :> Get '[JSON] StakeApiResponse
