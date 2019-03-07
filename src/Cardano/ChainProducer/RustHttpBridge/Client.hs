{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | An API client for the Cardano HTTP Bridge.
module Cardano.ChainProducer.RustHttpBridge.Client
    ( getBlockByHash
    , getEpochById
    , getTipBlockHeader
    ) where

import Cardano.ChainProducer.RustHttpBridge.Api
    ( Block, BlockHeader, EpochId, NetworkName, api )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( ClientM, client )
import Servant.Extra.ContentTypes
    ( Hash, WithHash )

-- | Retrieve a block identified by the unique hash of its header.
getBlockByHash :: NetworkName -> Hash Blake2b_256 BlockHeader -> ClientM Block

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
getEpochById :: NetworkName -> EpochId -> ClientM [Block]

-- | Retrieve the header of the latest known block.
getTipBlockHeader :: NetworkName -> ClientM (WithHash Blake2b_256 BlockHeader)

getBlockByHash
    :<|> getEpochById
    :<|> getTipBlockHeader
    = client api

