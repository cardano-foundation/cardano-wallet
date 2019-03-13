{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An API client for the Cardano HTTP Bridge.
module Cardano.ChainProducer.RustHttpBridge.Client
    ( mkNetworkLayer
    ) where

import Prelude

import Cardano.ChainProducer.RustHttpBridge.Api
    ( Block, BlockHeader, EpochIndex, NetworkName, api )
import Cardano.ChainProducer.RustHttpBridge.NetworkLayer
    ( NetworkLayer (..), NetworkLayerError (..) )
import Control.Monad.Except
    ( ExceptT (..), throwError )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( convert )
import Network.HTTP.Client
    ( Manager )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl, ClientM, client, mkClientEnv, runClientM )
import Servant.Extra.ContentTypes
    ( Hash (..), WithHash (..) )

import qualified Cardano.ChainProducer.RustHttpBridge.Api as Api
import qualified Cardano.Wallet.Primitive as Primitive

-- | Retrieve a block identified by the unique hash of its header.
getBlockByHash :: NetworkName -> Hash Blake2b_256 BlockHeader -> ClientM Block

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
getEpochById :: NetworkName -> EpochIndex -> ClientM [Block]

-- | Retrieve the header of the latest known block.
getTipBlockHeader :: NetworkName -> ClientM (WithHash Blake2b_256 BlockHeader)

getBlockByHash
    :<|> getEpochById
    :<|> getTipBlockHeader
    = client api

-- | Construct a new network layer
mkNetworkLayer :: Manager -> BaseUrl -> NetworkName -> NetworkLayer IO
mkNetworkLayer mgr baseUrl network = NetworkLayer
    { getBlock = \hash -> do
        hash' <- hashToApi' hash
        run (Api.getBlock <$> getBlockByHash network hash')
    , getEpoch = \ep -> run (map Api.getBlock <$>
        getEpochById network (Api.EpochIndex ep))
    , getNetworkTip = run (blockHeaderHash <$> getTipBlockHeader network)
    }
  where
    run query = ExceptT $ (first convertError) <$> runClientM query env
    env = mkClientEnv mgr baseUrl
    convertError = NetworkLayerError . show

blockHeaderHash
    :: WithHash algorithm BlockHeader
    -> (Primitive.Hash "BlockHeader", Primitive.BlockHeader)
blockHeaderHash (WithHash h (Api.BlockHeader bh)) =
    (Primitive.Hash (convert h), bh)

hashToApi :: HashAlgorithm a => Primitive.Hash h -> Maybe (Hash a b)
hashToApi (Primitive.Hash h) = Hash <$> digestFromByteString h

-- | Converts a Hash to the Digest type that the Api module requires.
hashToApi'
    :: (Monad m, HashAlgorithm algorithm)
    => Primitive.Hash a
    -> ExceptT NetworkLayerError m (Hash algorithm b)
hashToApi' h = case hashToApi h of
    Just h' -> pure h'
    Nothing -> throwError
        $ NetworkLayerError "hashToApi: Digest was of the wrong length"
