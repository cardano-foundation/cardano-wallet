{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
--
--
-- This module allows the wallet to retrieve blocks from a known @Jormungandr@
-- node. This is done by providing a @NetworkLayer@ with some logic building on
-- top of an underlying @JormungandrLayer@ HTTP client.
module Cardano.Wallet.Jormungandr.Network
    ( newNetworkLayer
    , mkNetworkLayer
    , JormungandrLayer (..)
    , mkJormungandrLayer

    -- * Exception
    , ErrUnexpectedNetworkFailure (..)

    -- * Re-export
    , BaseUrl (..)
    , newManager
    , defaultManagerSettings
    , Scheme (..)
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api
    ( BlockId (..), GetBlock, GetBlockDescendantIds, GetTipId, api )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnreachable (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..) )
import Control.Arrow
    ( left )
import Control.Exception
    ( Exception )
import Control.Monad
    ( forM )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Except
    ( ExceptT (..), withExceptT )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400 )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , responseStatusCode
    , runClientM
    )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Links
    ( Link, safeLink )

-- | Creates a new 'NetworkLayer' connecting to an underlying 'Jormungandr'
-- backend target.
newNetworkLayer
    :: forall n. ()
    => BaseUrl
    -> IO (NetworkLayer (Jormungandr n) IO)
newNetworkLayer url = do
    mgr <- newManager defaultManagerSettings
    return $ mkNetworkLayer $ mkJormungandrLayer mgr url

-- | Wrap a Jormungandr client into a 'NetworkLayer' common interface.
mkNetworkLayer
    :: Monad m
    => JormungandrLayer m
    -> NetworkLayer t m
mkNetworkLayer j = NetworkLayer
    { networkTip = do
        t <- (getTipId j) `mappingError`
            ErrNetworkTipNetworkUnreachable
        b <- (getBlock j t) `mappingError` \case
            ErrGetBlockNotFound _ ->
                ErrNetworkTipNotFound
            ErrGetBlockNetworkUnreachable e ->
                ErrNetworkTipNetworkUnreachable e
        return $ header b

    , nextBlocks = \tip -> do
        let count = 10000
        -- Get the descendants of the tip's /parent/.
        -- The first descendant is therefore the current tip itself. We need to
        -- skip it. Hence the 'tail'.
        ids <- tailOrEmpty <$> getDescendantIds j (prevBlockHash tip) count
                `mappingError` \case
            ErrGetDescendantsNetworkUnreachable e ->
                ErrGetBlockNetworkUnreachable e
            ErrGetDescendantsParentNotFound _ ->
                ErrGetBlockNotFound (prevBlockHash tip)
        forM ids (getBlock j)

    , postTx = error "postTx to be implemented"
    }
  where
    mappingError = flip withExceptT

    tailOrEmpty [] = []
    tailOrEmpty (_:xs) = xs

{-------------------------------------------------------------------------------
                            Jormungandr Client
-------------------------------------------------------------------------------}

-- | Endpoints of the jormungandr REST API.
data JormungandrLayer m = JormungandrLayer
    { getTipId
        :: ExceptT ErrNetworkUnreachable m (Hash "BlockHeader")
    , getBlock
        :: Hash "BlockHeader" -> ExceptT ErrGetBlock m Block
    , getDescendantIds
        :: Hash "BlockHeader"
        -> Word
        -> ExceptT ErrGetDescendants m [Hash "BlockHeader"]
    }

-- | Construct a 'JormungandrLayer'-client
--
-- >>> mgr <- newManager defaultManagerSettings
-- >>> j = mkJormungandrLayer mgr (BaseUrl Http "localhost" 8080 "")
--
-- >>> (Right tip) <- runExceptT $ getTipId j
-- >>> tip
-- BlockId (Hash {getHash = "26c640a3de09b74398c14ca0a137ec78"})
--
-- >>> (Right block) <- runExceptT $ getBlock j t
-- >>> block
-- >>> Block {header = BlockHeader {slotId = SlotId {epochNumber = 0, slotNumber = 0}, prevBlockHash = Hash {getHash = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"}}, transactions = [Tx {inputs = [], outputs = [TxOut {address = Address {unAddress = "3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"}, coin = Coin {getCoin = 14}}]}]}
--
-- At the time of writing, we only have the genesis-block, but we should be
-- able to get its descendants.
--
-- >>> let genesisHash = BlockId (Hash {getHash = "&\198@\163\222\t\183C\152\193L\160\161\&7\236x\245\229\EOT\175\177\167\131\190\b\b/\174\212\177:\179"})
-- >>> runExceptT $ getDescendantIds j t 4
-- Right []
mkJormungandrLayer
    :: Manager -> BaseUrl -> JormungandrLayer IO
mkJormungandrLayer mgr baseUrl = JormungandrLayer
    { getTipId = ExceptT $ do
        let ctx = safeLink api (Proxy @GetTipId)
        run (getBlockId <$> cGetTipId) >>= defaultHandler ctx

    , getBlock = \blockId -> ExceptT $ do
        run (cGetBlock (BlockId blockId))  >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
              return . Left . ErrGetBlockNotFound $ blockId
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId blockId)
                left ErrGetBlockNetworkUnreachable <$> defaultHandler ctx x

    , getDescendantIds = \parentId count -> ExceptT $ do
        run (map getBlockId <$> cGetBlockDescendantIds (BlockId parentId) (Just count))  >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
              return . Left $ ErrGetDescendantsParentNotFound parentId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlockDescendantIds)
                        (BlockId parentId)
                        (Just count)
                left ErrGetDescendantsNetworkUnreachable <$> defaultHandler ctx x
    }
  where
    run :: ClientM a -> IO (Either ServantError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    defaultHandler
        :: Link
        -> Either ServantError a
        -> IO (Either ErrNetworkUnreachable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetTipId
        :<|> cGetBlock
        :<|> cGetBlockDescendantIds
        = client api

data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure

data ErrGetDescendants
    = ErrGetDescendantsNetworkUnreachable ErrNetworkUnreachable
    | ErrGetDescendantsParentNotFound (Hash "BlockHeader")
    deriving (Show, Eq)
