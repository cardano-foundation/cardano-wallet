{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module implements parts of a Jörmungandr REST API Client. It also
-- provides some top-level helpers that are built using the API and are quite
-- handy in other situations.
module Cardano.Wallet.Jormungandr.Api.Client
    ( JormungandrClient (..)
    , mkJormungandrClient

    -- * Extra Helpers
    , getBlockHeader
    , getBlocks

    -- * Errors
    , LiftError(..)
    , ErrGetBlock (..)
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , ErrUnexpectedNetworkFailure (..)

    -- * Re-export
    , BaseUrl (..)
    , Scheme (..)
    , defaultManagerSettings
    , newManager
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api
    ( BlockId (..)
    , GetBlock
    , GetBlockDescendantIds
    , GetStakeDistribution
    , GetTipId
    , PostMessage
    , StakeApiResponse
    , api
    )
import Cardano.Wallet.Jormungandr.Binary
    ( ConfigParam (..), Fragment (..), convertBlock )
import Cardano.Wallet.Jormungandr.Compatibility
    ( softTxMaxSize )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , Hash (..)
    , SlotLength (..)
    , Tx (..)
    , TxWitness (..)
    )
import Control.Arrow
    ( left )
import Control.Exception
    ( Exception )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400, status404, status503 )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , responseBody
    , responseStatusCode
    , runClientM
    )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Links
    ( Link, safeLink )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                            Jormungandr Client
-------------------------------------------------------------------------------}

-- | Endpoints of the jormungandr REST API.
data JormungandrClient m = JormungandrClient
    { getTipId
        :: ExceptT ErrNetworkUnavailable m (Hash "BlockHeader")
    , getBlock
        :: Hash "BlockHeader"
        -> ExceptT ErrGetBlock m J.Block
    , getDescendantIds
        :: Hash "BlockHeader"
        -> Word
        -> ExceptT ErrGetDescendants m [Hash "BlockHeader"]
    , postMessage
        :: (Tx, [TxWitness])
        -> ExceptT ErrPostTx m ()
    , getInitialBlockchainParameters
        :: Hash "Genesis"
        -> ExceptT ErrGetBlockchainParams m (J.Block, BlockchainParameters)
    , getStakeDistribution
        :: ExceptT ErrNetworkUnavailable m StakeApiResponse
    }

-- | Construct a 'JormungandrClient'
mkJormungandrClient
    :: Manager -> BaseUrl -> JormungandrClient IO
mkJormungandrClient mgr baseUrl = JormungandrClient
    { getTipId = ExceptT $ do
        let ctx = safeLink api (Proxy @GetTipId)
        run (getBlockId <$> cGetTipId) >>= defaultHandler ctx

    , getBlock = \blockId -> ExceptT $ do
        run (cGetBlock (BlockId blockId)) >>= \case
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

    -- Never returns 'Left ErrPostTxProtocolFailure'. Will currently return
    -- 'Right ()' when submitting correctly formatted, but invalid transactions.
    --
    -- https://github.com/input-output-hk/jormungandr/blob/fe638a36d4be64e0c4b360ba1c041e8fa10ea024/jormungandr/src/rest/v0/message/post.rs#L25-L39
    , postMessage = \tx -> void $ ExceptT $ do
        run (cPostMessage tx) >>= \case
            Left (FailureResponse e)
                | responseStatusCode e == status400 -> do
                    let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                    return $ Left $ ErrPostTxBadRequest msg
            x -> do
                let ctx = safeLink api (Proxy @PostMessage)
                left ErrPostTxNetworkUnreachable <$> defaultHandler ctx x

    , getInitialBlockchainParameters = \block0 -> do
        jblock@(J.Block _ msgs) <- ExceptT $ run (cGetBlock (BlockId $ coerce block0)) >>= \case
            Left (FailureResponse e) | responseStatusCode e `elem` [status400, status404] ->
                return . Left . ErrGetBlockchainParamsGenesisNotFound $ block0
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId $ coerce block0)
                let networkUnreachable = ErrGetBlockchainParamsNetworkUnreachable
                left networkUnreachable <$> defaultHandler ctx x

        let params = mconcat $ mapMaybe getConfigParameters msgs
              where
                getConfigParameters = \case
                    Initial xs -> Just xs
                    _ -> Nothing

        let mpolicy = mapMaybe getPolicy params
              where
                getPolicy = \case
                    ConfigLinearFee x -> Just x
                    _ -> Nothing

        let mduration = mapMaybe getSlotDuration params
              where
                getSlotDuration = \case
                    SlotDuration x -> Just x
                    _ -> Nothing

        let mblock0Date = mapMaybe getBlock0Date params
              where
                getBlock0Date = \case
                    Block0Date x -> Just x
                    _ -> Nothing

        let mepochLength = mapMaybe getSlotsPerEpoch params
              where
                getSlotsPerEpoch = \case
                    SlotsPerEpoch x -> Just x
                    _ -> Nothing

        let mStability = mapMaybe getStability params
              where
                getStability = \case
                   EpochStabilityDepth x -> Just x
                   _ -> Nothing

        case (mpolicy, mduration, mblock0Date, mepochLength, mStability) of
            ([policy],[duration],[block0Date], [epochLength], [stability]) ->
                return
                    ( jblock
                    , BlockchainParameters
                        { getGenesisBlockHash = block0
                        , getGenesisBlockDate = block0Date
                        , getFeePolicy = policy
                        , getEpochLength = epochLength
                        , getSlotLength = SlotLength duration
                        , getTxMaxSize = softTxMaxSize
                        , getEpochStability = stability
                        }
                    )
            _ ->
                throwE $ ErrGetBlockchainParamsIncompleteParams params

    , getStakeDistribution = ExceptT $ do
        let ctx = safeLink api (Proxy @GetStakeDistribution)
        run cGetStakeDistribution >>= defaultHandler ctx
    }
  where
    run :: ClientM a -> IO (Either ServantError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    defaultHandler
        :: Link
        -> Either ServantError a
        -> IO (Either ErrNetworkUnavailable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        -- The node has started but its REST API is not ready yet to serve
        -- requests.
        Left (FailureResponse e) | responseStatusCode e == status503 ->
            return $ Left $ ErrNetworkUnreachable $ T.pack $ show e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetTipId
        :<|> cGetBlock
        :<|> cGetBlockDescendantIds
        :<|> cPostMessage
        :<|> cGetStakeDistribution
        = client api

{-------------------------------------------------------------------------------
                              Extra Helpers
-------------------------------------------------------------------------------}

-- Fetch a batch of blocks after but not including the given header.
getBlocks
    :: Monad m
    => JormungandrClient m
    -> Word
        -- ^ Batch size to be used for fetching blocks
    -> Hash "BlockHeader"
        -- ^ Block ID to start from
    -> ExceptT ErrGetBlock m [J.Block]
getBlocks j batchSize start = do
    ids <- withExceptT liftE $ getDescendantIds j start batchSize
    mapM (getBlock j) ids

-- | Get a block header corresponding to a header hash.
getBlockHeader
    :: Monad m
    => JormungandrClient m
    -> Hash "BlockHeader"
    -> ExceptT ErrGetBlock m BlockHeader
getBlockHeader j t =
    header . convertBlock <$> getBlock j t

{-------------------------------------------------------------------------------
                                Errors
-------------------------------------------------------------------------------}

data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure

data ErrGetDescendants
    = ErrGetDescendantsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetDescendantsParentNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

data ErrGetBlockchainParams
    = ErrGetBlockchainParamsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockchainParamsGenesisNotFound (Hash "Genesis")
    | ErrGetBlockchainParamsIncompleteParams [ConfigParam]
    deriving (Show, Eq)

-- | Safely lift error into a bigger type.
class LiftError lift where
    liftE :: lift

instance LiftError (ErrGetBlock -> ErrNetworkTip) where
    liftE = \case
        ErrGetBlockNotFound _ -> ErrNetworkTipNotFound
        ErrGetBlockNetworkUnreachable e -> ErrNetworkTipNetworkUnreachable e

instance LiftError (ErrNetworkUnavailable -> ErrGetBlock) where
    liftE =
        ErrGetBlockNetworkUnreachable

instance LiftError (ErrGetDescendants -> ErrGetBlock) where
    liftE = \case
        ErrGetDescendantsNetworkUnreachable e ->
            ErrGetBlockNetworkUnreachable e
        ErrGetDescendantsParentNotFound tip ->
            ErrGetBlockNotFound tip
