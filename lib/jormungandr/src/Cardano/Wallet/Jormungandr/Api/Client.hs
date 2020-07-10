{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
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
    , ErrGetAccountState (..)
    , ErrGetBlock (..)
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrCurrentNodeTip (..)
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
    ( GetAccountState
    , GetBlock
    , GetBlockDescendantIds
    , GetStakeDistribution
    , GetTipId
    , PostMessage
    , api
    )
import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountId (..), AccountState, ApiT (..), BlockId (..), StakeApiResponse )
import Cardano.Wallet.Jormungandr.Binary
    ( ConfigParam (..)
    , Fragment (..)
    , Milli (..)
    , convertBlock
    , overrideFeePolicy
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( softTxMaxSize )
import Cardano.Wallet.Jormungandr.Rewards
    ( PoolCapping (..) )
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , Hash (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SealedTx
    , SlotLength (..)
    , TxParameters (..)
    )
import Control.Arrow
    ( left )
import Control.Exception
    ( Exception, SomeException, handle, throwIO )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.List
    ( isSubsequenceOf )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400, status404, status500, status503 )
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
    ( ClientError (..) )
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
    { getAccountState
        :: Hash "Account"
        -> ExceptT ErrGetAccountState m AccountState
    , getTipId
        :: ExceptT ErrNetworkUnavailable m (Hash "BlockHeader")
    , getBlock
        :: Hash "BlockHeader"
        -> ExceptT ErrGetBlock m J.Block
    , getDescendantIds
        :: Hash "BlockHeader"
        -> Word
        -> ExceptT ErrGetDescendants m [Hash "BlockHeader"]
    , postMessage
        :: SealedTx
        -> ExceptT ErrPostTx m ()
    , getInitialBlockchainParameters
        :: Hash "Genesis"
        -> ExceptT ErrGetBlockchainParams m (J.Block, NetworkParameters)
    , getStakeDistribution
        :: EpochNo
        -> ExceptT ErrNetworkUnavailable m StakeApiResponse
    }

-- | Construct a 'JormungandrClient'
mkJormungandrClient
    :: Manager -> BaseUrl -> JormungandrClient IO
mkJormungandrClient mgr baseUrl = JormungandrClient
    { getAccountState = \accountId -> ExceptT $ do
        let action = cGetAccountState (AccountId accountId)
        run action >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status404 ->
                return $ Left $ ErrGetAccountStateAccountNotFound accountId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetAccountState)
                        (AccountId accountId)
                left ErrGetAccountStateNetworkUnreachable
                    <$> defaultHandler ctx x

    , getTipId = ExceptT $ do
        run (getBlockId <$> cGetTipId) >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status500 ->
                -- NOTE: /api/v0/tip returns 500 Internal Server Error when
                -- called during bootstrap. Treat this status the same as 503
                -- Service Unavailable, so that getTipId can be polled until
                -- jormungandr is ready.
                return $ Left $ ErrNetworkUnreachable $ T.pack $ show e
            x -> do
                let ctx = safeLink api (Proxy @GetTipId)
                defaultHandler ctx x

    , getBlock = \blockId -> ExceptT $ do
        let action = cGetBlock (BlockId blockId)
        run action >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status404 ->
                return $ Left $ ErrGetBlockNotFound blockId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlock)
                        (BlockId blockId)
                left ErrGetBlockNetworkUnreachable <$> defaultHandler ctx x

    , getDescendantIds = \parentId count -> ExceptT $ do
        let action = map getBlockId <$>
                cGetBlockDescendantIds (BlockId parentId) (Just count)
        run action >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status404 ->
                return $ Left $ ErrGetDescendantsParentNotFound parentId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlockDescendantIds)
                        (BlockId parentId)
                        (Just count)
                left ErrGetDescendantsNetworkUnreachable <$>
                    defaultHandler ctx x

    -- Never returns 'Left ErrPostTxProtocolFailure'. Will currently return
    -- 'Right ()' when submitting correctly formatted, but invalid transactions.
    --
    -- https://github.com/input-output-hk/jormungandr/blob/fe638a36d4be64e0c4b360ba1c041e8fa10ea024/jormungandr/src/rest/v0/message/post.rs#L25-L39
    , postMessage = \tx -> void $ ExceptT $ do
        run (cPostMessage tx) >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status400 -> do
                let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                return $ Left $ ErrPostTxBadRequest msg
            x -> do
                let ctx = safeLink api (Proxy @PostMessage)
                left ErrPostTxNetworkUnreachable <$> defaultHandler ctx x

    , getInitialBlockchainParameters = \block0 -> do
        let action = cGetBlock $ BlockId $ coerce block0
        jblock@(J.Block _ msgs) <- ExceptT $ run action >>= \case
            Left (FailureResponse _ e) | responseStatusCode e == status404 ->
                return
                    $ Left
                    $ ErrGetBlockchainParamsGenesisNotFound block0
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlock)
                        (BlockId $ coerce block0)
                let networkUnreachable =
                        ErrGetBlockchainParamsNetworkUnreachable
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

        let mperCertFee = mapMaybe getPerCertFee params
              where
                getPerCertFee = \case
                    ConfigPerCertificate x -> Just x
                    _ -> Nothing

        let mduration = mapMaybe getSlotDuration params
              where
                getSlotDuration = \case
                    SlotDuration x -> Just x
                    _ -> Nothing

        let mblock0T = mapMaybe getBlock0T params
              where
                getBlock0T = \case
                    Block0Date x -> Just x
                    _ -> Nothing

        let mepLength = mapMaybe getSlotsPerEpoch params
              where
                getSlotsPerEpoch = \case
                    SlotsPerEpoch x -> Just x
                    _ -> Nothing

        let mStability = mapMaybe getStability params
              where
                getStability = \case
                   EpochStabilityDepth x -> Just x
                   _ -> Nothing

        let mcoeff = mapMaybe getCoeff params
              where
                getCoeff = \case
                    ConsensusGenesisPraosParamF (Milli f) ->
                        Just $ ActiveSlotCoefficient $ fromIntegral f / 1000
                    _ -> Nothing

        let mpoolCapping = mapMaybe getPoolCapping params
              where
                getPoolCapping = \case
                    ConfigPoolCapping (PoolCapping _ x) -> Just x
                    _ -> Nothing

        case (mpolicy, mduration, mblock0T, mepLength, mStability, mcoeff, mpoolCapping) of
            ([policy],[duration],[block0T], [epLength], [stability],[coeff],[poolcapping]) ->
                return
                    ( jblock
                    , NetworkParameters
                        { genesisParameters = GenesisParameters
                            { getGenesisBlockHash = block0
                            , getGenesisBlockDate = block0T
                            , getEpochLength = epLength
                            , getSlotLength = SlotLength duration
                            , getEpochStability = stability
                            , getActiveSlotCoefficient = coeff
                            }
                        , protocolParameters = ProtocolParameters
                            { decentralizationLevel =
                                minBound
                            , txParameters = TxParameters
                                { getFeePolicy = case mperCertFee of
                                    [override] ->
                                        overrideFeePolicy policy override
                                    _ ->
                                        policy
                                , getTxMaxSize = softTxMaxSize
                                }
                            , desiredNumberOfStakePools = fromIntegral poolcapping
                            , minimumUTxOvalue = Coin 0
                            }
                        }
                    )
            _ ->
                throwE $ ErrGetBlockchainParamsIncompleteParams params

    , getStakeDistribution = \ep -> ExceptT $ do
        let ctx = safeLink api (Proxy @GetStakeDistribution) (ApiT ep)
        run (cGetStakeDistribution (ApiT ep)) >>= defaultHandler ctx
    }
  where
    run :: ClientM a -> IO (Either ClientError a)
    run query = handle windowsNetworkException $
        runClientM query (mkClientEnv mgr baseUrl)
      where
        windowsNetworkException :: SomeException -> IO (Either ClientError a)
        windowsNetworkException e
            | "WSAECONNREFUSED" `isSubsequenceOf` show e =
                pure $ Left $ ConnectionError e
            | otherwise =
                throwIO e

    defaultHandler
        :: Link
        -> Either ClientError a
        -> IO (Either ErrNetworkUnavailable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable $ T.pack $ show e

        -- The node has started but its REST API is not ready yet to serve
        -- requests.
        Left (FailureResponse _ e) | responseStatusCode e == status503 ->
            return $ Left $ ErrNetworkUnreachable $ T.pack $ show e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetAccountState
        :<|> cGetTipId
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
    = ErrUnexpectedNetworkFailure Link ClientError
    deriving (Generic, Show)

instance Exception ErrUnexpectedNetworkFailure

data ErrGetAccountState
    = ErrGetAccountStateNetworkUnreachable ErrNetworkUnavailable
    | ErrGetAccountStateAccountNotFound (Hash "Account")
    deriving (Generic, Eq, Show)

data ErrGetDescendants
    = ErrGetDescendantsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetDescendantsParentNotFound (Hash "BlockHeader")
    deriving (Generic, Show, Eq)

data ErrGetBlockchainParams
    = ErrGetBlockchainParamsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockchainParamsGenesisNotFound (Hash "Genesis")
    | ErrGetBlockchainParamsIncompleteParams [ConfigParam]
    deriving (Generic, Show, Eq)

-- | Safely lift error into a bigger type.
class LiftError lift where
    liftE :: lift

instance LiftError (ErrGetBlock -> ErrCurrentNodeTip) where
    liftE = \case
        ErrGetBlockNotFound _ -> ErrCurrentNodeTipNotFound
        ErrGetBlockNetworkUnreachable e -> ErrCurrentNodeTipNetworkUnreachable e

instance LiftError (ErrNetworkUnavailable -> ErrGetBlock) where
    liftE =
        ErrGetBlockNetworkUnreachable

instance LiftError (ErrGetDescendants -> ErrGetBlock) where
    liftE = \case
        ErrGetDescendantsNetworkUnreachable e ->
            ErrGetBlockNetworkUnreachable e
        ErrGetDescendantsParentNotFound tip ->
            ErrGetBlockNotFound tip
