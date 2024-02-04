{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.Api.Http.Server.Handlers.NetworkInformation where

import Prelude

import Cardano.Api
    ( NetworkId
    , toNetworkMagic
    , unNetworkMagic
    )
import Cardano.Wallet.Api.Types
    ( ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiNetworkInformation
    , ApiSlotId (..)
    , ApiSlotReference (..)
    , ApiT (..)
    , ApiWalletMode (..)
    , toApiEra
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    , timeInterpreter
    )
import Cardano.Wallet.Pools
    ( EpochInfo (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( RelativeTime
    , TimeInterpreter
    , currentRelativeTime
    , hoistTimeInterpreter
    , interpretQuery
    , neverFails
    , ongoingSlotAt
    , slotToUTCTime
    , timeOfEpoch
    , toSlotId
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , SlotId
    , SlotNo (..)
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    , exceptToMaybeT
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.Quantity
    ( Quantity (..)
    )
import Data.Word
    ( Word32
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Server
    ( Handler (..)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Api.Types as Api

getNetworkInformation
    :: HasCallStack
    => NetworkId
    -> NetworkLayer IO block
    -> ApiWalletMode
    -> Handler ApiNetworkInformation
getNetworkInformation
    nid
    NetworkLayer
        { syncProgress
        , currentNodeTip
        , currentNodeEra
        , timeInterpreter
        }
    mode = liftIO $ do
        now <- currentRelativeTime ti
        nodeTip <- currentNodeTip
        nodeEra <- currentNodeEra
        apiNodeTip <-
            makeApiBlockReferenceFromHeader
                (neverFails "node tip is within safe-zone" timeInterpreter)
                nodeTip
        nowInfo <- runMaybeT $ networkTipInfo now
        progress <- syncProgress $ view #slotNo nodeTip
        pure
            Api.ApiNetworkInformation
                { Api.syncProgress = ApiT progress
                , Api.nextEpoch = snd <$> nowInfo
                , Api.nodeTip = apiNodeTip
                , Api.networkTip = fst <$> nowInfo
                , Api.nodeEra = toApiEra nodeEra
                , Api.networkInfo =
                    Api.ApiNetworkInfo
                        ( case nid of
                            Cardano.Mainnet -> "mainnet"
                            Cardano.Testnet _ -> "testnet"
                        )
                        (fromIntegral $ unNetworkMagic $ toNetworkMagic nid)
                , Api.walletMode = mode
                }
      where
        ti :: TimeInterpreter (MaybeT IO)
        ti = hoistTimeInterpreter exceptToMaybeT timeInterpreter

        -- (network tip, next epoch)
        -- May be unavailable if the node is still syncing.
        networkTipInfo :: RelativeTime -> MaybeT IO (ApiSlotReference, EpochInfo)
        networkTipInfo now = do
            networkTipSlot <- interpretQuery ti $ ongoingSlotAt now
            tip <- makeApiSlotReference ti networkTipSlot
            let curEpoch = tip ^. #slotId . #epochNumber . #getApiT
            (_, nextEpochStart) <- interpretQuery ti $ timeOfEpoch curEpoch
            let nextEpoch = EpochInfo (succ curEpoch) nextEpochStart
            return (tip, nextEpoch)

makeApiBlockReferenceFromHeader
    :: Monad m
    => TimeInterpreter m
    -> BlockHeader
    -> m ApiBlockReference
makeApiBlockReferenceFromHeader ti tip =
    makeApiBlockReference ti (tip ^. #slotNo) (natural $ tip ^. #blockHeight)

natural :: Quantity q Word32 -> Quantity q Natural
natural = Quantity . fromIntegral . getQuantity

makeApiSlotReference
    :: Monad m
    => TimeInterpreter m
    -> SlotNo
    -> m ApiSlotReference
makeApiSlotReference ti sl =
    ApiSlotReference (ApiT sl)
        <$> fmap apiSlotId (interpretQuery ti $ toSlotId sl)
        <*> interpretQuery ti (slotToUTCTime sl)

apiSlotId :: SlotId -> ApiSlotId
apiSlotId slotId =
    ApiSlotId
        (ApiT $ slotId ^. #epochNumber)
        (ApiT $ slotId ^. #slotNumber)

makeApiBlockReference
    :: Monad m
    => TimeInterpreter m
    -> SlotNo
    -> Quantity "block" Natural
    -> m ApiBlockReference
makeApiBlockReference ti sl height = do
    slotId <- interpretQuery ti (toSlotId sl)
    slotTime <- interpretQuery ti (slotToUTCTime sl)
    pure
        ApiBlockReference
            { absoluteSlotNumber = ApiT sl
            , slotId = apiSlotId slotId
            , time = slotTime
            , block = ApiBlockInfo{height}
            }
