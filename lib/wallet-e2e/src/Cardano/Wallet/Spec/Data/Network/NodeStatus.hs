module Cardano.Wallet.Spec.Data.Network.NodeStatus where

import qualified Wallet as W

data NodeStatus = NodeIsSynced | NodeIsSyncing | NodeIsNotResponding
    deriving stock (Eq, Show)

fromString :: String -> Maybe NodeStatus
fromString "ready" = Just NodeIsSynced
fromString "syncing" = Just NodeIsSyncing
fromString "not_responding" = Just NodeIsNotResponding
fromString _ = Nothing

toString :: NodeStatus -> String
toString NodeIsSynced = "ready"
toString NodeIsSyncing = "syncing"
toString NodeIsNotResponding = "not_responding"

fromClientResponse
    :: W.GetNetworkInformationResponseBody200Sync_progress -> Maybe NodeStatus
fromClientResponse
    (W.GetNetworkInformationResponseBody200Sync_progress _ status) =
        case status of
            W.GetNetworkInformationResponseBody200Sync_progressStatusOther{} ->
                Nothing
            W.GetNetworkInformationResponseBody200Sync_progressStatusTyped{} ->
                Nothing
            W.GetNetworkInformationResponseBody200Sync_progressStatusEnumReady ->
                Just NodeIsSynced
            W.GetNetworkInformationResponseBody200Sync_progressStatusEnumSyncing ->
                Just NodeIsSyncing
            W.GetNetworkInformationResponseBody200Sync_progressStatusEnumNot_responding ->
                Just NodeIsNotResponding
