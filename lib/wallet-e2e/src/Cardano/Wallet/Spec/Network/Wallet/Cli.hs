module Cardano.Wallet.Spec.Network.Wallet.Cli where

import qualified Cardano.Wallet.Spec.Data.Network.NodeStatus as NodeStatus
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.String as String

import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..) )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus )
import Data.Aeson
    ( withObject, (.:) )
import Data.Aeson.Types
    ( explicitParseField )
import Data.Time
    ( UTCTime )
import Prelude hiding
    ( stderr, stdout )
import System.Process.Typed
    ( ExitCode (..), readProcess, shell )

data Error
    = CliErrorDecode (Aeson.JSONPath, String) LByteString
    | CliErrorExitCode Int LByteString
    deriving stock (Eq, Show)

queryNetworkInformation :: WalletApi -> IO (Either Error NetworkInformation)
queryNetworkInformation walletApi = do
    (exitCode, stdout, stderr) <-
        readProcess . shell
            $ String.unwords
                [ "cardano-wallet"
                , "network"
                , "information"
                , "--port"
                , show (walletInstanceApiPort walletApi)
                ]
    case exitCode of
        ExitFailure code -> pure $ Left $ CliErrorExitCode code stderr
        ExitSuccess -> do
            let parser = Aeson.iparse parseNetworkInformation
            case Aeson.eitherDecodeWith Aeson.value parser stdout of
                Left err -> pure $ Left $ CliErrorDecode err stdout
                Right tip -> pure $ Right tip

--------------------------------------------------------------------------------
-- Data types ------------------------------------------------------------------

data NetworkInformation = NetworkInformation
    { networkInfo :: NetworkInfo
    , networkTip :: NetworkTip
    , nextEpoch :: NextEpoch
    , nodeEra :: Text
    , nodeTip :: NodeTip
    , nodeStatus :: NodeStatus
    , walletMode :: Text
    }
    deriving stock (Eq, Show)

parseNetworkInformation :: Aeson.Value -> Aeson.Parser NetworkInformation
parseNetworkInformation =
    withObject "NetworkInformation" \o -> do
        networkInfo <- explicitParseField parseNetworkInfo o "network_info"
        networkTip <- explicitParseField parseNetworkTip o "network_tip"
        nextEpoch <- explicitParseField parseNextEpoch o "next_epoch"
        nodeEra <- o .: "node_era"
        nodeTip <- explicitParseField parseNodeTip o "node_tip"
        syncProgress <- o .: "sync_progress"
        syncStatus <- withObject "sync_progress" (.: "status") syncProgress
        nodeStatus <-
            case NodeStatus.fromString syncStatus of
                Nothing -> fail $ "Invalid sync_progress value: " <> syncStatus
                Just status -> pure status
        walletMode <- o .: "wallet_mode"
        pure NetworkInformation{..}

data NetworkInfo = NetworkInfo
    { networkId :: Text
    , protocolMagic :: Natural
    }
    deriving stock (Eq, Show)

parseNetworkInfo :: Aeson.Value -> Aeson.Parser NetworkInfo
parseNetworkInfo = withObject "NetworkInfo" \o -> do
    networkId <- o .: "network_id"
    protocolMagic <- o .: "protocol_magic"
    pure NetworkInfo{..}

data NetworkTip = NetworkTip
    { absoluteSlotNumber :: Natural
    , epochNumber :: Natural
    , slotNumber :: Natural
    , time :: UTCTime
    }
    deriving stock (Eq, Show)

parseNetworkTip :: Aeson.Value -> Aeson.Parser NetworkTip
parseNetworkTip = withObject "NetworkTip" \o -> do
    absoluteSlotNumber <- o .: "absolute_slot_number"
    epochNumber <- o .: "epoch_number"
    slotNumber <- o .: "slot_number"
    time <- o .: "time"
    pure NetworkTip{..}

data NextEpoch = NextEpoch
    { nextEpochNumber :: Natural
    , nextEpochStartTime :: UTCTime
    }
    deriving stock (Eq, Show)

parseNextEpoch :: Aeson.Value -> Aeson.Parser NextEpoch
parseNextEpoch = withObject "NextEpoch" \o -> do
    nextEpochNumber <- o .: "epoch_number"
    nextEpochStartTime <- o .: "epoch_start_time"
    pure NextEpoch{..}

data NodeTip = NodeTip
    { nodeTipAbsoluteSlotNumber :: Natural
    , nodeTipEpochNumber :: Natural
    , nodeTipHeight :: TipHeight
    , nodeTipSlotNumber :: Natural
    , nodeTipTime :: UTCTime
    }
    deriving stock (Eq, Show)

parseNodeTip :: Aeson.Value -> Aeson.Parser NodeTip
parseNodeTip = withObject "NodeTip" \o -> do
    nodeTipAbsoluteSlotNumber <- o .: "absolute_slot_number"
    nodeTipEpochNumber <- o .: "epoch_number"
    nodeTipHeight <- explicitParseField parseTipHeight o "height"
    nodeTipSlotNumber <- o .: "slot_number"
    nodeTipTime <- o .: "time"
    pure NodeTip{..}

data TipHeight = TipHeight
    { tipHeightQuantity :: Natural
    , tipHeightUnit :: Text
    }
    deriving stock (Eq, Show)

parseTipHeight :: Aeson.Value -> Aeson.Parser TipHeight
parseTipHeight = withObject "TipHeight" \o -> do
    tipHeightQuantity <- o .: "quantity"
    tipHeightUnit <- o .: "unit"
    pure TipHeight{..}
