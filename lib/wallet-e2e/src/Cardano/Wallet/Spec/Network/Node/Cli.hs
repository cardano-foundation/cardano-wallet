module Cardano.Wallet.Spec.Network.Node.Cli where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.String as String

import Cardano.Node.Cli.Launcher
    ( NodeApi
    , nodeApiSocket
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( toFilePath
    )
import Data.Aeson
    ( withObject
    , (.:)
    )
import Prelude hiding
    ( stderr
    , stdout
    )
import System.Process.Typed
    ( ExitCode (..)
    , readProcess
    , shell
    )

data NodeTip = NodeTip
    { block :: Natural
    , epoch :: Natural
    , era :: Text
    , hash :: Text
    , slot :: Natural
    , slotInEpoch :: Natural
    , slotsToEpochEnd :: Natural
    , syncProgress :: Double
    }
    deriving stock (Eq, Show, Generic)

parseNodeTip :: Aeson.Value -> Aeson.Parser NodeTip
parseNodeTip = withObject "NodeTip" \o -> do
    block <- o .: "block"
    epoch <- o .: "epoch"
    era <- o .: "era"
    hash <- o .: "hash"
    slot <- o .: "slot"
    slotInEpoch <- o .: "slotInEpoch"
    slotsToEpochEnd <- o .: "slotsToEpochEnd"
    syncProgress <-
        o .: "syncProgress" >>= either (fail . toString) pure . readEither
    pure NodeTip{..}

data CliError
    = CliErrorExitCode Int LByteString
    | CliErrorDecode (Aeson.JSONPath, String) LByteString
    deriving stock (Eq, Show)

queryTip :: NodeApi -> IO (Either CliError NodeTip)
queryTip nodeApi = do
    (exitCode, stdout, stderr) <-
        readProcess . shell
            $ String.unwords
                [ "cardano-cli"
                , "query"
                , "tip"
                , "--testnet-magic"
                , "1"
                , "--socket-path"
                , toFilePath (nodeApiSocket nodeApi)
                ]
    case exitCode of
        ExitFailure code ->
            pure $ Left $ CliErrorExitCode code stderr
        ExitSuccess -> do
            let parser = Aeson.iparse parseNodeTip
            case Aeson.eitherDecodeWith Aeson.value parser stdout of
                Left err -> pure $ Left $ CliErrorDecode err stdout
                Right tip -> pure $ Right tip

checkSocket :: NodeApi -> IO Bool
checkSocket nodeApi = do
    (exitCode, _stdout, _stderr) <-
        readProcess . shell
            $ String.unwords
                [ "cardano-cli"
                , "query"
                , "tip"
                , "--testnet-magic"
                , "1"
                , "--socket-path"
                , toFilePath (nodeApiSocket nodeApi)
                ]
    case exitCode of
        ExitSuccess -> pure True
        ExitFailure _code -> pure False
