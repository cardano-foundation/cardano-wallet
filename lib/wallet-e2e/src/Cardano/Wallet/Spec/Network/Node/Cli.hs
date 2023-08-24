module Cardano.Wallet.Spec.Network.Node.Cli where

import qualified Data.Aeson as Aeson
import qualified Data.String as String

import Data.Aeson
    ( FromJSON, withObject, (.:) )
import Path
    ( Abs, File, Path, toFilePath )
import Prelude hiding
    ( stderr, stdout )
import System.Process.Typed
    ( ExitCode (..), readProcess, shell )

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

instance FromJSON NodeTip where
    parseJSON = withObject "NodeTip" \o -> do
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
    | CliErrorDecode String LByteString
    deriving stock (Eq, Show)

queryTip :: Path Abs File -> IO (Either CliError NodeTip)
queryTip nodeSocket = do
    (exitCode, stdout, stderr) <-
        readProcess . shell
            $ String.unwords
                [ "cardano-cli"
                , "query"
                , "tip"
                , "--testnet-magic"
                , "1"
                , "--socket-path"
                , toFilePath nodeSocket
                ]
    case exitCode of
        ExitFailure code -> pure $ Left $ CliErrorExitCode code stderr
        ExitSuccess -> case Aeson.eitherDecode stdout of
            Left err -> pure $ Left $ CliErrorDecode err stdout
            Right tip -> pure $ Right tip
