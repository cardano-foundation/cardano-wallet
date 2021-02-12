module Main where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerReloading, withMetadataServer )
import Control.Concurrent
    ( threadDelay )
import Options.Applicative
    ( Parser
    , argument
    , customExecParser
    , help
    , helper
    , idm
    , info
    , metavar
    , prefs
    , showHelpOnEmpty
    , str
    )

newtype MetadataServerArgs = MetadataServerArgs
    { sourceJson :: FilePath
    } deriving Show

main :: IO ()
main = do
    args <- customExecParser (prefs showHelpOnEmpty) (info opts idm)
    startMetadataServer $ sourceJson args

startMetadataServer :: FilePath -> IO ()
startMetadataServer fp =
    withMetadataServer (pure $ queryServerReloading fp) $ \(TokenMetadataServer uri) -> do
        putStrLn $ "Mock metadata server running with url " <> show uri
        threadDelay maxBound

opts :: Parser MetadataServerArgs
opts = helper <*> cmd
  where
   cmd = fmap MetadataServerArgs $ argument str $
        metavar "FILE"
        <> help (mconcat
            [ "Path to json file containing the mock data to be used."
            , "On requests, the server will read the file, filter and "
            , "return the queried metadata."
            , "\n\n"
            , "The format of the json file must match the POST /metadata/query "
            , "endpoint. That is: "
            , "  { \"subjects\": [entry1, entry2, ...] }, "
            , " where the entries match what is usually submitted to the metadata "
            , "server. E.g.:\n"
            , "https://github.com/input-output-hk/metadata-registry-testnet/blob/7eb91951a2adf6f9deff9f3fbe990abc536657fa/789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f16861707079636f696e.json"
            ])
