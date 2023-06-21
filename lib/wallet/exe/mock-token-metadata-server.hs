{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( TokenMetadataServer (..) )
import Cardano.Wallet.TokenMetadata.MockServer
    ( queryServerReloading, withMetadataServerOptions )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forever )
import Network.Wai.Middleware.RequestLogger
    ( logStdoutDev )
-- See ADP-1910
import "optparse-applicative" Options.Applicative
    ( ParserInfo, argument, auto, customExecParser, footerDoc, fullDesc, header,
    help, helper, info, long, metavar, option, optional, prefs, showHelpOnEmpty,
    str )
-- See ADP-1910
import "optparse-applicative" Options.Applicative.Help.Pretty
    ( hang, indent, line, text, (</>) )

data MetadataServerArgs = MetadataServerArgs
    { sourceJson :: FilePath
    , port :: Maybe Int
    } deriving Show

main :: IO ()
main = do
    args <- customExecParser (prefs showHelpOnEmpty) parserInfo
    startMetadataServer args

startMetadataServer :: MetadataServerArgs -> IO a
startMetadataServer MetadataServerArgs{sourceJson,port} = do
    let server = queryServerReloading sourceJson
    withMetadataServerOptions logStdoutDev port (pure server)
        $ \(TokenMetadataServer uri) -> do
            putStrLn $ "Mock metadata server running with url " <> show uri
            forever $ threadDelay maxBound

parserInfo :: ParserInfo MetadataServerArgs
parserInfo = info (helper <*> argsParser) $ mconcat
    [ fullDesc
    , footerDoc (Just docs)
    , header $ "mock-token-metadata-server"
        <> " - for testing cardano-wallet native asset metadata"
    ]
  where
    docs = mconcat
        [ p [ "This serves only the `POST /metadata/query' endpoint of Cardano "
            , "metadata-server. Other endpoints are unimplemented because they "
            , "are not used by cardano-wallet."
            ]
        , p [ "The OpenAPI specification of the real metadata-server is here:" ]
        , code $ "https://github.com/input-output-hk/metadata-server"
            <> "/blob/master/specifications/api/openapi.yaml"
        , p [ "To use this, create a `registry.json' file in the same format "
            , "of that returned by the metadata-server query endpoint. That is:"
            ]
        , code "{ \"subjects\": [entry1, entry2, ...] }"
        , p [ "Each entry should match what is usually submitted to the "
            , "metadata server. There are some example json files in this "
            , "registry:"
            ]
        , code "https://github.com/input-output-hk/metadata-registry-testnet"
        , p [ "To connect your wallet backend to the mock metadata server, use "
            , "the URL which is printed out when this program starts."
            ]
        , p [ "Pass the URL to the wallet like so:" ]
        , code "cardano-wallet serve --token-metadata-server URL ..."
        , p [ "On every request, the server will read the file, filter and "
            , "return the queried metadata. So you don't need to restart the "
            , "mock server after editing metadata."
            ]
        , p [ "You can make test requests to the metadata server using curl: " ]
        , code $ hang 2 $
            "curl -i -H \"Content-type: application/json\" \\" <> line <>
            "--data '{\"subjects\":[\"entry1\", \"entry2\", ...]," <>
            "\"properties\":[\"name\",\"description\"]}' \\"
            <> line <> "http://localhost:PORT/metadata/query"
        ]
    p = (<> sep) . foldr ((</>) . text) mempty . words . mconcat
    sep = line <> line
    code d = indent 2 d <> sep

    argsParser = MetadataServerArgs <$> fileArg <*> portOpt
    fileArg = argument str $
        metavar "FILE" <>
        help "Path to the JSON file containing metadata."

    portOpt = optional $ option auto $
        long "port" <>
        metavar "PORT" <>
        help "Port to listen on (default: random unused port)"
