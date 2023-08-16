{-# LANGUAGE PackageImports #-}

module  Cardano.Wallet.CLI
    ( networkConfigurationOption
    , nodeSocketOption
    )
    where

import Prelude

import Cardano.Launcher.Node
import Cardano.Wallet.Network.Config
    ( NetworkConfiguration (..) )
import Cardano.Wallet.Options
    ( optionT )
import Data.Bifunctor
    ( first )
import "optparse-applicative" Options.Applicative

-- | --mainnet --shelley-genesis=FILE
-- --testnet --byron-genesis=FILE --shelley-genesis=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption = mainnet <|> testnet
  where
    mainnet = mainnetFlag
    testnet = TestnetConfig <$> genesisFileOption "byron" "testnet"

    mainnetFlag = flag' MainnetConfig $ mempty
        <> long "mainnet"
        <> help "Use Cardano mainnet protocol"

    genesisFileOption :: String -> String -> Parser FilePath
    genesisFileOption era net = optionT $ mempty
        <> long net
        <> metavar "FILE"
        <> help ("Path to the " <> era <> " genesis data in JSON format.")

-- | --node-socket=FILE
nodeSocketOption :: Parser CardanoNodeConn
nodeSocketOption = option (eitherReader (addHelp . cardanoNodeConn)) $ mempty
    <> long "node-socket"
    <> metavar (if isWindows then "PIPENAME" else "FILE")
    <> help helpText
  where
    helpText = mconcat
        [ "Path to the node's domain socket file (POSIX) "
        , "or pipe name (Windows). "
        , "Note: Maximum length for POSIX socket files is approx. 100 bytes. "
        , "Note:", pipeHelp ]
    pipeHelp = " Windows named pipes are of the form \\\\.\\pipe\\cardano-node"
    addHelp = first (if isWindows then (++ pipeHelp) else id)
