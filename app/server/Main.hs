{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Cardano.Wallet
    ( sayHello )
import Data.Data
import Prelude
import System.Console.CmdArgs

data Network = Mainnet | Testnet
    deriving (Data, Typeable, Show, Eq)

-- | Record for storing CLI parameters.
--
-- Naming conventions according to
-- https://hackage.haskell.org/package/cmdargs-0.4
data WalletServer = WalletServer
    { network
        :: Network
    , nodePort
        :: Int
    , walletServerPort
        :: Int
    } deriving (Show, Data, Typeable)

walletServer :: WalletServer
walletServer = WalletServer
    { network = enum
        [ Mainnet &= help "mainnet"
        , Testnet &= help "testnet"
        ]
    , nodePort =
         8080 &= help "port used for node-wallet communication"
    , walletServerPort =
         8090 &= help "port used for serving the wallet API"
    } &= summary "Cardano Wallet vX.X.X"


main :: IO ()
main = do
    sayHello
    print =<< cmdArgs walletServer
