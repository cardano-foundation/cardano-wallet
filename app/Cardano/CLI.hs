{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}


-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Shared types and helpers for CLI parsing

module Cardano.CLI
    ( getArg
    , Port
    , Network
    , encode
    , decode
    ) where

import Prelude

import Cardano.Wallet.Mnemonic
    ( Mnemonic, mkMnemonic )
import Control.Monad
    ( when )
import Data.Bifunctor
    ( first )
import GHC.TypeLits
    ( Symbol )
import System.Console.Docopt
    ( Arguments, Docopt, Option, exitWithUsage, getAllArgs, getArgOrExitWith )
import Text.Read
    ( readMaybe )

import qualified Data.Text as T


-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port Int

data Network = Mainnet | Testnet
    deriving (Show, Enum)


class GetArg from where
    getArg
        :: Arguments
        -> Docopt
        -> Option
        -> (from -> Either String to)
        -> IO to

instance GetArg String where
    getArg args cli opt decod = do
        str <- getArgOrExitWith cli args opt
        case decod str of
            Right a -> return a
            Left err -> do
                putStrLn $ "Invalid " <> show opt <> ". " <> err
                putStrLn ""
                exitWithUsage cli

instance GetArg [String] where
    getArg args cli opt decod = do
        let str = getAllArgs args opt
        when (null str) $ exitWithUsage cli
        case decod str of
            Right a -> return a
            Left err -> do
                putStrLn $ "Invalid " <> show opt <> ". " <> err
                putStrLn ""
                exitWithUsage cli

-- | Encoding things into command line arguments
class Encodable from to where
    encode :: from -> to

-- | Decoding command line arguments
class Decodable from to where
    decode :: from -> Either String to

instance Encodable Int String where
    encode = show

instance Decodable String Int where
    decode str =
        maybe (Left err) Right (readMaybe str)
      where
        err = "Not an integer: " ++ show str ++ "."

instance Encodable (Port tag) String where
    encode (Port p) = encode p

instance Decodable String (Port tag) where
    decode str = Port <$> decode str

instance Encodable Network String where
    encode Mainnet = "mainnet"
    encode Testnet = "testnet"

instance Decodable String Network where
    decode "mainnet" = Right Mainnet
    decode "testnet" = Right Testnet
    decode s = Left $ show s ++ " is neither \"mainnet\" nor \"testnet\"."

instance Decodable [String] (Mnemonic 15) where
    decode ws = first show $ mkMnemonic @15 (T.pack <$> ws)
