{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module CLI where

import GHC.TypeLits
    ( Symbol )
import Prelude
import System.Console.Docopt
    ( Arguments, Docopt, Option, exitWithUsage, getArgOrExitWith )
import Text.Read
    ( readMaybe )


-- Shared types and helpers for CLI parsing


-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port
    { getPort :: Int
    }


data Network = Mainnet | Testnet
    deriving (Show, Enum)


getArg
    :: Arguments
    -> Docopt
    -> Option
    -> (String -> Either String a)
    -> IO a
getArg args cli opt decod = do
    str <- getArgOrExitWith cli args opt
    case decod str of
        Right a -> return a
        Left err -> do
            putStrLn $ "Invalid " <> show opt <> ". " <> err
            putStrLn ""
            exitWithUsage cli

-- | Encoding things into command line arguments
class Encodable a where
    encode :: a -> String


-- | Decoding command line arguments
class Decodable a where
    decode :: String -> Either String a

instance Encodable Int where
    encode = show

instance Decodable Int where
    decode str =
        maybe (Left err) Right (readMaybe str)
      where
        err = "Not an integer: " ++ show str ++ "."

instance Encodable (Port (tag :: Symbol)) where
    encode (Port p) = encode p

instance Decodable (Port (tag :: Symbol))where
    decode str = Port <$> decode str

instance Encodable Network where
    encode Mainnet = "mainnet"
    encode Testnet = "testnet"

instance Decodable Network where
    decode "mainnet" = Right Mainnet
    decode "testnet" = Right Testnet
    decode s = Left $ show s ++ " is neither \"mainnet\" nor \"testnet\"."
