{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..), FromText (..), TextDecodingError (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..), WalletName )
import Control.Exception
    ( finally )
import Data.Text
    ( Text )
import System.Console.Docopt
    ( Arguments
    , Docopt
    , Option
    , command
    , docoptFile
    , exitWithUsage
    , getArg
    , getArgOrExitWith
    , isPresent
    , longOption
    , parseArgsOrExit
    )
import System.Environment
    ( getArgs )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stdout )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.ANSI as ANSI

patterns :: Docopt
patterns = [docoptFile|app/wallet/USAGE.txt|]

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    hSetBuffering stdout NoBuffering
    print =<< parseCommand args

{-------------------------------------------------------------------------------
                          Command and Argument Parsing
-------------------------------------------------------------------------------}

parseCommand :: Arguments -> IO Command
parseCommand args
    | args `isPresent` command "address" =
        AddressCommand
            <$> parseAddressCommand args
    | args `isPresent` command "wallet" =
        WalletCommand
            <$> parseWalletCommand args
    | otherwise =
        exitWithUsage patterns

parseAddressCommand :: Arguments -> IO AddressCommand
parseAddressCommand args
    | args `isPresent` command "list" =
        AddressList <$> addressListOptions
    | otherwise =
        exitWithUsage patterns
  where
    addressListOptions = AddressListOptions
        <$> args `parseArg` longOption "wallet-id"

parseWalletCommand :: Arguments -> IO WalletCommand
parseWalletCommand args
    | args `isPresent` command "create" =
        WalletCreate <$> walletCreateOptions
    | args `isPresent` command "delete" =
        WalletDelete <$> walletDeleteOptions
    | args `isPresent` command "list" =
        pure WalletList
    | args `isPresent` command "update" =
        WalletUpdate <$> walletUpdateOptions
    | otherwise =
        exitWithUsage patterns
  where
    walletCreateOptions = WalletCreateOptions
        <$> args `parseArgMaybe` longOption "address-pool-gap"
        <*> args `parseArg` longOption "name"
    walletDeleteOptions = WalletDeleteOptions
        <$> args `parseArg` longOption "id"
    walletUpdateOptions = WalletUpdateOptions
        <$> args `parseArg` longOption "id"
        <*> args `parseArg` longOption "name"

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseArg :: FromText a => Arguments -> Option -> IO a
parseArg args option =
    either (fail . getTextDecodingError) pure . fromText . T.pack
        =<< args `getArgOrExit` option

parseArgMaybe :: FromText a => Arguments -> Option -> IO (Maybe a)
parseArgMaybe args option = maybe
    (pure Nothing)
    (either (fail . getTextDecodingError) (pure . pure) . fromText)
    (T.pack <$> args `getArg` option)

{-------------------------------------------------------------------------------
                                 Commands
-------------------------------------------------------------------------------}

data Command
    = AddressCommand AddressCommand
    | WalletCommand WalletCommand
    deriving (Eq, Show)

newtype AddressCommand
    = AddressList AddressListOptions
    deriving (Eq, Show)

newtype AddressListOptions = AddressListOptions
    { walletId :: ApiT WalletId
    } deriving (Eq, Show)

data WalletCommand
    = WalletCreate WalletCreateOptions
    | WalletDelete WalletDeleteOptions
    | WalletList
    | WalletUpdate WalletUpdateOptions
    deriving (Eq, Show)

data WalletCreateOptions = WalletCreateOptions
    { addressPoolGap :: Maybe (ApiT AddressPoolGap)
    , name :: ApiT WalletName
    } deriving (Eq, Show)

newtype WalletDeleteOptions = WalletDeleteOptions
    { id :: ApiT WalletId
    } deriving (Eq, Show)

data WalletUpdateOptions = WalletUpdateOptions
    { id :: ApiT WalletId
    , name :: ApiT WalletName
    } deriving (Eq, Show)

{-------------------------------------------------------------------------------
                                 Utilities
-------------------------------------------------------------------------------}

-- | Read a line of user input containing sensitive data from the terminal.
--
-- The terminal lines containing the data are cleared once the user has finished
-- entering data.
--
-- The terminal lines containing the data are also cleared if the application
-- exits abnormally, before the user has finished entering data.
--
getLineWithSensitiveData :: IO Text
getLineWithSensitiveData =
    finally TIO.getLine . clearSensitiveData =<< ANSI.getCursorPosition0
  where
    clearSensitiveData cursorPosition = case cursorPosition of
        Just (_, y) -> do
            -- We know the original position of the cursor.
            -- Just clear everything from that line onwards.
            ANSI.setCursorPosition 0 y
            ANSI.clearFromCursorToScreenEnd
        Nothing ->
            -- We don't know the original position of the cursor.
            -- For safety, we must clear the entire screen.
            ANSI.clearScreen
