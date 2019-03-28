{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( WalletId (..), WalletName (..) )
import Options.Applicative
    ( Parser
    , ReadM
    , command
    , commandGroup
    , eitherReader
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , subparser
    , (<**>)
    , (<|>)
    )

import qualified Data.Text as T
import qualified Data.UUID.Types as UUID

main :: IO ()
main = run =<< execParser opts where
    opts = info
        (parseCommand <**> helper)
        (fullDesc <> progDesc "Cardano Wallet CLI")

-- TODO:
-- For the moment, we simply print the command back to the console.
-- In the future, we'll want to translate the command to an API call.
run :: Command -> IO ()
run = print

{-------------------------------------------------------------------------------
                               Command Types
-------------------------------------------------------------------------------}

data Command
    = AddressCommand AddressCommand
    | WalletCommand WalletCommand
    deriving (Eq, Show)

parseCommand :: Parser Command
parseCommand = addressCommand <|> walletCommand

{-------------------------------------------------------------------------------
                               Address Commands
-------------------------------------------------------------------------------}

newtype AddressCommand
    = AddressList AddressListOptions
    deriving (Eq, Show)

newtype AddressListOptions = AddressListOptions
    { id :: WalletId
    } deriving (Eq, Show)

addressCommand :: Parser Command
addressCommand = fmap AddressCommand $ subparser $ mconcat
    [ commandGroup
        "Address commands:"
    , command "address-list" $ info addressList $ progDesc
        "List all existing addresses."
    ]

addressList :: Parser AddressCommand
addressList = AddressList . AddressListOptions
    <$> option walletId
        (  long "wallet-id"
        <> metavar "UUID"
        <> help "the ID of a wallet" )

{-------------------------------------------------------------------------------
                               Wallet Commands
-------------------------------------------------------------------------------}

data WalletCommand
    = WalletCreate WalletCreateOptions
    | WalletDelete WalletDeleteOptions
    | WalletUpdate WalletUpdateOptions
    | WalletList
    deriving (Eq, Show)

newtype WalletCreateOptions = WalletCreateOptions
    { name :: WalletName
    } deriving (Eq, Show)

newtype WalletDeleteOptions = WalletDeleteOptions
    { id :: WalletId
    } deriving (Eq, Show)

data WalletUpdateOptions = WalletUpdateOptions
    { id :: WalletId
    , name :: WalletName
    } deriving (Eq, Show)

walletCommand :: Parser Command
walletCommand = fmap WalletCommand $ subparser $ mconcat
    [ commandGroup
        "Wallet commands:"
    , command "wallet-create" $ info walletCreate $ progDesc
        "Create a completely new wallet."
    , command "wallet-delete" $ info walletDelete $ progDesc
        "Delete an existing wallet."
    , command "wallet-update" $ info walletUpdate $ progDesc
        "Update an existing wallet's metadata."
    , command "wallet-list" $ info walletList $ progDesc
        "List all existing wallets."
    ]

walletCreate :: Parser WalletCommand
walletCreate = WalletCreate . WalletCreateOptions
    <$> option walletName
        (  long "name"
        <> metavar "STRING"
        <> help "a name for the new wallet" )

walletDelete :: Parser WalletCommand
walletDelete = WalletDelete . WalletDeleteOptions
    <$> option walletId
        (  long "id"
        <> metavar "UUID"
        <> help "the ID of the wallet to delete" )

walletUpdate :: Parser WalletCommand
walletUpdate = fmap WalletUpdate . WalletUpdateOptions
    <$> option walletId
        (  long "id"
        <> metavar "UUID"
        <> help "the ID of the wallet to update" )
    <*> option walletName
        (  long "name"
        <> metavar "STRING"
        <> help "a new name for the wallet" )

walletList :: Parser WalletCommand
walletList = pure WalletList

walletId :: ReadM WalletId
walletId = eitherReader mkWalletId
  where
    mkWalletId :: String -> Either String WalletId
    mkWalletId = maybe
        (Left "invalid wallet ID: must be a valid UUID")
        (pure . WalletId)
        . UUID.fromString

-- TODO:
-- Much of the validation logic herein is identical to that defined within
-- the Cardano.Api.Wallet.Types module. Find a way to reduce the duplication.
walletName :: ReadM WalletName
walletName = eitherReader mkWalletName
  where
    mkWalletName :: String -> Either String WalletName
    mkWalletName n
        | length n < walletNameMinLength =
            Left $ "name is too short: expected at least "
                <> show walletNameMinLength <> " chars"
        | length n > walletNameMaxLength =
            Left $ "name is too long: expected at most "
                <> show walletNameMaxLength <> " chars"
        | otherwise =
            pure $ WalletName $ T.pack n

    walletNameMinLength = 1
    walletNameMaxLength = 255
