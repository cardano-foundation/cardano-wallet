{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Logging types specific to the shared wallet database.
--
module Cardano.SharedWallet.DB.Log
    ( SharedWalletDbLog (..)
    , ParseFailure (..)
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.DB.Sqlite
    ( DBLog (..) )
import Cardano.SharedWallet.Script
    ( CredentialType )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), toText )

import qualified Data.Text as T

data SharedWalletDbLog
    = MsgGeneric DBLog
    | MsgParseFailure ParseFailure
    | MsgCreatingSharedWallet WalletId
    | MsgAddingCosigner WalletId Cosigner CredentialType
    | MsgRemovingSharedWallet WalletId
    deriving (Eq, Show)

data ParseFailure = ParseFailure
    { parseFailureOperationName
        :: Text
      -- ^ The name of the operation in which the parse failure occurred.
    , parseFailure
        :: Text
      -- ^ A description of the parse failure.
    }
    deriving (Eq, Show)

instance HasPrivacyAnnotation SharedWalletDbLog

instance HasSeverityAnnotation SharedWalletDbLog where
    getSeverityAnnotation = \case
        MsgGeneric e -> getSeverityAnnotation e
        MsgParseFailure {} -> Error
        MsgRemovingSharedWallet {} -> Notice
        MsgCreatingSharedWallet {} -> Notice
        MsgAddingCosigner {} -> Notice

instance ToText SharedWalletDbLog where
    toText = \case
        MsgGeneric e -> toText e
        MsgParseFailure e -> mconcat
            [ "Unexpected parse failure in '"
            , parseFailureOperationName e
            , "'. Description of error: "
            , parseFailure e
            ]
        MsgRemovingSharedWallet wid -> mconcat
            [ "Removing the following shared wallet from the database: "
            , toText wid
            , "."
            ]
        MsgCreatingSharedWallet wid -> mconcat
            [ "Creating the following shared wallet in the database: "
            , toText wid
            , "."
            ]
        MsgAddingCosigner wid (Cosigner c) cred -> mconcat
            [ "Adding the account public key for the cosigner#"
            , T.pack (show c)
            , " for "
            , toText cred
            , " credential for the following shared wallet in the database: "
            , toText wid
            , "."
            ]
