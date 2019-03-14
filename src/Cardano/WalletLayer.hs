{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- | Module provides wallet layer api that is used by API layer
-- | and uses both DB and Networking layer to realize its role
-- | as being intermediary between the three.


module Cardano.WalletLayer where

import Prelude

import Cardano.Wallet
    ( Wallet )
import Cardano.Wallet.AddressDerivation
    ( Depth (..), Key, Passphrase, XPub )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap )
import Cardano.Wallet.Mnemonic
    ( Mnemonic )
import Control.Monad.Except
    ( ExceptT )
import Data.Text
    ( Text )
import Data.Time.Units
    ( Microsecond )
import GHC.Generics
    ( Generic )



-- | Errors
data CreateWalletError

data GetWalletError

-- | Types
data WalletLayer m s = WalletLayer
    { createWallet :: CreateWallet -> ExceptT CreateWalletError m (Wallet s)
    , getWallet :: WalletId -> ExceptT GetWalletError m (Wallet s)
    }

data CreateWallet =
    CreateWallet NewWallet
    | ImportWallet (Key 'AccountK XPub)

data NewWallet = NewWallet
    {
      mnemonicSentence
        :: !(Mnemonic 15)
    , mnemonicSentencePassphrase
        :: !(Maybe (Mnemonic 9))
    , name
        :: !WalletName
    , passphrase
        :: !(Passphrase "encryption")
    , addressPoolGap
        :: !AddressPoolGap
    } deriving (Show, Generic)

newtype WalletName = WalletName Text
    deriving (Eq, Show)

newtype WalletId = WalletId Text
    deriving (Eq, Show)

newtype WalletTimestamp = WalletTimestamp Microsecond
    deriving (Eq, Ord, Show)

data WalletMode = Ready | Restoring deriving (Eq, Show)

data Delegation = Delegated | NotDelegated deriving (Eq, Show)

newtype PassphraseInfo = PassphraseInfo { lastUpdated :: WalletTimestamp }
    deriving (Eq, Show)

data WalletMetadata = WalletMetadata {
      id
        :: !WalletId
    , name
        :: !WalletName
    , addressPoolGap
        :: !AddressPoolGap
    , passphraseInfo
        :: !PassphraseInfo
    , state
        :: !WalletMode
    , delegation
        :: !Delegation
    } deriving (Eq, Show, Generic)
