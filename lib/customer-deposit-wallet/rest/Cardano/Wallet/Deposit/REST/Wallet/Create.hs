{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic (..)
    , PostWalletViaXPub (..)
    )
where

import Prelude

import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )

-- | Data for a request to create a wallet via a mnemonic.
data PostWalletViaMnemonic = PostWalletViaMnemonic
    { mnemonics :: Text
    , password :: Text
    , trackedCustomers :: Int
    }
    deriving (Generic)

-- | Data for a request to create a wallet via an extended public key.
data PostWalletViaXPub = PostWalletViaXPub
    { xpub :: Text
    , trackedCustomers :: Int
    }
    deriving (Generic)
