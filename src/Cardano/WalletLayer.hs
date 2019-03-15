{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.DBLayer" and "Cardano.NetworkLayer" to realize its role as being
-- intermediary between the three.


module Cardano.WalletLayer where

import Prelude

import Cardano.DBLayer
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet
    ( Wallet, WalletId, WalletName, applyBlock )
import Cardano.Wallet.AddressDerivation
    ( Depth (..), Key, Passphrase, XPub )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, SeqState )
import Cardano.Wallet.Mnemonic
    ( Mnemonic )
import Cardano.Wallet.Primitive
    ( Block (..) )
import Control.DeepSeq
    ( deepseq )
import Control.Monad.Except
    ( ExceptT )
import Data.List
    ( foldl' )
import GHC.Generics
    ( Generic )

import qualified Data.Set as Set


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

-- | A wallet worker which monitor blocks for a given wallet.
walletWorker
    :: DBLayer IO SeqState
    -> WalletId
    -> [Block]
    -> IO ()
walletWorker db wid blocks = do
    cps' <- readCheckpoints db (PrimaryKey wid) >>= \case
        Nothing ->
            fail $ "couldn't find worker wallet: " <> show wid
        Just cps -> do
            let nonEmpty = not . Set.null . transactions
            let cps' = foldl' (flip applyBlock) cps (filter nonEmpty blocks)
            return cps'
    cps' `deepseq` putCheckpoints db (PrimaryKey wid) cps'
