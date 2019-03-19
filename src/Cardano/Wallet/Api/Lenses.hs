{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Api.Lenses where

import qualified Cardano.Wallet.Api.Types.Amount as T
import qualified Cardano.Wallet.Api.Types.Wallet as T
import qualified Cardano.Wallet.Api.Types.WalletBalance as T
import qualified Cardano.Wallet.Api.Types.WalletDelegation as T
import qualified Cardano.Wallet.Api.Types.WalletId as T
import qualified Cardano.Wallet.Api.Types.WalletPassphraseInfo as T
import qualified Cardano.Wallet.Api.Types.WalletState as T

import Control.Lens
    ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''T.Amount
makeFieldsNoPrefix ''T.Wallet
makeFieldsNoPrefix ''T.WalletBalance
makeFieldsNoPrefix ''T.WalletDelegation
makeFieldsNoPrefix ''T.WalletId
makeFieldsNoPrefix ''T.WalletPassphraseInfo
makeFieldsNoPrefix ''T.WalletState
