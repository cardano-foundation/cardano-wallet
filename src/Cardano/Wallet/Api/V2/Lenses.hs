{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Api.V2.Lenses where

import qualified Cardano.Wallet.Api.V2.Types.Amount as T
import qualified Cardano.Wallet.Api.V2.Types.Wallet as T
import qualified Cardano.Wallet.Api.V2.Types.WalletBalance as T
import qualified Cardano.Wallet.Api.V2.Types.WalletDelegation as T
import qualified Cardano.Wallet.Api.V2.Types.WalletId as T
import qualified Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo as T
import qualified Cardano.Wallet.Api.V2.Types.WalletState as T

import Control.Lens
    ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''T.Amount
makeFieldsNoPrefix ''T.Wallet
makeFieldsNoPrefix ''T.WalletBalance
makeFieldsNoPrefix ''T.WalletDelegation
makeFieldsNoPrefix ''T.WalletId
makeFieldsNoPrefix ''T.WalletPassphraseInfo
makeFieldsNoPrefix ''T.WalletState
