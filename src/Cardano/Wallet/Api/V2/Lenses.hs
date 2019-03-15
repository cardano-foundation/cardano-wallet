{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Api.V2.Lenses where

import Cardano.Wallet.Api.V2.Types.Amount
import Cardano.Wallet.Api.V2.Types.Wallet
import Cardano.Wallet.Api.V2.Types.WalletBalance
import Cardano.Wallet.Api.V2.Types.WalletDelegation
import Cardano.Wallet.Api.V2.Types.WalletId
import Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo
import Cardano.Wallet.Api.V2.Types.WalletState

import Control.Lens
    ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''Amount
makeFieldsNoPrefix ''Wallet
makeFieldsNoPrefix ''WalletBalance
makeFieldsNoPrefix ''WalletDelegation
makeFieldsNoPrefix ''WalletId
makeFieldsNoPrefix ''WalletPassphraseInfo
makeFieldsNoPrefix ''WalletState
