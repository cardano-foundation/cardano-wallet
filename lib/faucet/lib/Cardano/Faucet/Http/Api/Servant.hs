{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Faucet.Http.Api.Servant
    ( FaucetApi
    , Mnemonic
    , MnemonicIndex
    , IndexedMnemonic
    , FaucetAddress
    , AddressIndex
    , IndexedAddress
    ) where

--------------------------------------------------------------------------------

import Cardano.Address
    ( NetworkTag
    )
import Cardano.Faucet.Http.Api.OrphanInstances
    ()
import Cardano.Faucet.Mnemonics
    ( MnemonicLength
    )
import Cardano.Faucet.Types
    ( AddressIndex
    , AddressStyle
    , FaucetAddress
    , IndexedAddress
    , IndexedMnemonic
    , Mnemonic
    , MnemonicIndex
    )
import Servant.API
    ( Capture
    , Get
    , JSON
    , (:<|>)
    , (:>)
    )

--------------------------------------------------------------------------------

type FaucetApi =
    "mnemonics"
    :> Capture "size" MnemonicLength
    :> (MnemonicsByRange :<|> MnemonicByIndex)

type MnemonicsByRange =
    Capture "minIndex" MnemonicIndex
    :> Capture "maxIndex" MnemonicIndex
    :> Get '[JSON] [IndexedMnemonic]

type MnemonicByIndex =
    Capture "index" MnemonicIndex
    :> (Get '[JSON] Mnemonic :<|> AddressesByRange)

type AddressesByRange =
    "addresses"
    :> Capture "style" AddressStyle
    :> Capture "networkTag" NetworkTag
    :> Capture "minIndex" AddressIndex
    :> Capture "maxIndex" AddressIndex
    :> Get '[JSON] [IndexedAddress]
