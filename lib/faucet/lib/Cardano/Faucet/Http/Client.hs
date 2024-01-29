{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Http.Client
    ( fetchMnemonicRange
    , fetchMnemonicByIndex
    , fetchMnemonicAddresses
    ) where

--------------------------------------------------------------------------------

import qualified Servant.Client as Servant

import Cardano.Address
    ( NetworkTag
    )
import Cardano.Faucet.Http.Api.Servant
    ( AddressIndex
    , FaucetApi
    , IndexedAddress
    , IndexedMnemonic
    , Mnemonic
    , MnemonicIndex
    )
import Cardano.Faucet.Mnemonics
    ( MnemonicLength
    )
import Cardano.Faucet.Types
    ( AddressStyle
    )
import Servant
    ( Proxy (..)
    , (:<|>) (..)
    )

--------------------------------------------------------------------------------

fetchMnemonicRange
    :: MnemonicLength
    -> MnemonicIndex
    -> MnemonicIndex
    -> Servant.ClientM [IndexedMnemonic]
fetchMnemonicRange mnLength = fetchMnemonics
  where
    fetchMnemonics :<|> _ = Servant.client (Proxy @FaucetApi) mnLength

fetchMnemonicByIndex
    :: MnemonicLength
    -> MnemonicIndex
    -> Servant.ClientM Mnemonic
fetchMnemonicByIndex mnLength mnIndex = fetchMnemonicByIndex'
  where
    _ :<|> fetchMnemonic = Servant.client (Proxy @FaucetApi) mnLength
    fetchMnemonicByIndex' :<|> _ = fetchMnemonic mnIndex

fetchMnemonicAddresses
    :: MnemonicLength
    -> MnemonicIndex
    -> AddressStyle
    -> NetworkTag
    -> AddressIndex
    -> AddressIndex
    -> Servant.ClientM [IndexedAddress]
fetchMnemonicAddresses mnLength mnIndex = fetchAddresses'
  where
    _ :<|> fetchAddresses = Servant.client (Proxy @FaucetApi) mnLength
    _ :<|> fetchAddresses' = fetchAddresses mnIndex
