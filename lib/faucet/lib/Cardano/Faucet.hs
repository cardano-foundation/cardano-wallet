{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Faucet
    ( initialState
    , serveMnemonics
    , serveMenmonic
    , serveAddresses
    ) where

import Prelude

import qualified Cardano.Faucet.Addresses as Addresses
import qualified Cardano.Faucet.Mnemonics as Mnemonic
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Map

import Cardano.Address
    ( NetworkTag
    )
import Cardano.Faucet.FaucetM
    ( FaucetM
    , FaucetState (..)
    )
import Cardano.Faucet.Mnemonics
    ( MnemonicLength (M24)
    )
import Cardano.Faucet.Types
    ( AddressIndex
    , AddressStyle (..)
    , FaucetAddress (FaucetAddress)
    , IndexedAddress (..)
    , IndexedMnemonic (..)
    , Mnemonic (..)
    , MnemonicIndex
    )
import Cardano.Mnemonic.Extended
    ( SomeMnemonic (..)
    )
import Control.Applicative
    ( liftA2
    )
import Control.Monad
    ( forM
    , unless
    )
import Control.Monad.Error.Class
    ( throwError
    )
import Control.Monad.State.Class
    ( get
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( (<&>)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Servant
    ( err404
    )

--------------------------------------------------------------------------------

initialState :: IO FaucetState
initialState = do
    indexedMnemonics <- forM [minBound .. maxBound] \len ->
        (len,) <$> genIndexedMnemonics len
    pure $ FaucetState $ Map.fromAscList indexedMnemonics

genIndexedMnemonics :: MnemonicLength -> IO (NonEmpty IndexedMnemonic)
genIndexedMnemonics len =
    (:|)
        <$> genIndexedMnemonic len minBound
        <*> traverse (genIndexedMnemonic len) [succ minBound .. maxBound]

genIndexedMnemonic :: MnemonicLength -> MnemonicIndex -> IO IndexedMnemonic
genIndexedMnemonic len index =
    IndexedMnemonic index . Mnemonic <$> Mnemonic.generateSome len

serveMnemonics
    :: MnemonicLength
    -> MnemonicIndex
    -> MnemonicIndex
    -> FaucetM [IndexedMnemonic]
serveMnemonics mnLen minIndex maxIndex = do
    unless (minIndex <= maxIndex) (throwError err404)
    get <&> \FaucetState{indexedMnemonics} ->
        case Map.lookup mnLen indexedMnemonics of
            Nothing -> []
            Just mnemonics ->
                mnemonics
                    & NE.filter \(IndexedMnemonic index _mnemonic) ->
                        index >= minIndex && index <= maxIndex

serveMenmonic :: MnemonicLength -> MnemonicIndex -> FaucetM Mnemonic
serveMenmonic mnLen index =
    serveMnemonics mnLen index index >>= \case
        [IndexedMnemonic _index mnemonic] -> pure mnemonic
        _ -> throwError err404

serveAddresses
    :: MnemonicLength
    -> MnemonicIndex
    -> AddressStyle
    -> NetworkTag
    -> AddressIndex
    -> AddressIndex
    -> FaucetM [IndexedAddress]
serveAddresses mnLen mnIdx style netTag minAddrIdx maxAddrIdx = do
    Mnemonic (SomeMnemonic mnemonic) <- serveMenmonic mnLen mnIdx
    let stylishEncoder = case style of
            AddressStyleShelley -> Addresses.shelley
            AddressStyleByron -> Addresses.byron
            AddressStyleIcarus -> Addresses.icarus
    pure
        $ mnemonic
        & stylishEncoder netTag
        & fmap FaucetAddress
        & zipWith IndexedAddress [0 ..]
        & drop (fromEnum minAddrIdx)
        & take (fromEnum maxAddrIdx - fromEnum minAddrIdx + 1)
