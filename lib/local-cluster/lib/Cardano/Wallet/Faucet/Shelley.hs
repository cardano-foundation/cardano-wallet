{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Faucet.Shelley
    ( Faucet (..)
    , initFaucet
    ) where

import Prelude

import Cardano.Wallet.Faucet
    ( Faucet (..) )
import UnliftIO.MVar
    ( newMVar )

import qualified Cardano.Wallet.Faucet.Mnemonics as Mnemonics

initFaucet :: IO Faucet
initFaucet =
    Faucet
        <$> newMVar Mnemonics.sequential
        <*> newMVar Mnemonics.icarus
        <*> newMVar Mnemonics.random
        <*> newMVar Mnemonics.mir
        <*> newMVar Mnemonics.shelleyMA
        <*> newMVar [] -- FIXME: txBuilder for external transaction.
