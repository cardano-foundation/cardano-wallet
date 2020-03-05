{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Byron.Faucet
    ( Faucet(..)

    , initFaucet
    ) where

import Prelude

import Control.Concurrent.MVar
    ( newMVar )
import Test.Integration.Faucet
    ( Faucet (..), icaMnemonics, rndMnemonics, seqMnemonics )

initFaucet :: IO Faucet
initFaucet = Faucet
    <$> newMVar seqMnemonics
    <*> newMVar icaMnemonics
    <*> newMVar rndMnemonics
    <*> newMVar [] -- FIXME: txBuilder for external transaction.
