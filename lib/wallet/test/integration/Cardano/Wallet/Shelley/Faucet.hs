{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Shelley.Faucet
    ( Faucet(..)

    , initFaucet
    ) where

import Prelude

import Cardano.Wallet.Faucet
    ( Faucet (..)
    , icaMnemonics
    , maMnemonics
    , mirMnemonics
    , rndMnemonics
    , seqMnemonics
    )
import UnliftIO.MVar
    ( newMVar )

initFaucet :: IO Faucet
initFaucet = Faucet
    <$> newMVar seqMnemonics
    <*> newMVar icaMnemonics
    <*> newMVar rndMnemonics
    <*> newMVar mirMnemonics
    <*> newMVar maMnemonics
    <*> newMVar [] -- FIXME: txBuilder for external transaction.
