module Cardano.Wallet.Spec.Stories.Wallet
    ( testEnvironmentIsReady
    , createdWalletListed
    , createdWalletRetrievable
    ) where

import Cardano.Wallet.Spec.Data.Network.Info
    ( NetworkInfo (..) )
import Cardano.Wallet.Spec.Data.Network.NodeStatus
    ( NodeStatus (..) )
import Cardano.Wallet.Spec.Data.Wallet
    ( Wallet (..), walletId )
import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assert )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery
    , createWalletFromMnemonic
    , deleteWallet
    , getWallet
    , listKnownWallets
    , queryNetworkInfo
    )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, randomMnemonic, randomWalletName )
import Cardano.Wallet.Spec.Effect.Timeout
    ( FxTimeout, within_ )
import Cardano.Wallet.Spec.Stories.Language
    ( FxStory )
import Data.Set
    ( member, notMember )
import Data.Time.TimeSpan
    ( minutes )

testEnvironmentIsReady :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
testEnvironmentIsReady = do
    NetworkInfo{nodeStatus} <- queryNetworkInfo
    assert "the Cardano Node is running and synced" (nodeStatus == NodeIsSynced)

createdWalletListed :: FxStory fxs '[FxQuery, FxRandom, FxTimeout, FxAssert] ()
createdWalletListed = do
    walletName <- randomWalletName "Test Wallet"
    mnemonic <- randomMnemonic
    wallet <- createWalletFromMnemonic walletName mnemonic
    wallets <- listKnownWallets
    assert "the new wallet is known" (wallet `member` wallets)
    within_ (minutes 5.0) do deleteWallet wallet
    wallets' <- listKnownWallets
    assert "the wallet is forgotten" (wallet `notMember` wallets')

createdWalletRetrievable :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
createdWalletRetrievable = do
    name <- randomWalletName "Test Wallet"
    mnemonic <- randomMnemonic
    createdWallet <- createWalletFromMnemonic name mnemonic
    retrievedWallet <- getWallet (walletId createdWallet)
    assert "same wallet retrieved by id" (createdWallet == retrievedWallet)
