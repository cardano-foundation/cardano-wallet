module Cardano.Wallet.Spec.Stories.Wallet
    ( testEnvironmentIsReady
    , createdWalletListed
    , createdWalletRetrievable
    , createdWalletHasZeroAda
    ) where

import Cardano.Wallet.Spec.Data.AdaBalance
    ( zeroAdaBalance )
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
    ( FxTimeout, within )
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
    wallet <- createFreshWallet
    wallets <- listKnownWallets
    assert "the new wallet is known" (wallet `member` wallets)
    within (minutes 2.0) do deleteWallet wallet
    wallets' <- listKnownWallets
    assert "the wallet is forgotten" (wallet `notMember` wallets')

createdWalletRetrievable :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
createdWalletRetrievable = do
    createdWallet <- createFreshWallet
    retrievedWallet <- getWallet (walletId createdWallet)
    assert "same wallet retrieved by id" (createdWallet == retrievedWallet)

createdWalletHasZeroAda :: FxStory fxs '[FxQuery, FxRandom, FxAssert] ()
createdWalletHasZeroAda = do
    wallet <- createFreshWallet
    assert "freshly created wallet has 0 ADA balance"
        $ walletBalance wallet == zeroAdaBalance

--------------------------------------------------------------------------------
-- Re-usable sequences of actions ----------------------------------------------

createFreshWallet :: FxStory fxs '[FxQuery, FxRandom] Wallet
createFreshWallet = do
    name <- randomWalletName "Test Wallet"
    mnemonic <- randomMnemonic
    createWalletFromMnemonic name mnemonic
