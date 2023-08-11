module Cardano.Wallet.Spec.Stories.Wallet
    ( createdWallet
    )
where

import Cardano.Wallet.Spec.Effect.Assert
    ( FxAssert, assert )
import Cardano.Wallet.Spec.Effect.Query
    ( FxQuery, createWalletFromMnemonic, deleteWallet, listKnownWallets )
import Cardano.Wallet.Spec.Effect.Random
    ( FxRandom, randomMnemonic )
import Cardano.Wallet.Spec.Stories.Language
    ( FxStory )
import Data.Set
    ( member, notMember )

createdWallet :: FxStory otherEffects '[FxQuery, FxRandom, FxAssert] ()
createdWallet = do
    mnemonic <- randomMnemonic
    wallet <- createWalletFromMnemonic "My Wallet" mnemonic
    wallets <- listKnownWallets
    assert "the new wallet is known" (wallet `member` wallets)
    deleteWallet wallet
    wallets' <- listKnownWallets
    assert "the wallet is forgotten" (wallet `notMember` wallets')
