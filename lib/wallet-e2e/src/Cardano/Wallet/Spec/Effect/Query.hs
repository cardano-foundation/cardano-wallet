{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Spec.Effect.Query where

import qualified Data.Set as Set
import qualified Data.Text as T

import Cardano.Wallet.Spec.Effect.Trace
    ( FxTrace, trace )
import Cardano.Wallet.Spec.Types
    ( Mnemonic (..), Wallet (..) )
import Effectful
    ( (:>), Eff, Effect )
import Effectful.Dispatch.Dynamic
    ( reinterpret )
import Effectful.State.Static.Local
    ( evalState, get, modify )
import Effectful.TH
    ( makeEffect )
import Prelude hiding
    ( evalState, get, gets, modify, trace )

data FxQuery :: Effect where
    ListKnownWallets :: FxQuery m (Set Wallet)
    CreateWalletFromMnemonic :: Mnemonic -> FxQuery m Wallet
    DeleteWallet :: Wallet -> FxQuery m ()

$(makeEffect ''FxQuery)

runQueryMock :: (FxTrace :> es) => Set Wallet -> Eff (FxQuery : es) a -> Eff es a
runQueryMock db0 = reinterpret (evalState db0) \_ -> \case
    ListKnownWallets -> do
        wallets <- get
        trace $ "Listing known wallets (" <> show (length wallets) <> ")"
        pure wallets
    CreateWalletFromMnemonic (Mnemonic phrase) -> do
        let wallet = Wallet (T.intercalate "." (toList phrase))
        trace $ "Creating a wallet " <> show wallet
        wallet <$ modify (Set.insert wallet)
    DeleteWallet wallet -> do
        trace $ "Deleting a wallet " <> show wallet
        modify (Set.delete wallet)
