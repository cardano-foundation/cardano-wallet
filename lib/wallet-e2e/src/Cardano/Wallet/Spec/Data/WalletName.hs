module Cardano.Wallet.Spec.Data.WalletName
    ( WalletName
    , toText
    , mkUnsafe
    ) where

import Prelude hiding
    ( toText
    )

newtype WalletName = WalletName Text
    deriving stock (Show, Eq, Ord)

toText :: WalletName -> Text
toText (WalletName text) = text

mkUnsafe :: Text -> WalletName
mkUnsafe = WalletName
