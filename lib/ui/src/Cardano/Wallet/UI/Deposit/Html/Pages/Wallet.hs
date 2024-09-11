{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
where

import Prelude

import Cardano.Address.Derivation
    ( xpubChainCode
    , xpubPublicKey
    )
import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( record
    , simpleField
    )
import Cardano.Wallet.UI.Common.Html.Pages.Wallet
    ( PostWalletConfig (..)
    , newWalletH
    )
import Cardano.Wallet.UI.Deposit.API
    ( walletLink
    , walletMnemonicLink
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , encode
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( HtmlT
    )

import qualified Data.Text.Encoding as T

data WalletPresent = WalletPresent WalletPublicIdentity | WalletAbsent

walletH :: WalletPresent -> WHtml ()
walletH walletPresent = do
    -- sseH sseLink walletLink "wallet" ["wallet"]
    case walletPresent of
        WalletPresent wallet -> walletElementH wallet
        WalletAbsent ->
            newWalletH walletMnemonicLink $ PostWalletConfig
                { walletDataLink = walletLink
                , passwordVisibility = Just Hidden
                }

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . encode EBase16

walletElementH :: WalletPublicIdentity -> Monad m => HtmlT m ()
walletElementH (WalletPublicIdentity xpub customers) = do
    record $ do
        simpleField "XPub" $ record $ do
            simpleField "public key" $ base16 $ xpubPublicKey xpub
            simpleField "other" $ base16 $ xpubChainCode xpub
        simpleField "Known customers" $ toText customers
