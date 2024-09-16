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
    ( Html
    , p_
    )

import qualified Data.Text.Encoding as T

walletH :: Html ()
walletH = do
    -- sseH sseLink walletLink "wallet" ["wallet"]
    p_
        "You have no wallet. Pls initialize it"
    newWalletH walletMnemonicLink $ PostWalletConfig
        { walletDataLink = walletLink
        , passwordVisibility = Just Hidden
        , namePresence = False
        }

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . encode EBase16

walletElementH :: WalletPublicIdentity -> Html ()
walletElementH (WalletPublicIdentity xpub customers) = do
    record $ do
        simpleField "XPub" $ record $ do
            simpleField "public key" $ base16 $ xpubPublicKey xpub
            simpleField "other" $ base16 $ xpubChainCode xpub
        simpleField "Known customers" $ toText customers
