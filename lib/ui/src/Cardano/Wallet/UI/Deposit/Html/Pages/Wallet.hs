{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubToBytes
    )
import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.REST
    ( ErrDatabase
    )
import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( copyButton
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Wallet
    ( PostWalletConfig (..)
    , newWalletFromMnemonicH
    , newWalletFromXPubH
    )
import Cardano.Wallet.UI.Deposit.API
    ( walletLink
    , walletMnemonicLink
    , walletPostMnemonicLink
    , walletPostXPubLink
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    , WalletType (..)
    , runWHtml
    )
import Control.Exception
    ( SomeException
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Html
    , HtmlT
    , ToHtml (..)
    , class_
    , div_
    , hidden_
    , id_
    )

import Cardano.Wallet.UI.Shelley.API
    ( sseLink
    )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL

data WalletPresent
    = WalletPresent WalletPublicIdentity
    | WalletAbsent
    | WalletFailedToInitialize ErrDatabase
    | WalletVanished SomeException
    | WalletInitializing

instance Show WalletPresent where
    show (WalletPresent x) = "WalletPresent: " <> show x
    show WalletAbsent = "WalletAbsent"
    show (WalletFailedToInitialize _) = "WalletFailedToInitialize"
    show (WalletVanished _) = "WalletVanished"
    show WalletInitializing = "WalletInitializing"

walletH :: WHtml ()
walletH = sseH sseLink walletLink "wallet" ["wallet"]

base64 :: ByteString -> ByteString
base64 = convertToBase Base64

pubKeyH :: Monad m => XPub -> HtmlT m ()
pubKeyH xpub = div_ [class_ "row"] $ do
    div_ [id_ "public_key", hidden_ "false"] $ toHtml xpubByteString
    div_ [class_ "col-6"] $ toHtml $ headAndTail 4 $ B8.dropEnd 2 xpubByteString
    div_ [class_ "col-6"]
        $ copyButton "public_key"
  where
    xpubByteString = base64 $ xpubToBytes xpub

headAndTail :: Int -> ByteString -> ByteString
headAndTail n t = B8.take n t <> ".." <> B8.takeEnd n t

walletElementH :: (BL.ByteString -> Html ()) -> WalletPresent -> Html ()
walletElementH alert = \case
    (WalletPresent (WalletPublicIdentity xpub customers)) -> do
        record $ do
            simpleField "Public Key" $ pubKeyH xpub
            simpleField "Customer Discovery" $ toHtml $ toText customers
        -- div_ [class_ "row"] $ do
            -- button_
            --     [ class_ "btn btn-danger"
            --     , hxDelete_ $ linkText walletDeleteLink
            --     , hxTarget_ "#delete-result"
            --     ]
            --     "Delete Wallet"
            -- div_ [id_ "delete-result"] mempty
    WalletAbsent -> runWHtml Deposit $ do
        newWalletFromMnemonicH walletMnemonicLink
            $ PostWalletConfig
                { walletDataLink = walletPostMnemonicLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        newWalletFromXPubH
            $ PostWalletConfig
                { walletDataLink = walletPostXPubLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        div_ [id_ "post-response"] mempty
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
