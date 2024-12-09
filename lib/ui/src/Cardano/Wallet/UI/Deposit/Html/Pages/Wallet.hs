{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxDelete_
    , hxSwap_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , dataBsDismiss_
    , linkText
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( ModalData (..)
    , mkModal
    , mkModalButton
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
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
    ( walletDeleteLink
    , walletDeleteModalLink
    , walletLink
    , walletMnemonicLink
    , walletPostMnemonicLink
    , walletPostXPubLink
    , walletStatusLink
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( chainPointToSlotH
    , networkTagH
    , timeH
    , valueH
    , withOriginH
    )
import Cardano.Wallet.UI.Deposit.Types.Wallet
    ( Status (..)
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    , WalletType (..)
    , runWHtml
    )
import Control.Exception
    ( SomeException
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
    , HtmlT
    , ToHtml (..)
    , button_
    , class_
    , div_
    , id_
    , p_
    )

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL

data WalletPresent
    = WalletPresent WalletPublicIdentity
    | WalletAbsent
    | WalletFailedToInitialize ErrDatabase
    | WalletVanished SomeException
    | WalletInitializing
    | WalletClosing

isPresent :: WalletPresent -> Bool
isPresent = \case
    WalletPresent _ -> True
    _ -> False
instance Show WalletPresent where
    show (WalletPresent x) = "WalletPresent: " <> show x
    show WalletAbsent = "WalletAbsent"
    show (WalletFailedToInitialize _) = "WalletFailedToInitialize"
    show (WalletVanished _) = "WalletVanished"
    show WalletInitializing = "WalletInitializing"
    show WalletClosing = "WalletClosing"

walletH :: WHtml ()
walletH = sseH walletLink "wallet" ["wallet"]

pubKeyH :: Monad m => XPub -> HtmlT m ()
pubKeyH xpub =
    truncatableText WithCopy "public_key"
        $ toHtml
        $ B16.encode
        $ xpubToBytes xpub

headAndTail :: Int -> ByteString -> ByteString
headAndTail n t = B8.take n t <> " .. " <> B8.takeEnd n t

deleteWalletButtonH :: Html ()
deleteWalletButtonH =
    mkModalButton
        walletDeleteModalLink
        [class_ "btn btn-danger"]
        "Delete Wallet"

deleteWalletModalH :: Html ()
deleteWalletModalH =
    mkModal
        $ ModalData
            { modalTitle = "Delete Wallet"
            , modalBody = p_ "Are you sure you want to delete this wallet?"
            , modalFooter = do
                button_
                    [ class_ "btn btn-danger"
                    , hxDelete_ $ linkText walletDeleteLink
                    , dataBsDismiss_ "modal"
                    , hxSwap_ "none"
                    ]
                    "Delete Wallet"
                button_
                    [ class_ "btn btn-secondary"
                    , dataBsDismiss_ "modal"
                    ]
                    "Cancel"
            }

walletStatusH :: Status -> Html ()
walletStatusH status = do
    box "Status" mempty
        $ record (Just 13) Full Striped
        $ do
            simpleField "Tip Slot" $ do
                chainPointToSlotH $ tip status
            simpleField "Tip Time" $ do
                maybe mempty (withOriginH timeH) (tipTime status)
            simpleField "Balance" $ valueH $ balance status
            simpleField "Network" $ networkTagH $ network status

walletElementH
    :: (BL.ByteString -> Html ())
    -> WalletPresent
    -> Html ()
walletElementH alert presence = case presence of
    WalletPresent (WalletPublicIdentity xpub customers) -> do
        div_ [class_ "row mt-2 gx-0"]
            $ sseH walletStatusLink "wallet-status" ["wallet-tip"]
        div_ [class_ "row mt-2 gx-0"] $ do
            box "Public Identity" mempty
                $ record (Just 13) Full Striped
                $ do
                    simpleField "Extended Public Key" $ pubKeyH xpub
                    simpleField "Tracked Addresses"
                        $ div_ [class_ "d-flex justify-content-end align-items-center"]
                        $ toHtml
                        $ toText customers
        div_ [class_ "row mt-2 gx-0"] $ do
            box "Management" mempty
                $ div_
                    [class_ "d-flex justify-content-end align-items-center"]
                    deleteWalletButtonH
            div_ [id_ "delete-result"] mempty
    WalletAbsent -> runWHtml Deposit $ do
        div_ [class_ "row mt-2 gx-0"]
            $ newWalletFromMnemonicH walletMnemonicLink
            $ PostWalletConfig
                { walletDataLink = walletPostMnemonicLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        div_ [class_ "row mt-2 gx-0"]
            $ newWalletFromXPubH
            $ PostWalletConfig
                { walletDataLink = walletPostXPubLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        div_ [class_ "row mt-2 gx-0"]
            $ div_ [id_ "post-response"] mempty
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
    WalletClosing -> alert "Wallet is closing"

onWalletPresentH
    :: (WalletPublicIdentity -> Html ())
    -> (BL.ByteString -> Html ())
    -> WalletPresent
    -> Html ()
onWalletPresentH f alert = \case
    WalletPresent wpi -> f wpi
    WalletAbsent -> alert "Wallet is absent"
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
    WalletClosing -> alert "Wallet is closing"

data BadgeStyle
    = Primary
    | Secondary
    | Success
    | Danger
    | Warning
    | Info
    | Light
    | Dark

renderBadgeStyle :: BadgeStyle -> Text
renderBadgeStyle = \case
    Primary -> "primary"
    Secondary -> "secondary"
    Success -> "success"
    Danger -> "danger"
    Warning -> "warning"
    Info -> "info"
    Light -> "light"
    Dark -> "dark"
