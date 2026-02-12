{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Html.Modal
    ( modalsH
    , ModalData (..)
    , mkModal
    , mkModalButton
    )
where

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxGet_
    , hxTarget_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( ariaHidden_
    , dataBsTarget_
    , dataBsToggle_
    , linkText
    )
import Lucid
    ( Attribute
    , HtmlT
    , button_
    , class_
    , div_
    , id_
    , role_
    , tabindex_
    )
import Servant
    ( Link
    )
import Prelude

-- | the unique node of the modal support, pls add it to the end of the body
modalsH :: Applicative m => HtmlT m ()
modalsH = do
    div_
        [ class_ "modal"
        , role_ "dialog"
        , ariaHidden_ "false"
        , tabindex_ "-1"
        , id_ "modals"
        ]
        $ do
            div_
                [ class_ "modal-dialog"
                , role_ "document"
                ]
                $ do
                    div_ [class_ "modal-content", id_ "modal-content"] mempty

data ModalData m = ModalData
    { modalTitle :: HtmlT m ()
    , modalBody :: HtmlT m ()
    , modalFooter :: HtmlT m ()
    }

mkModal :: Monad m => ModalData m -> HtmlT m ()
mkModal ModalData{..} = do
    div_ [class_ "modal-header"] $ do
        modalTitle
    div_ [class_ "modal-body"] modalBody
    div_ [class_ "modal-footer"] modalFooter

mkModalButton
    :: Monad m => Link -> [Attribute] -> HtmlT m () -> HtmlT m ()
mkModalButton getLink buttonAttrs =
    button_
        $ [ hxGet_ $ linkText getLink
          , hxTarget_ "#modal-content"
          , dataBsToggle_ "modal"
          , dataBsTarget_ "#modals"
          ]
            <> buttonAttrs
