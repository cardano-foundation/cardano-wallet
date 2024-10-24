{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    , initClipboardScript
    , copyableHidden
    , offscreenCss
    )
where

import Prelude

import Data.String.Interpolate
    ( i
    )
import Data.Text
    ( Text
    )
import Lucid
    ( Attribute
    , HtmlT
    , Term (..)
    , ToHtml (..)
    , button_
    , class_
    , height_
    , id_
    , script_
    , style_
    , svg_
    , width_
    )
import Lucid.Base
    ( makeAttribute
    )

-- | A button that copies the content of a field to the clipboard.
copyButton
    :: Monad m
    => Text
    -- ^ Field id
    -> HtmlT m ()
copyButton field' = do
    button_
        [ class_ "btn copy-button"
        , id_ button
        , makeAttribute "data-clipboard-target" fieldId
        ]
        buttonImage
  where
    fieldId = "#" <> field'
    button = field' <> "-copy-button"

buttonImage :: Monad m => HtmlT m ()
buttonImage = svg_
    [ class_ "bi bi-copy"
    , width_ "16"
    , height_ "16"
    , fill_ "currentColor"
    , viewBox_ "0 0 16 16"
    ]
    $ do
        path_
            [ fillRule_ "evenodd"
            , d_ drawCopyButton
            ]
            mempty

drawCopyButton :: Text
drawCopyButton = "M4 2a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2zm2-1a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1V2a1 1 0 0 0-1-1zM2 5a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h8a1 1 0 0 0 1-1v-1h1v1a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h1v1z"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

fillRule_ :: Text -> Attribute
fillRule_ = makeAttribute "fill-rule"

path_ :: Term arg result => arg -> result
path_ = term "path"

initClipboardScript :: Monad m => HtmlT m ()
initClipboardScript = script_ "var clipboard = new ClipboardJS('.copy-button');"

copyableHidden :: Text -> [Attribute]
copyableHidden identity =
    [ class_ "offscreen"
    , makeAttribute "aria-hidden" "true"
    , id_ identity
    ]

offscreenCss :: Monad m => HtmlT m ()
offscreenCss =
    style_ []
        $ toHtml @Text
            [i|
    .offscreen {
        position: absolute;
        left: -9999px;
        top: auto;
        width: 1px;
        height: 1px;
        overflow: hidden;
    }
|]
