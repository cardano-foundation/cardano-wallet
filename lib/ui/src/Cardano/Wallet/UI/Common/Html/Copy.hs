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
    , ToHtml (..)
    , button_
    , class_
    , i_
    , id_
    , script_
    , style_
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
        [ class_ "btn copy-button p-0 ps-1"
        , id_ button
        , makeAttribute "data-clipboard-target" fieldId
        ]
        $ i_ [class_ "bi bi-copy"] mempty
  where
    fieldId = "#" <> field'
    button = field' <> "-copy-button"

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
