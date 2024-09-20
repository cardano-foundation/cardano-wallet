{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
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
    , button_
    , class_
    , height_
    , id_
    , script_
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
    button_ [class_ "btn", id_ button] buttonImage
    script_ $ copyButtonScript button field'
  where
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

copyButtonScript :: Text -> Text -> Text
copyButtonScript button field' =
    [i|
    document.getElementById('#{button}').addEventListener('click', function() {
        var mnemonic = document.getElementById('#{field'}').innerText;
        navigator.clipboard.writeText(mnemonic);
    });
    |]
