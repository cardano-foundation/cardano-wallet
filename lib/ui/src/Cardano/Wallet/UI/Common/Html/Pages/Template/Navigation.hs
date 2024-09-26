module Cardano.Wallet.UI.Common.Html.Pages.Template.Navigation
    ( navigationH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Deposit.API
    ( faviconLink
    , homePageLink
    )
import Control.Monad
    ( forM_
    )
import Data.Text
    ( Text
    )
import Lucid
    ( HtmlT
    , a_
    , alt_
    , button_
    , class_
    , div_
    , href_
    , id_
    , img_
    , nav_
    , span_
    , src_
    , style_
    , term
    , type_
    )
import Servant
    ( Link
    )

-- | Wrap a content as link tab.
navElem
    :: Monad m
    => Text
    -- ^ Link prefix
    -> Bool
    -- ^ Is current page
    -> Link
    -- ^ Page link
    -> HtmlT m ()
    -- ^ Tab content
    -> HtmlT m ()
    -- ^ Tab
navElem prefix c p = a_ ([href_ $ prefix <> linkText p, class_ class'])
  where
    class' = baseClass <> " " <> if c then "active" else ""
    baseClass = "nav-link ms-auto fs-4 fs-5-md"

-- | Navigation bar definition.

-- | Navigation bar rendered.
navigationH
    :: Monad m
    => Text
    -- ^ Link prefix
    -> [(Bool, Link, HtmlT m ())]
    -- ^ Pages
    -> HtmlT m ()
navigationH prefix pages = do
    nav_ [class_ "navbar navbar-expand-lg bg-body-tertiary mb-2"]
        $ div_ [class_ "container-fluid"]
        $ do
            a_ [class_ "navbar-brand", href_ $ linkText homePageLink]
                $ img_ [src_ $ linkText faviconLink
                    , alt_ "Cardano Deposit Wallet"
                    , class_ "img-fluid"
                    , style_ "height: 2em;"
                    ]
            button_
                [ class_ "navbar-toggler"
                , type_ "button"
                , term "data-bs-toggle" "collapse"
                , term "data-bs-target" "#navbar"
                , term "aria-controls" "navbar"
                , term "aria-expanded" "false"
                , term "aria-label" "Toggle navigation"
                ]
                $ do
                    span_ [class_ "navbar-toggler-icon"] ""
            div_
                [ class_ "collapse navbar-collapse"
                , id_ "navbar"
                ]
                $ div_ [class_ "navbar-nav"]
                $ forM_ pages
                $ \(c, p, t) -> navElem prefix c p t
