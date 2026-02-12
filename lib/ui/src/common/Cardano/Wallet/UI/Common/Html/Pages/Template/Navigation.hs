{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.UI.Common.Html.Pages.Template.Navigation
    ( navigationH
    , Navigation (..)
    )
where

import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Control.Monad
    ( forM_
    )
import Data.List
    ( find
    )
import Data.Text
    ( Text
    )
import Lucid
    ( HtmlT
    , ToHtml (toHtml)
    , a_
    , alt_
    , button_
    , class_
    , div_
    , h3_
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
import Prelude

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

data Navigation = Navigation
    { navigationHomePage :: Link
    , navigationFavicon :: Link
    , navigationTitle :: Text
    }

-- | Navigation bar rendered.
navigationH
    :: Monad m
    => Text
    -- ^ Link prefix
    -> Navigation
    -- ^ Navigation context
    -> [(Bool, Link, HtmlT m ())]
    -- ^ Pages
    -> HtmlT m ()
navigationH
    prefix
    Navigation
        { navigationHomePage
        , navigationFavicon
        , navigationTitle
        }
    pages = do
        nav_ [class_ "navbar navbar-expand-lg mb-2"]
            $ div_ [class_ "container-fluid"]
            $ do
                a_ [class_ "navbar-brand", href_ $ linkText navigationHomePage]
                    $ img_
                        [ src_ $ linkText navigationFavicon
                        , alt_ navigationTitle
                        , class_ "img-fluid"
                        , style_ "height: 2em;"
                        ]
                h3_ [class_ "d-block d-lg-none"]
                    $ case find (\(c, _, _) -> c) pages of
                        Just (_, _, t) -> t
                        Nothing -> toHtml navigationTitle
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
                    [ class_ "collapse navbar-collapse justify-content-end"
                    , id_ "navbar"
                    ]
                    $ div_ [class_ "navbar-nav"]
                    $ forM_ pages
                    $ \(c, p, t) -> navElem prefix c p t
