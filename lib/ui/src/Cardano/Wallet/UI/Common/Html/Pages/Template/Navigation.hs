module Cardano.Wallet.UI.Common.Html.Pages.Template.Navigation
    ( navigationH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Control.Monad
    ( forM_
    )
import Data.Text
    ( Text
    )
import Lucid
    ( Attribute
    , Html
    , a_
    , class_
    , header_
    , href_
    , li_
    , term
    , ul_
    )
import Servant
    ( Link
    )

-- | Add attributes for the active page.
activePageH :: Bool -> [Attribute] -> [Attribute]
activePageH c =
    if c
        then (<> [class_ "nav-link active", term "aria-current" "page"])
        else (<> [class_ "nav-link"])

-- | Wrap a content as link tab.
tabOf
    :: Text
    -- ^ Link prefix
    -> Bool
    -- ^ Is current page
    -> Link
    -- ^ Page link
    -> Html ()
    -- ^ Tab content
    -> Html ()
    -- ^ Tab
tabOf prefix c p t =
    li_ [class_ "nav-item"]
        $ a_ (activePageH c [href_ $ prefix <> linkText p]) t

-- | Navigation bar definition.
type PageLinks = [(Bool, Link, Html ())]

-- | Navigation bar rendered.
navigationH
    :: Text -- ^ Link prefix
    -> PageLinks -- ^ Page links
    -> Html ()
navigationH prefix pages = do
    header_ [class_ "d-flex justify-content-center py-3"] $ do
        ul_ [class_ "nav nav-pills"] $ do
            forM_ pages $ \(c, p, t) -> tabOf prefix c p t