module Cardano.Wallet.UI.Html.Pages.Template.Navigation
    ( navigationH
    )
where

import Prelude

import Cardano.Wallet.UI.API
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

activePageH :: Bool -> [Attribute] -> [Attribute]
activePageH c =
    if c
        then (<> [class_ "nav-link active", term "aria-current" "page"])
        else (<> [class_ "nav-link"])

tabOf :: Text -> Bool -> Link -> Html () -> Html ()
tabOf prefix c p t =
    li_ [class_ "nav-item"]
        $ a_ (activePageH c [href_ $ prefix <> linkText p]) t

type PageLinks = [(Bool, Link, Html ())]

navigationH :: Text -> PageLinks -> Html ()
navigationH prefix pages = do
    header_ [class_ "d-flex justify-content-center py-3"] $ do
        ul_ [class_ "nav nav-pills"] $ do
            forM_ pages $ \(c, p, t) -> tabOf prefix c p t
