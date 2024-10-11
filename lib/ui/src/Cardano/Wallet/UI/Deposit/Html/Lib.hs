{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Html.Lib
    ( imageOverlay
    , overlayFakeDataH
    , selectCustomerH
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( AssocRow
    , simpleField
    )
import Cardano.Wallet.UI.Deposit.API
    ( fakeDataBackgroundLink
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( ListOf
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( HtmlT
    , ToHtml (toHtml)
    , class_
    , div_
    , id_
    , img_
    , input_
    , max_
    , min_
    , name_
    , size_
    , src_
    , step_
    , style_
    , type_
    , value_
    )
import Servant
    ( Link
    )

imageOverlay :: Monad m => HtmlT m ()
imageOverlay =
    style_ []
        $ toHtml @String
            ".overlay-image { position: absolute; top: 0; left: 0; z-index: 10;\
            \ width: 100%; opacity: 5%; pointer-events: none }"

overlayFakeDataH :: Monad m => HtmlT m () -> HtmlT m ()
overlayFakeDataH x = div_ [style_ "position: relative;"] $ do
    x
    img_ [class_ "overlay-image", src_ $ linkText fakeDataBackgroundLink]

selectCustomerH
    :: Monad m
    => Text
    -- ^ CSS selector for the target element
    -> Maybe Text
    -- ^ HTMX include
    -> Link
    -- ^ post link
    -> Int
    -- ^ Number of tracked users
    -> ListOf (AssocRow m)
selectCustomerH identifier include link trackedUsers =
    simpleField "Customer Number"
        $ div_ [class_ "d-flex justify-content-end align-items-center"]
        $ input_
        $ [ id_ "select-customer"
          , type_ "number"
          , hxTarget_ identifier
          , class_ "form-control m-1 p-1"
          , hxTrigger_ "load, change"
          , hxPost_ $ linkText link
          , min_ "0"
          , max_ $ toText $ trackedUsers - 1
          , step_ "1"
          , name_ "customer"
          , value_ "0"
          , size_ "5"
          , style_ "width: 7em"
          ]
            <> maybe [] (\x -> [hxInclude_ x]) include
