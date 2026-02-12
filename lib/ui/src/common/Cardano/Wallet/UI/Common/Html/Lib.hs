{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.UI.Common.Html.Lib
    ( showPercentage
    , showLocalTime
    , ShowTime
    , justifyRight
    , linkText
    , showHtml
    , toTextHtml
    , dataBsToggle_
    , dataBsTarget_
    , dataBsDismiss_
    , ariaHidden_
    , ariaLabel_
    , AlertH
    , monospaced
    , truncatableText
    , WithCopy (..)
    , tdEnd
    , thEnd
    , imageOverlay
    )
where

import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    )
import Data.Generics.Product
    (
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , getCurrentTimeZone
    , utcToLocalTime
    )
import Lucid
    ( Attribute
    , Html
    , HtmlT
    , ToHtml (..)
    , class_
    , div_
    , em_
    , id_
    , style_
    , td_
    , th_
    )
import Lucid.Base
    ( makeAttribute
    )
import Servant.Links
    ( Link
    , linkURI
    )
import Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

showPercentage :: Rational -> String
showPercentage p =
    show @Double
        ( fromIntegral
            (round (p * 100_000_000) :: Int)
            / 1_000_000
        )
        <> "%"

type ShowTime = UTCTime -> String

showLocalTime :: IO ShowTime
showLocalTime = do
    zone <- getCurrentTimeZone
    pure
        $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . utcToLocalTime zone

justifyRight :: ToHtml b => b -> Html ()
justifyRight =
    div_ [class_ "d-flex justify-content-end align-items-center"] . toHtml

linkText :: Link -> Text
linkText = T.pack . ('/' :) . show . linkURI

showHtml :: (Show a, Monad m) => a -> HtmlT m ()
showHtml = toHtml . show

toTextHtml :: (Monad m, ToText a) => a -> HtmlT m ()
toTextHtml = toHtml . toText

dataBsToggle_ :: Text -> Attribute
dataBsToggle_ = makeAttribute "data-bs-toggle"

dataBsTarget_ :: Text -> Attribute
dataBsTarget_ = makeAttribute "data-bs-target"

dataBsDismiss_ :: Text -> Attribute
dataBsDismiss_ = makeAttribute "data-bs-dismiss"

ariaHidden_ :: Text -> Attribute
ariaHidden_ = makeAttribute "aria-hidden"

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"

type AlertH = BL.ByteString -> Html ()

monospaced :: Monad m => HtmlT m ()
monospaced =
    style_
        ".monospaced {font-family: \"Courier New\",monospace !important;}"

data WithCopy = WithCopy | WithoutCopy

truncatableText
    :: Monad m => WithCopy -> Text -> HtmlT m () -> HtmlT m ()
truncatableText copy identifier h =
    div_ [class_ "d-flex justify-content-end align-items-center"] $ do
        div_
            [ id_ identifier
            , class_ "text-truncate text-end monospaced"
            ]
            h
        case copy of
            WithCopy -> copyButton identifier
            WithoutCopy -> mempty

tdEnd :: Monad m => HtmlT m () -> HtmlT m ()
tdEnd = td_ [class_ "text-end p-1 align-bottom"]

thEnd :: Monad m => Maybe Int -> HtmlT m () -> HtmlT m ()
thEnd mw x =
    ($ em_ x)
        $ th_
        $ [ class_ "text-end p-1 align-bottom"
          , style_ "background:#26263d;font-weight:normal"
          ]
            <> maybe [] (\w -> [style_ $ "width: " <> T.pack (show w) <> "em;"]) mw

imageOverlay :: Monad m => HtmlT m ()
imageOverlay =
    style_ []
        $ toHtml @String
            ".overlay-image { position: absolute; top: 0; left: 0; z-index: 10;\
            \ width: 100%; opacity: 5%; pointer-events: none }"
