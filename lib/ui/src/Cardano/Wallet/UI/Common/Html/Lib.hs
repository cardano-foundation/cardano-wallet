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
    )
where

import Prelude

import Data.Generics.Product
    ()
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
    )
import Lucid.Base
    ( makeAttribute
    )
import Servant.Links
    ( Link
    , linkURI
    )

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
justifyRight = div_ [class_ "d-flex justify-content-end"] . toHtml

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
