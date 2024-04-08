{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.UI.Html.Lib
    ( showPercentage
    , showLocalTime
    , ShowTime
    , justifyRight

    )
where

import Prelude

import Data.Generics.Product
    ()
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , getCurrentTimeZone
    , utcToLocalTime
    )
import Lucid
    ( Html
    , ToHtml (..)
    , class_
    , div_
    )

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
