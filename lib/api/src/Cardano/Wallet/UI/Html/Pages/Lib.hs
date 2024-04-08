{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Html.Pages.Lib
    ( PrintHtml
    , alertH
    , rogerH
    , AssocRow (..)
    , assocRowH
    , record
    , field
    , simpleField
    , fieldHtml
    , fieldShow
    , sseH
    , sseInH
    , adaOfLovelace
    , showAda
    , showAdaOfLoveLace
    , showThousandDots
    , copyButton
    )
where

import Prelude

import Cardano.Wallet.UI.API
    ( linkText
    )
import Cardano.Wallet.UI.Html.Htmx
    ( hxExt_
    , hxGet_
    , hxSse_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( Cons (..)
    , ListOf
    , listOf
    )
import Control.Monad.Operational
    ( singleton
    )
import Data.ByteString.Lazy.Char8
    ( ByteString
    )
import Data.String.Interpolate
    ( i
    )
import Data.Text
    ( Text
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , b_
    , button_
    , class_
    , div_
    , id_
    , role_
    , scope_
    , script_
    , table_
    , td_
    , tr_
    )
import Lucid.Base
    ( makeAttribute
    )
import Numeric.Natural
    ( Natural
    )
import Servant
    ( Link
    )

import qualified Data.Text as T

type PrintHtml = forall a. ToHtml a => a -> ByteString

alertH :: ToHtml a => a -> Html ()
alertH =
    div_
        [ id_ "result"
        , class_ "alert alert-primary"
        , role_ "alert"
        ]
        . toHtml

rogerH :: ToHtml a => a -> Html ()
rogerH =
    div_
        [ id_ "result"
        , class_ "alert alert-success"
        , role_ "alert"
        ]
        . toHtml

data AssocRow = forall b.
      ToHtml b =>
    AssocRow
    { rowAttributes :: [Attribute]
    , key :: Text
    , val :: b
    }

assocRowH :: AssocRow -> Html ()
assocRowH AssocRow{..} = tr_ ([scope_ "row"] <> rowAttributes) $ do
    td_ [scope_ "col"] $ b_ $ toHtml key
    td_ [scope_ "col"] $ toHtml val

record :: ListOf AssocRow -> Html ()
record xs =
    table_ [class_ "table table-hover table-striped"]
        $ mapM_ assocRowH
        $ listOf xs

field :: ToHtml b => [Attribute] -> Text -> b -> ListOf AssocRow
field attrs key val = singleton $ Elem $ AssocRow attrs key val

simpleField :: ToHtml b => Text -> b -> ListOf AssocRow
simpleField = field []

fieldHtml :: [Attribute] -> Text -> Html () -> ListOf AssocRow
fieldHtml = field @(Html ())

fieldShow :: Show a => [Attribute] -> Text -> a -> ListOf AssocRow
fieldShow attrs key val = field attrs key (show val)

sseConnectFromLink :: Link -> Text
sseConnectFromLink sse = "connect:" <> linkText sse

sseH
    :: Link
    -- ^ SSE link
    -> Link
    -- ^ Link to fetch data from
    -> Text
    -- ^ Target element
    -> [Text]
    -- ^ Events to trigger onto
    -> Html ()
sseH sseLink link target events = do
    div_ [hxSse_ $ sseConnectFromLink sseLink] $ do
        div_
            [ hxTrigger_ triggered
            , hxGet_ $ linkText link
            , hxTarget_ $ "#" <> target
            , hxSwap_ "innerHTML"
            ]
            $ div_
                [ id_ target
                , hxGet_ $ linkText link
                , hxTrigger_ "load"
                ]
                ""
  where
    triggered = T.intercalate "," $ ("sse:" <>) <$> events

sseInH :: Link -> Text -> [Text] -> Html ()
sseInH sseLink target events =
    div_
        [ hxSse_ $ sseConnectFromLink sseLink
        , hxExt_ "sse"
        ]

        $ div_
            [ hxTarget_ $ "#" <> target
            , hxSwap_ "innerHTML"
            , makeAttribute "sse-swap" triggered
            ]
        $ div_ [id_ target] "hello"
  where
    triggered = T.intercalate "," events

adaOfLovelace :: Natural -> (Natural, Natural)
adaOfLovelace x =
    let
        (ada, lovelace) = properFraction @Double $ fromIntegral x / 1_000_000
    in
        (ada, floor $ lovelace * 1_000_000)

showAda :: (Natural, Natural) -> Text
showAda (ada, lovelace) = T.pack $ showThousandDots ada <> ", " <> pad 6 (show lovelace) <> " ADA"
  where
    pad n s = replicate (n - length s) '0' <> s

showAdaOfLoveLace :: Natural -> Text
showAdaOfLoveLace = showAda . adaOfLovelace

showThousandDots :: Show a => a -> String
showThousandDots = reverse . showThousandDots' . reverse . show
  where
    showThousandDots' :: String -> String
    showThousandDots' [] = []
    showThousandDots' xs =
        let
            (a, b) = splitAt 3 xs
        in
            a <> if null b then [] else "." <> showThousandDots' b

copyButton :: Text -> Html ()
copyButton field' = do
    script_
        [i|
            document.getElementById('#{button}').addEventListener('click', function() {
                var mnemonic = document.getElementById('#{field'}').innerText;
                navigator.clipboard.writeText(mnemonic);
            });
        |]
    button_ [class_ "btn btn-outline-secondary", id_ button] "Copy"
  where
    button = field' <> "-copy-button"
