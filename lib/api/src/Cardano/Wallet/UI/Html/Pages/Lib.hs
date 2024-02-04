{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Html.Pages.Lib where

import Prelude

import Cardano.Wallet.UI.Html.Htmx
    ( hxGet_
    , hxSseSwap_
    , hxSse_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Html.Pages.ListOf
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
import Data.Text
    ( Text
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , b_
    , class_
    , div_
    , id_
    , role_
    , scope_
    , style_
    , table_
    , td_
    , tr_
    )
import Numeric.Natural
    ( Natural
    )
import Servant
    ( Link
    , linkURI
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

recordTable :: ListOf AssocRow -> Html ()
recordTable xs =
    table_ [class_ "table table-hover table-striped"]
        $ mapM_ assocRowH
        $ listOf xs

row :: ToHtml b => [Attribute] -> Text -> b -> ListOf AssocRow
row attrs key val = singleton $ Elem $ AssocRow attrs key val

rowHtml :: [Attribute] -> Text -> Html () -> ListOf AssocRow
rowHtml = row @(Html ())

rowShow :: Show a => [Attribute] -> Text -> a -> ListOf AssocRow
rowShow attrs key val = row attrs key (show val)

sseH :: Link -> Text -> [Text] -> Html ()
sseH link target events = do
    div_ [hxSse_ "connect:/sse"] $ do
        div_
            [ hxTrigger_ triggered
            , hxGet_ linkText
            , hxTarget_ $ "#" <> target
            , hxSwap_ "innerHTML"
            ]
            $ div_
                [ id_ target
                , hxGet_ linkText
                , hxTrigger_ "load"
                , style_ "{ animation: fadeIn 1s; }"
                ]
                ""
  where
    triggered = T.intercalate "," $ ("sse:" <>) <$> events
    linkText = T.pack $ show . linkURI $ link

sseInH :: [Text] -> Html ()
sseInH events = div_ [hxSse_ "connect:/sse", hxSseSwap_ triggered] ""
  where
    triggered = T.intercalate "," events

adaOfLovelace :: Natural -> (Natural, Natural)
adaOfLovelace x = let
    (ada, lovelace) = properFraction @Double $ fromIntegral x / 1_000_000
    in (ada, floor $ lovelace * 1_000_000)

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
    showThousandDots' xs = let
        (a, b) = splitAt 3 xs
        in a <> if null b then [] else "." <> showThousandDots' b
