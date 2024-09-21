{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
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
    , fadeInId
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxExt_
    , hxGet_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( Cons (..)
    , ListOf
    , listOf
    )
import Control.Monad.Operational
    ( singleton
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
    , HtmlT
    , ToHtml (..)
    , b_
    , button_
    , class_
    , div_
    , id_
    , role_
    , scope_
    , script_
    , style_
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

-- | A simple alert message around any html content.
alertH :: (ToHtml a, Monad m) => a -> HtmlT m ()
alertH =
    div_
        [ id_ "result"
        , class_ "alert alert-primary"
        , role_ "alert"
        ]
        . toHtml

-- | A simple OK message around any html content.
rogerH :: ToHtml a => a -> Html ()
rogerH =
    div_
        [ id_ "result"
        , class_ "alert alert-success"
        , role_ "alert"
        ]
        . toHtml

-- | A simple table row with two columns.
data AssocRow m
    = AssocRow
    { rowAttributes :: [Attribute]
    , key :: HtmlT m ()
    , val :: HtmlT m ()
    }

-- | Render an 'AssocRow' as a table row.
assocRowH :: AssocRow m -> Monad m => HtmlT m ()
assocRowH AssocRow{..} = tr_ ([scope_ "row"] <> rowAttributes) $ do
    td_ [scope_ "col"] $ b_ key
    td_ [scope_ "col"] val

-- | Render a list of 'AssocRow' as a table. We use 'listOf' to allow 'do' notation
-- in the definition of the rows
record :: ListOf (AssocRow m) -> Monad m => HtmlT m ()
record xs =
    table_ [class_ "table table-hover table-striped"]
        $ mapM_ assocRowH
        $ listOf xs

-- | Create an 'AssocRow' from a key and a value.
field :: [Attribute] -> HtmlT m () -> HtmlT m () -> ListOf (AssocRow m)
field attrs key val = singleton $ Elem $ AssocRow attrs key val

-- | Create a simple 'AssocRow' from a key and a value. where the key is a 'Text'.
simpleField :: Monad m => Text -> HtmlT m () -> ListOf (AssocRow m)
simpleField = field [] . toHtml

-- | Create an 'AssocRow' from a key and a value where the value is an 'Html'.
fieldHtml :: Monad m => [Attribute] -> Text -> HtmlT m () -> ListOf (AssocRow m)
fieldHtml as = field as . toHtml

-- | Create an 'AssocRow' from a key and a value where the value is a 'Show' instance.
fieldShow :: (Show a, Monad m) => [Attribute] -> Text -> a -> ListOf (AssocRow m)
fieldShow attrs key val = field attrs (toHtml key) (toHtml $ show val)

fadeInId :: Monad m => HtmlT m ()
fadeInId = style_ [] $ toHtml @Text
    ".smooth.htmx-added { transition: opacity: 0.1s ease-in; opacity: 0} \
    \.smooth { opacity: 1; transition: opacity 0.1s ease-out; }"

-- | A tag that can self populate with data that is fetched as GET from a link
-- whenever some specific events are received from an SSE endpoint.
-- It also self populate on load.
sseH
    :: Link
    -- ^ Link to fetch data from
    -> Text
    -- ^ Target element
    -> [Text]
    -- ^ Events to trigger onto
    -> Monad m
    => HtmlT m ()
sseH link target events = do
     do
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
                , class_ "smooth"
                ]
                ""
  where
    triggered = T.intercalate "," $ ("sse:" <>) <$> events

-- | A tag that can self populate with data directly received in the SSE event.
sseInH :: Text -> [Text] -> Html ()
sseInH target events =
    div_
        [hxExt_ "sse"
        ]
        $ div_
            [ hxTarget_ $ "#" <> target
            , hxSwap_ "innerHTML"
            , makeAttribute "sse-swap" triggered
            ]
        $ div_ [id_ target] "hello"
  where
    triggered = T.intercalate "," events

-- | Convert a number of lovelace to ADA and lovelace.
adaOfLovelace :: Natural -> (Natural, Natural)
adaOfLovelace x =
    let
        (ada, lovelace) = properFraction @Double $ fromIntegral x / 1_000_000
    in
        (ada, floor $ lovelace * 1_000_000)

-- | Show ADA and lovelace.
showAda :: (Natural, Natural) -> Text
showAda (ada, lovelace) =
    T.pack
        $ showThousandDots ada
            <> ", "
            <> pad 6 (show lovelace)
            <> " ADA"
  where
    pad n s = replicate (n - length s) '0' <> s

-- | Show ADA and lovelace from lovelace.
showAdaOfLoveLace :: Natural -> Text
showAdaOfLoveLace = showAda . adaOfLovelace

-- | Show a number with thousand dots.
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

-- | A button that copies the content of a field to the clipboard.
copyButton
    :: Monad m
    => Text
    -- ^ Field id
    -> HtmlT m ()
copyButton field' = do
    button_ [class_ "btn btn-outline-secondary", id_ button] "Copy"
    script_ $ copyButtonScript button field'
  where
    button = field' <> "-copy-button"

copyButtonScript :: Text -> Text -> Text
copyButtonScript button field' =
    [i|
    document.getElementById('#{button}').addEventListener('click', function() {
        var mnemonic = document.getElementById('#{field'}').innerText;
        navigator.clipboard.writeText(mnemonic);
    });
    |]
