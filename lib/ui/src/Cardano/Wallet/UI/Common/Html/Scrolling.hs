{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Common.Html.Scrolling
    ( Configuration (..)
    , Scrolling (..)
    , newScrolling
    )
where

import Prelude

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxSwapOob_
    , hxSwap_
    , hxTrigger_
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )
import Data.Traversable
    ( forM
    )
import Lucid
    ( Attribute
    , Html
    , div_
    , form_
    , hidden_
    , id_
    , input_
    , name_
    , value_
    )

import qualified Data.Set as Set

-- | A 'Direction' is used to indicate whether an element should be added
-- before or after another element.
data Direction = Before | After
    deriving (Show)

-- | A 'Change' represents a change to the scrolling table.
-- The index is an index into some form of paging. It's required that
-- if nothing happened in the monad, the mapping from index to data is
-- constant and that all the data is present in the set of indices.
data Change index
    = -- | Delete the data and the state for the given index.
      Delete index
    | -- | Add the data and the state for the given index in the given direction.
      Add Direction index
    deriving (Show)

-- | Compute what changes need to be made to the scrolling table.
changes
    :: (Eq index, Monad m)
    => Configuration m index
    -- ^ Configuration
    -> Set index
    -- ^ The set of indices that are currently present in the scrolling table.
    -> index
    -- ^ The index that entered the viewport.
    -> m [Change index]
changes configuration presences signal = case Set.toList presences of
    [_] -> onNext signal Add
    -- we cannot expand in the past as sadly htmx will focus on the new element
    -- and we will loop on it as it is revealed straight away
    [p0, p1]
        | p1 == signal -> onNext signal Add
        | p0 == signal -> onPrevious signal Add
    [p0, _p1, p2]
        | p2 == signal -> onNext signal Add
        | p0 == signal -> onPrevious signal Add
    [p0, _p1, _p2, p3]
        | p3 == signal -> delete p0 $ onNext signal Add
        | p0 == signal -> delete p3 $ onPrevious signal Add
    _ -> pure []
  where
    delete d f = do
        rs <- f
        case rs of
            [] -> pure []
            _ -> pure $ rs <> [Delete d]
    onAny dir get i f = do
        nextIndex <- get configuration i
        pure $ case nextIndex of
            Nothing -> []
            Just j -> [f dir j]
    onNext = onAny After next
    onPrevious = onAny Before previous

-- | Render a 'Change' to the scrolling table as a series of out-of-band
-- updates.
-- 'Delete' is actually a pure operation as it does not require any
-- effect to be performed, but 'Add' does require retrieving the new data
renderChange :: Monad m => Configuration m index -> Change index -> m (Html ())
renderChange c (Delete i) = pure $ do
    scrollableContainer
        c
        [ id_ $ appendScrollingId c scrollableName
        , hxSwapOob_
            $ "delete:" <> appendIndexAndScrollingId c i (mkId dataNames)
        ]
        mempty
    input_
        [ id_ $ appendIndexAndScrollingId c i stateName
        , hxSwapOob_ "delete"
        ]
renderChange c (Add dir i) = do
    newData <-
        scrollableContainer c [hxSwapOob_ w]
            <$> retrieveContentAtIndex c i
    pure $ do
        newData
        form_ [hxSwapOob_ f] $ renderIndexId c i
  where
    f = "beforeend:" <> appendScrollingId c (mkId stateName)
    w = case dir of
        After -> "beforeend:" <> appendScrollingId c (mkId scrollableName)
        Before -> "afterbegin:" <> appendScrollingId c (mkId scrollableName)

-- | A 'Configuration' is used to configure the scrolling table.
data Configuration m index = Configuration
    { scrollableContainer :: [Attribute] -> Html () -> Html ()
    -- ^ The container that will have scrollable content.
    , scrollableWidget :: [Attribute] -> Html () -> Html ()
    -- ^ The widget that will have a scrollable container.
    , retrieveContent :: index -> [Attribute] -> m (Html ())
    -- ^ Render the rows for the given index.
    , uniqueScrollingId :: Text
    -- ^ A unique identifier for the scrolling table.
    , previous :: index -> m (Maybe index)
    -- ^ Get the previous index if it exists.
    , next :: index -> m (Maybe index)
    -- ^ Get the next index if it exists.
    , start :: m (Maybe index)
    -- ^ The initial index.
    , renderIndex :: index -> Text
    -- ^ Render an index as a 'Text' to be used in queries
    , renderIdOfIndex :: index -> Text
    -- ^ Render an index as a 'Text' to be used in the tag ids
    , updateURL :: index -> Text
    -- ^ The URL to use to update the data for the given index. It has to be
    -- a POST request to permit th interaction state to thread through as body.
    , presentFieldName :: Text
    -- ^ The name of the field to use to witness the presence of an index.
    , controlSelector :: Text
    -- ^ An out of band tag that contains a control state to thread through.
    -- Be careful for name clashes with other controls.
    }

-- | A template used to indicate scrolling state fields.
stateName :: Text
stateName = "state"

-- | The name of the table tag.
scrollableName :: Text
scrollableName = "table"

-- | A name template for the data rendered at each index.
dataNames :: Text
dataNames = "data"

-- | A name template for the whole scrolling widget.
scrollingName :: Text
scrollingName = "scrolling"

-- | A helper to create an HTML identifier from it's name.
mkId :: Text -> Text
mkId x = "#" <> x

-- | Append a scrolling identifier to a text.
appendScrollingId :: Configuration m index -> Text -> Text
appendScrollingId Configuration{uniqueScrollingId} =
    (<> ("-" <> uniqueScrollingId))

-- | Append an index to a text.
appendIndex :: Configuration m index -> index -> Text -> Text
appendIndex Configuration{renderIdOfIndex} j = (<> ("-" <> renderIdOfIndex j))

-- | Append an index and a scrolling identifier to a text.
appendIndexAndScrollingId :: Configuration m index -> index -> Text -> Text
appendIndexAndScrollingId c j = appendScrollingId c . appendIndex c j

-- | Add a no-swap attribute to a list of attributes.
dontSwap :: [Attribute] -> [Attribute]
dontSwap = (:) $ hxSwap_ "none"

-- | Add a trigger on intersect attribute to a list of attributes.
triggerIntersect :: [Attribute] -> [Attribute]
triggerIntersect = (:) $ hxTrigger_ "intersect consume"

-- | Add a inclusion tags to a list of attributes.
includeStates :: Configuration m index -> [Attribute] -> [Attribute]
includeStates c =
    (:)
        $ hxInclude_
        $ scrollingState <> "," <> controlState
  where
    scrollingState = mkId $ appendScrollingId c stateName
    controlState = controlSelector c

-- | Add a tbody data identifier to a list of attributes.
indexDataId :: Configuration m index -> index -> [Attribute] -> [Attribute]
indexDataId c j =
    (:)
        $ id_
        $ appendIndexAndScrollingId c j dataNames

-- | Render a page identifier.
renderIndexId :: Configuration m index -> index -> Html ()
renderIndexId c j =
    input_
        [ id_ $ appendIndexAndScrollingId c j stateName
        , hidden_ ""
        , name_ $ presentFieldName c
        , value_ $ renderIndex c j
        ]

-- | Add the htmx post attribute to a list of attributes.
postToUpdate :: Configuration m index -> index -> [Attribute] -> [Attribute]
postToUpdate c j = (:) $ hxPost_ $ updateURL c j

-- | Create a list of attributes for an index tag that will
-- * Fire a POST request to update the scrolling view.
-- * Include the scrolling state and the control state.
-- * Trigger on intersect with the viewport.
-- * Have a data identifier mapped from the index.
-- * Not swap itself.
updaterAttributes :: Configuration m index -> index -> [Attribute]
updaterAttributes c j =
    dontSwap
        . triggerIntersect
        . includeStates c
        . postToUpdate c j
        . indexDataId c j
        $ []

-- | Retrieve and render the page for the given index.
retrieveContentAtIndex
    :: Configuration m index
    -> index
    -> m (Html ())
retrieveContentAtIndex c j = retrieveContent c j (updaterAttributes c j)

-- | Set up the scrolling widget.
setup
    :: Monad m
    => Configuration m index
    -> m ([Attribute] -> Html ())
setup c = do
    mzero <- start c
    case mzero of
        Nothing -> pure mempty
        Just zero -> do
            zeroContent <- retrieveContentAtIndex c zero
            pure $ \attrs -> do
                div_
                    ([id_ $ scrollingName <> "-" <> uniqueScrollingId c] <> attrs)
                    $ do
                        form_
                            [id_ $ appendScrollingId c stateName]
                            $ renderIndexId c zero
                        scrollableWidget
                            c
                            [id_ $ appendScrollingId c scrollableName]
                            zeroContent

-- | A 'Scrolling' is a widget that can be scrolled in constant HTML space.
data Scrolling m index = Scrolling
    { widget :: [Attribute] -> Html ()
    -- ^ The widget that contains the scrolling table.
    , scroll :: Set index -> index -> m (Html ())
    -- ^ Scroll the table to the given index, given the set of indices that
    -- are currently present in the scrolling table.
    }

-- | Create a new scrolling widget.
newScrolling
    :: (Eq index, Monad m)
    => Configuration m index
    -> m (Scrolling m index)
newScrolling c = do
    w <- setup c
    pure
        $ Scrolling
            { widget = w
            , scroll = \state focus -> do
                cs <- changes c state focus
                fmap mconcat $ forM cs $ renderChange c
            }
