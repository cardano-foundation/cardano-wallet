{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Html.Pages.Wallet where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiWallet (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletBalance (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletPassphraseInfo (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( toNatural
    )
import Cardano.Wallet.Api.Types.WalletAsset
    ( ApiWalletAsset (..)
    )
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.UI.API
    ( linkText
    , sseLink
    , walletDeleteLink
    , walletLink
    )
import Cardano.Wallet.UI.Html.Htmx
    ( hxPost_
    , hxTarget_
    )
import Cardano.Wallet.UI.Html.Lib
    ( ShowTime
    , showPercentage
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( fieldHtml
    , record
    , showThousandDots
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Html.Pages.Network
    ( blockReferenceH
    )
import Control.Monad
    ( forM_
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Html
    , ToHtml (toHtml)
    , button_
    , class_
    , div_
    , id_
    , li_
    , ul_
    )

import qualified Data.Percentage as Percentage

data WalletPresent = WalletPresent | WalletAbsent

walletH :: WalletPresent -> Html ()
walletH wp = do
    sseH sseLink walletLink "wallet" ["refresh", "wallet"]
    case wp of
        WalletPresent -> walletActionsH
        WalletAbsent -> mempty

walletActionsH :: Html ()
walletActionsH = do
    div_ [class_ "mt-3"] $ do
        -- div_ [class_ "btn-group mb-3", role_ "group"] $ do
        button_
                [ class_ "btn btn-danger"
                , hxPost_ (linkText walletDeleteLink)
                , hxTarget_ "#actions"
                ]
                "Forget this wallet"
        div_
            [ id_ "actions"
            ]
            mempty
        mempty

        -- div_ [class_ "alert alert-danger"] $ do
        --     div_ [class_ "alert-heading"] "Warning!"
        --     "This action is irreversible. All data associated with this wallet will be lost."
        -- div_ [class_ "mt-3"] $ do
        --     div_ [class_ "btn btn-danger"]
        --         "Delete Wallet"

walletElementH :: ShowTime -> ApiWallet -> Html ()
walletElementH showTime ApiWallet{..} = do
    record $ do
        simpleField "name" $ toText $ getApiT name
        simpleField "id" $ toText $ getApiT id
        simpleField "state" $ renderState state
        simpleField "tip" $ blockReferenceH showTime tip
        simpleField "pool gap" $ renderPoolGap addressPoolGap
        simpleField "balance" $ renderBalance balance
        simpleField "assets" $ renderAssets assets
        simpleField "delegation" $ renderDelegation delegation
        simpleField "passphrase" $ renderPassphrase showTime passphrase

renderPassphrase :: ShowTime -> Maybe ApiWalletPassphraseInfo -> Html ()
renderPassphrase _ Nothing = ""
renderPassphrase showTime (Just ApiWalletPassphraseInfo{..}) =
    toHtml $ showTime lastUpdatedAt

renderPoolGap :: ApiT AddressPoolGap -> Html ()
renderPoolGap = toHtml . show . getAddressPoolGap . getApiT

renderDelegation :: ApiWalletDelegation -> Html ()
renderDelegation ApiWalletDelegation{..} = record
    $ do
        simpleField "active" $ renderActive active
        fieldHtml [] "next" $ ul_ $ forM_ next $ li_ . renderActive

renderActive :: ApiWalletDelegationNext -> Html ()
renderActive (ApiWalletDelegationNext status target voting _changesAt) =
    record $ do
        case status of
            NotDelegating -> simpleField "not delegating" (mempty :: Text)
            Delegating -> simpleField "delegating to" $ foldMap (show . getApiT) target
            Voting -> simpleField "voting through" $ foldMap (show . getApiT) voting
            VotingAndDelegating -> do
                simpleField "delegating to" $ foldMap (show . getApiT) target
                simpleField "voting through" $ foldMap (show . getApiT) voting

renderAsset :: ApiWalletAsset -> Html ()
renderAsset ApiWalletAsset{..} = record $ do
    simpleField "policy id" $ toText $ getApiT policyId
    simpleField "asset name" $ toText $ getApiT assetName
    simpleField "quantity" $ toHtml $ showThousandDots quantity

renderAssets :: ApiWalletAssetsBalance -> Html ()
renderAssets ApiWalletAssetsBalance{..} =
    record $ do
        fieldHtml [] "available"
            $ ul_
            $ forM_ (getApiWalletAssets available)
            $ li_
                . renderAsset
        fieldHtml [] "total"
            $ ul_
            $ forM_ (getApiWalletAssets total)
            $ li_ . renderAsset

renderBalance :: ApiWalletBalance -> Html ()
renderBalance ApiWalletBalance{..} =
    record $ do
        simpleField "available" $ toHtml $ showAmount available
        simpleField "total" $ toHtml $ showAmount total
        simpleField "reward" $ toHtml $ showAmount reward
  where
    showAmount = toHtml . show . toNatural

renderState :: ApiT SyncProgress -> String
renderState (ApiT (Syncing (Quantity p))) =
    showPercentage
        $ Percentage.toRational p
renderState (ApiT Ready) = "Ready"
renderState (ApiT NotResponding) = "NotResponding"
