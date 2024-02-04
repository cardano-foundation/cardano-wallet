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
    ( showPercentage
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( recordTable
    , row
    , rowHtml
    , showThousandDots
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

walletElementH :: ApiWallet -> Html ()
walletElementH ApiWallet{..} = do
    recordTable $ do
        row [] "name" $ toText $ getApiT name
        row [] "id" $ toText $ getApiT id
        row [] "state" $ renderState state
        row [] "tip" $ blockReferenceH tip
        row [] "pool gap" $ renderPoolGap addressPoolGap
        row [] "balance" $ renderBalance balance
        row [] "assets" $ renderAssets assets
        row [] "delegation" $ renderDelegation delegation
        row [] "passphrase" $ renderPassphrase passphrase

renderPassphrase :: Maybe ApiWalletPassphraseInfo -> Html ()
renderPassphrase Nothing = ""
renderPassphrase (Just ApiWalletPassphraseInfo{..}) =
    toHtml $ show lastUpdatedAt

renderPoolGap :: ApiT AddressPoolGap -> Html ()
renderPoolGap = toHtml . show . getAddressPoolGap . getApiT

renderDelegation :: ApiWalletDelegation -> Html ()
renderDelegation ApiWalletDelegation{..} = recordTable
    $ do
        row [] "active" $ renderActive active
        rowHtml [] "next" $ ul_ $ forM_ next $ li_ . renderActive

renderActive :: ApiWalletDelegationNext -> Html ()
renderActive (ApiWalletDelegationNext status target voting _changesAt) =
    recordTable $ do
        case status of
            NotDelegating -> row [] "not delegating" (mempty :: Text)
            Delegating -> row [] "delegating to" $ foldMap (show . getApiT) target
            Voting -> row [] "voting through" $ foldMap (show . getApiT) voting
            VotingAndDelegating -> do
                row [] "delegating to" $ foldMap (show . getApiT) target
                row [] "voting through" $ foldMap (show . getApiT) voting

renderAsset :: ApiWalletAsset -> Html ()
renderAsset ApiWalletAsset{..} = recordTable $ do
    row [] "policy id" $ toText $ getApiT policyId
    row [] "asset name" $ toText $ getApiT assetName
    row [] "quantity" $ toHtml $ showThousandDots quantity

renderAssets :: ApiWalletAssetsBalance -> Html ()
renderAssets ApiWalletAssetsBalance{..} =
    recordTable $ do
        rowHtml [] "available"
            $ ul_
            $ forM_ (getApiWalletAssets available)
            $ li_
                . renderAsset
        rowHtml [] "total"
            $ ul_
            $ forM_ (getApiWalletAssets total)
            $ li_ . renderAsset

renderBalance :: ApiWalletBalance -> Html ()
renderBalance ApiWalletBalance{..} =
    recordTable $ do
        row [] "available" $ toHtml $ showAmount available
        row [] "total" $ toHtml $ showAmount total
        row [] "reward" $ toHtml $ showAmount reward
  where
    showAmount = toHtml . show . toNatural

renderState :: ApiT SyncProgress -> String
renderState (ApiT (Syncing (Quantity p))) =
    showPercentage
        $ Percentage.toRational p
renderState (ApiT Ready) = "Ready"
renderState (ApiT NotResponding) = "NotResponding"
