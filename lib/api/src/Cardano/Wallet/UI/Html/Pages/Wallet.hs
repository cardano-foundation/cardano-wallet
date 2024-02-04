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
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.UI.API
    ( Visible (..)
    , walletGetLink
    )
import Cardano.Wallet.UI.Html.Htmx
    ( hxExt_
    , hxGet_
    , hxPost_
    , hxTarget_
    , useHtmxExtension
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
    , form_
    , id_
    , input_
    , li_
    , name_
    , placeholder_
    , role_
    , type_
    , ul_
    )

import Cardano.Wallet.Api.Types.WalletAsset
    ( ApiWalletAsset (..)
    )
import qualified Data.Percentage as Percentage
import qualified Data.Text as T

walletNewH :: Html ()
walletNewH = postWalletH

postWalletForm :: Maybe Visible -> Html ()
postWalletForm mv = form_
    [ hxPost_ "/wallet"
    , hxExt_ "json-enc"
    , hxTarget_ "#result"
    ]
    $ do
        input_
            [ class_ "form-control form-control-lg mb-3"
            , visibility
            , name_ "mnemonicSentence"
            , placeholder_ "Mnemonic Sentence"
            ]
        input_
            [ class_ "form-control form-control-lg mb-3"
            , type_ "text"
            , name_ "name"
            , placeholder_ "Wallet Name"
            ]
        input_
            [ class_ "form-control form-control-lg mb-3"
            , visibility
            , name_ "passphrase"
            , placeholder_ "Passphrase"
            ]
        button_
            [ class_ "btn btn-primary btn-block mb-3"
            , type_ "submit"
            ]
            "Create Wallet"
  where
    visibility = type_ $ case mv of
        Just Visible -> "text"
        Just Hidden -> "password"
        Nothing -> "password"

postWalletH :: Html ()
postWalletH = do
    useHtmxExtension "json-enc"
    div_ [class_ "btn-group mb-3", role_ "group"] $ do
        button_
            [ class_ "btn btn-outline-secondary"
            , hxGet_ "/wallet/mnemonic"
            , hxTarget_ "#menmonic"
            ]
            "Hint a mnemonic"
        button_
            [ class_ "btn btn-outline-secondary"
            , hxGet_ "/wallet/mnemonic?clean=true"
            , hxTarget_ "#menmonic"
            ]
            "Clean hinted mnemonic"

    div_ [id_ "menmonic", class_ "mb-3"] ""

    postWalletForm Nothing

    div_
        [ id_ "result"
        , class_ "alert alert-primary"
        , role_ "alert"
        ]
        mempty

mnemonicH :: Maybe [Text] -> Html ()
mnemonicH Nothing = ""
mnemonicH (Just mnemonic) = do
    div_ [class_ "card"] $ do
        div_ [class_ "card-body text-muted small"]
            $ toHtml
            $ T.intercalate " " mnemonic

walletH :: Html ()
walletH = sseH walletGetLink "content" ["refresh", "wallet"]

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

-- data ApiWalletAsset = ApiWalletAsset
--     { policyId :: !(ApiT W.TokenPolicyId)
--     , assetName :: !(ApiT W.AssetName)
--     , quantity :: !Natural
--     }
--     deriving (Data, Eq, Generic, Hashable, Ord, Show, Typeable)
--     deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletAsset
--     deriving anyclass NFData
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
