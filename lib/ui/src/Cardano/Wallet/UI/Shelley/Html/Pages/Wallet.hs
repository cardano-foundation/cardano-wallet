{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Shelley.Html.Pages.Wallet where

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
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxPost_
    , hxTarget_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( ShowTime
    , linkText
    , showPercentage
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( fieldHtml
    , record
    , showThousandDots
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Network
    ( blockReferenceH
    )
import Cardano.Wallet.UI.Shelley.API
    ( sseLink
    , walletDeleteLink
    , walletLink
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
    ( HtmlT
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

walletH :: Monad m => WalletPresent -> HtmlT m  ()
walletH wp = do
    sseH sseLink walletLink "wallet" ["wallet"]
    case wp of
        WalletPresent -> walletActionsH
        WalletAbsent -> mempty

walletActionsH :: Monad m => HtmlT m  ()
walletActionsH = do
    div_ [class_ "mt-3"] $ do
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

walletElementH :: Monad m => ShowTime -> ApiWallet -> HtmlT m  ()
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

renderPassphrase :: Monad m => ShowTime
    -> Maybe ApiWalletPassphraseInfo -> HtmlT m  ()
renderPassphrase _ Nothing = ""
renderPassphrase showTime (Just ApiWalletPassphraseInfo{..}) =
    toHtml $ showTime lastUpdatedAt

renderPoolGap :: Monad m => ApiT AddressPoolGap -> HtmlT m  ()
renderPoolGap = toHtml . show . getAddressPoolGap . getApiT

renderDelegation :: Monad m => ApiWalletDelegation -> HtmlT m  ()
renderDelegation ApiWalletDelegation{..} = record
    $ do
        simpleField "active" $ renderActive active
        fieldHtml [] "next" $ ul_ $ forM_ next $ li_ . renderActive

renderActive :: Monad m => ApiWalletDelegationNext -> HtmlT m  ()
renderActive (ApiWalletDelegationNext status target voting _changesAt) =
    record $ do
        case status of
            NotDelegating -> simpleField "not delegating" (mempty :: Text)
            Delegating -> simpleField "delegating to" $ foldMap (show . getApiT) target
            Voting -> simpleField "voting through" $ foldMap (show . getApiT) voting
            VotingAndDelegating -> do
                simpleField "delegating to" $ foldMap (show . getApiT) target
                simpleField "voting through" $ foldMap (show . getApiT) voting

renderAsset :: Monad m => ApiWalletAsset -> HtmlT m  ()
renderAsset ApiWalletAsset{..} = record $ do
    simpleField "policy id" $ toText $ getApiT policyId
    simpleField "asset name" $ toText $ getApiT assetName
    simpleField "quantity" $ toHtml $ showThousandDots quantity

renderAssets :: Monad m => ApiWalletAssetsBalance -> HtmlT m  ()
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

renderBalance :: Monad m => ApiWalletBalance -> HtmlT m  ()
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
