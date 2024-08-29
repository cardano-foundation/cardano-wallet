{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Server where

import Prelude

import Cardano.Wallet.Api.Http.Server.Handlers.NetworkInformation
    ( getNetworkInformation
    )
import Cardano.Wallet.Api.Types
    ( ApiWalletMode (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , SNetworkId
    , networkIdVal
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.UI.Common.Handlers.Settings
    ( toggleSSE
    )
import Cardano.Wallet.UI.Common.Handlers.SSE
    ( sse
    )
import Cardano.Wallet.UI.Common.Handlers.State
    ( getState
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    , rogerH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Network
    ( networkInfoH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Settings
    ( settingsStateH
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    , sessioning
    , withSession
    , withSessionRead
    )
import Cardano.Wallet.UI.Deposit.API
    ( UI
    , settingsSseToggleLink
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Page
    ( Page (..)
    , page
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Functor
    ( ($>)
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    )
import Paths_cardano_wallet_ui
    ( getDataFileName
    )
import Servant
    ( Handler
    , Server
    , (:<|>) (..)
    )

import qualified Cardano.Read.Ledger.Block.Block as Read
import qualified Data.ByteString.Lazy as BL

pageHandler
    :: UILayer ()
    -> PageConfig
    -> Page
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
pageHandler uiLayer config x =
    withSessionLayer uiLayer $ \_session -> do
        pure $ page config x

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

serveUI
    :: forall n
     . HasSNetworkId n
    => UILayer ()
    -> PageConfig
    -> SNetworkId n
    -> NetworkLayer IO Read.ConsensusBlock
    -> BlockchainSource
    -> Server UI
serveUI ul config _ nl  bs =
    ph About
        :<|> ph About
        :<|> ph Network
        :<|> ph Settings
        :<|> sessioning (renderHtml . networkInfoH showTime <$> getNetworkInformation nid nl mode)
        :<|> wsl (\l -> getState l (renderHtml . settingsStateH settingsSseToggleLink))
        :<|> wsl (\l -> toggleSSE l $> RawHtml "")
        :<|> withSessionLayerRead (sse . sseConfig)
        :<|> serveFavicon
  where
    ph = pageHandler ul config
    _ok _ = renderHtml . rogerH @Text $ "ok"
    _alert = renderHtml . alertH
    nid = networkIdVal (sNetworkId @n)
    mode = case bs of
        NodeSource{} -> Node
    _ = networkInfoH
    wsl = withSessionLayer ul
    withSessionLayerRead :: (SessionLayer () -> Handler a) -> Maybe RequestCookies -> Handler a
    withSessionLayerRead f = withSessionRead $ \k -> do
        s <- liftIO $ sessions ul k
        f s

withSessionLayer :: UILayer () -> (SessionLayer () -> Handler a) -> Maybe RequestCookies -> Handler (CookieResponse a)
withSessionLayer ulayer f = withSession $ \k -> do
    s <- liftIO $ sessions ulayer k
    f s

serveFavicon :: Handler BL.ByteString
serveFavicon = do
    file <- liftIO $ getDataFileName "data/images/icon.png"
    liftIO $ BL.readFile file
