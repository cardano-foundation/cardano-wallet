{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Shelley.Handlers.Wallets where

import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Api
    ( ApiLayer
    )
import Cardano.Wallet.Api.Types
    ( ApiWallet
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer (..)
    , stateL
    )
import Control.Lens
    ( view
    )
import Control.Monad.Catch
    ( MonadCatch (..)
    , SomeException (..)
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Data.Aeson
    ( Value
    , decode
    )
import Data.Time
    ( UTCTime
    )
import Servant
    ( Handler
    , ServerError (..)
    , runHandler
    )
import Prelude

import qualified Cardano.Wallet.Api.Http.Shelley.Server as Server
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

listWallets
    :: HasSNetworkId n
    => SessionLayer (Maybe WalletId)
    -> ApiLayer (SeqState n ShelleyKey)
    -> (Maybe WalletId -> [(ApiWallet, UTCTime)] -> RawHtml)
    -> Handler RawHtml
listWallets SessionLayer{..} ctx render = do
    catch
        do
            ex <-
                liftIO
                    $ runHandler
                    $ Server.listWallets ctx Server.mkShelleyWallet
            case ex of
                Left ServerError{..} ->
                    case decode errBody of
                        Nothing -> pure . RawHtml $ errBody
                        Just je -> pure . RawHtml . Aeson.encodePretty @Value $ je
                Right ls -> do
                    wid <- liftIO $ view stateL <$> state
                    pure $ render wid ls
        do \(SomeException e) -> pure . RawHtml . BL.pack . show $ e
