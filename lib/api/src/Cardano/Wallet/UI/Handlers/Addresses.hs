{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Handlers.Addresses where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet
    ( normalizeDelegationAddress
    )
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
    ( ApiAddressWithPath
    , ApiT (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.UI.Handlers.Lib
    ( alertOnServerError
    , catching
    )
import Cardano.Wallet.UI.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Layer
    ( SessionLayer (..)
    , walletId
    )
import Control.Lens
    ( view
    )
import Control.Monad.Trans
    ( MonadIO (..)
    )
import Servant
    ( Handler
    , runHandler
    )

import qualified Cardano.Wallet.Api.Http.Shelley.Server as Server
import qualified Data.ByteString.Lazy.Char8 as BL

listAddresses
    :: forall n
     . HasSNetworkId n
    => SessionLayer
    -> ApiLayer (SeqState n ShelleyKey)
    -> (BL.ByteString -> RawHtml)
    -> ([ApiAddressWithPath n] -> RawHtml)
    -> Handler RawHtml
listAddresses SessionLayer{..} ctx alert render = catching alert $ do
    liftIO $ do
        mwid <- view walletId <$> state
        case mwid of
            Nothing -> pure $ alert "No wallet selected"
            Just wid -> do
                result <-
                    runHandler
                        $ Server.listAddresses
                            ctx
                            (normalizeDelegationAddress @_ @ShelleyKey @n)
                            (ApiT wid)
                            Nothing
                alertOnServerError alert result $ pure . render
