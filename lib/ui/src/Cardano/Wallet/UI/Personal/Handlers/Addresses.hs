{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Personal.Handlers.Addresses
    ( listAddresses
    )
where

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
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    )
import Cardano.Wallet.UI.Personal.Handlers.Lib
    ( alertOnServerError
    , catching
    )
import Cardano.Wallet.UI.Personal.Layer
    ( SessionLayer (..)
    , stateL
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
    => SessionLayer (Maybe WalletId)
    -> ApiLayer (SeqState n ShelleyKey)
    -> (BL.ByteString -> RawHtml)
    -> ([ApiAddressWithPath n] -> RawHtml)
    -> Handler RawHtml
listAddresses SessionLayer{..} ctx alert render = catching alert $ do
    liftIO $ do
        mwid <- view stateL <$> state
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
                pure $ alertOnServerError alert render result
