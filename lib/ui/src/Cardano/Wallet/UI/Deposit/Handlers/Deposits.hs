module Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositsHandler
    ) where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Slot
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , getValueTransfers
    )
import Cardano.Wallet.UI.Common.Layer
    ( SessionLayer
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceHtml
    )
import Data.Map.Strict
    ( Map
    )
import Servant
    ( Handler
    )

import qualified Data.ByteString.Lazy as BL

depositsHandler
    :: SessionLayer WalletResource
    -> (Map Slot (Map Address ValueTransfer) -> html)
    -> (BL.ByteString -> html)
    -> Handler html

depositsHandler layer render alert =
    catchRunWalletResourceHtml layer alert id $ do
        render <$> getValueTransfers
