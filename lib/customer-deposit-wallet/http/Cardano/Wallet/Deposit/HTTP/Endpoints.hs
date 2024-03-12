-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- HTTP endpoints.
--
-- Each HTTP endpoint corresponds to a single function from the
-- "Cardano.Wallet.Deposit.IO" module.
--
module Cardano.Wallet.Deposit.HTTP.Endpoints
    ( listCustomers
    , createAddress
    ) where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , ApiT (..)
    , Customer
    , CustomerList
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Servant.Server
    ( Handler
    )

import qualified Cardano.Wallet.Deposit.IO as Wallet

{-----------------------------------------------------------------------------
    HTTP Wallet endpoints
------------------------------------------------------------------------------}
{- NOTE [ApiT]

The arguments and results of the 'Handler's defined in the present module
are all wrapped in the 'ApiT' type constructor.

The purpose of the 'ApiT' type constructor is to control the
JSON encoding of the wrapped data type.

Specifically, we for any data type @X@ that we define a separate package,
we define instances such as 'FromJSON'@ (ApiT X)@ in the present package.
In other words, defining the JSON encoding for data types is a concern
of the @Cardano.Wallet.Deposit.HTTP.*@ modules;
it's not a concern of the modules where the data type is originally defined.

The JSONS instances for @ApiT@ can be implemented "by hand"
or they can be derived automatically from the definition (using e.g. Generics).
We plan on using FineTypes to generate the JSON encoding automatically.

-}

listCustomers
    :: Wallet.WalletInstance
    -> Handler (ApiT CustomerList)
listCustomers w =
    liftIO $ ApiT <$> Wallet.listCustomers w

createAddress
    :: Wallet.WalletInstance
    -> ApiT Customer
    -> Handler (ApiT Address)
createAddress w a =
    liftIO $ ApiT <$> Wallet.createAddress (unApiT a) w
