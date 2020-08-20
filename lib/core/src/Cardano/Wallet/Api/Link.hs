{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Type-safe endpoint accessors for the wallet API. Under normal circumstances,
-- one would prefer to use 'WalletClient' from 'Cardano.Wallet.Api.Client' and
-- not to bother with endpoints at all.
--
-- Yet, in some cases (like in black-box testing), one could want to purposely
-- send malformed requests to specific endpoints. Thus, this module facilitates
-- the construction of valid endpoints that'd be accepted by the server, and for
-- which, users are free to send all sort of data as payload, valid or invalid.
--
-- This module is meant to be used via qualified imports and with
-- type-applications since all exposed functions are type ambiguous in a
-- variable @style@ of type 'WalletStyle'.
--
-- @import qualified Cardano.Wallet.Api.Link as Link@
--
-- For examples:
--
-- >>> Link.deleteWallet @'Shelley myWallet
-- ( "DELETE", "/v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
-- >>> Link.getWallet @('Byron 'Icarus) myWallet
-- ( "GET", "/v2/byron-wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )

module Cardano.Wallet.Api.Link
    ( -- * Wallets
      deleteWallet
    , getWallet
    , listWallets
    , postWallet
    , putWallet
    , putWalletPassphrase
    , getUTxOsStatistics
    , getMigrationInfo
    , migrateWallet

      -- * Addresses
    , postRandomAddress
    , putRandomAddresses
    , listAddresses
    , listAddresses'

      -- * CoinSelections
    , selectCoins

      -- * Transactions
    , createTransaction
    , listTransactions
    , listTransactions'
    , getTransactionFee
    , deleteTransaction
    , getTransaction

      -- * StakePools
    , listStakePools
    , listJormungandrStakePools
    , joinStakePool
    , quitStakePool
    , getDelegationFee

      -- * Network
    , getNetworkInfo
    , getNetworkParams
    , getNetworkClock
    , getNetworkClock'

      -- * Proxy
    , postExternalTransaction

    , PostWallet
    , Discriminate
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiPoolId (..)
    , ApiT (..)
    , ApiTxId (ApiTxId)
    , Iso8601Time
    , MinWithdrawal (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( AddressState, Coin (..), Hash, PoolId, SortOrder, WalletId (..) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Servant.API
    ( (:>)
    , Capture
    , Header'
    , IsElem
    , NoContentVerb
    , QueryFlag
    , QueryParam
    , ReflectMethod (..)
    , ReqBody
    , Stream
    , Verb
    )
import Servant.Links
    ( HasLink (..), safeLink' )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api as Api

--
-- Wallets
--

-- NOTE
-- Type-class is necessary here to type-check 'IsElem endpoint Api' below.
class PostWallet (style :: WalletStyle) where
    postWallet :: (Method, Text)

instance PostWallet 'Shelley where
    postWallet = endpoint @Api.PostWallet id

instance PostWallet 'Byron where
    postWallet = endpoint @Api.PostByronWallet id

deleteWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
deleteWallet w = discriminate @style
    (endpoint @Api.DeleteWallet (wid &))
    (endpoint @Api.DeleteByronWallet (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

getWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getWallet w = discriminate @style
    (endpoint @Api.GetWallet (wid &))
    (endpoint @Api.GetByronWallet (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

getUTxOsStatistics
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getUTxOsStatistics w = discriminate @style
    (endpoint @Api.GetUTxOsStatistics (wid &))
    (endpoint @Api.GetByronUTxOsStatistics (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

listWallets
    :: forall (style :: WalletStyle).
        ( Discriminate style
        )
    => (Method, Text)
listWallets = discriminate @style
    (endpoint @Api.ListWallets id)
    (endpoint @Api.ListByronWallets id)

putWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putWallet w = discriminate @style
    (endpoint @Api.PutWallet (wid &))
    (endpoint @Api.PutByronWallet (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

putWalletPassphrase
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putWalletPassphrase w = discriminate @style
    (endpoint @Api.PutWalletPassphrase (wid &))
    (endpoint @Api.PutByronWalletPassphrase (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

migrateWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
migrateWallet w = discriminate @style
    (endpoint @(Api.MigrateShelleyWallet Net) (wid &))
    (endpoint @(Api.MigrateByronWallet Net) (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

getMigrationInfo
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getMigrationInfo w = discriminate @style
    (endpoint @Api.GetShelleyWalletMigrationInfo (wid &))
    (endpoint @Api.GetByronWalletMigrationInfo (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Addresses
--

postRandomAddress
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
postRandomAddress w =
    endpoint @(Api.PostByronAddress Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

putRandomAddresses
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putRandomAddresses w =
    endpoint @(Api.PutByronAddresses Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

listAddresses
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
listAddresses w =
    listAddresses' @style w Nothing

listAddresses'
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> Maybe AddressState
    -> (Method, Text)
listAddresses' w mstate = discriminate @style
    (endpoint @(Api.ListAddresses Net) (\mk -> mk wid (ApiT <$> mstate)))
    (endpoint @(Api.ListByronAddresses Net) (\mk -> mk wid (ApiT <$> mstate)))
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Coin Selections
--

selectCoins
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
selectCoins w = discriminate @style
    (endpoint @(Api.SelectCoins Net) (wid &))
    (endpoint @(Api.ByronSelectCoins Net) (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Transactions
--

createTransaction
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
createTransaction w = discriminate @style
    (endpoint @(Api.CreateTransaction Net) (wid &))
    (endpoint @(Api.CreateByronTransaction Net) (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

listTransactions
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
listTransactions w =
    listTransactions' @style w Nothing Nothing Nothing Nothing

listTransactions'
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> Maybe Natural
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe SortOrder
    -> (Method, Text)
listTransactions' w minWithdrawal inf sup order = discriminate @style
    (endpoint @(Api.ListTransactions Net)
        (\mk -> mk wid (MinWithdrawal <$> minWithdrawal) inf sup (ApiT <$> order)))
    (endpoint @(Api.ListByronTransactions Net)
        (\mk -> mk wid inf sup (ApiT <$> order)))
  where
    wid = w ^. typed @(ApiT WalletId)

getTransactionFee
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
getTransactionFee w = discriminate @style
    (endpoint @(Api.PostTransactionFee Net) (wid &))
    (endpoint @(Api.PostByronTransactionFee Net) (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

deleteTransaction
    :: forall (style :: WalletStyle) w t.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        , HasType (ApiT (Hash "Tx")) t
        )
    => w
    -> t
    -> (Method, Text)
deleteTransaction w t = discriminate @style
    (endpoint @Api.DeleteTransaction mkURL)
    (endpoint @Api.DeleteByronTransaction mkURL)
  where
    wid = w ^. typed @(ApiT WalletId)
    tid = ApiTxId (t ^. typed @(ApiT (Hash "Tx")))
    mkURL mk = mk wid tid

getTransaction
    :: forall (style :: WalletStyle) w t.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        , HasType (ApiT (Hash "Tx")) t
        )
    => w
    -> t
    -> (Method, Text)
getTransaction w t = discriminate @style
    (endpoint @(Api.GetTransaction Net) mkURL)
    (endpoint @(Api.GetByronTransaction Net) mkURL)
  where
    wid = w ^. typed @(ApiT WalletId)
    tid = ApiTxId (t ^. typed @(ApiT (Hash "Tx")))
    mkURL mk = mk wid tid

--
-- Stake Pools
--

listStakePools
    :: Maybe Coin
    -> (Method, Text)
listStakePools stake =
    endpoint @(Api.ListStakePools ()) (\mk -> mk (ApiT <$> stake))

-- | Like @listStakePools@ but with out the query parameter for the stake that
-- the user intends to delegate.
listJormungandrStakePools :: (Method, Text)
listJormungandrStakePools =
    endpoint @(Api.ListStakePools ()) (\mk -> mk Nothing)

joinStakePool
    :: forall s w.
        ( HasType (ApiT PoolId) s
        , HasType (ApiT WalletId) w
        )
    => s
    -> w
    -> (Method, Text)
joinStakePool s w =
    endpoint @(Api.JoinStakePool Net) (\mk -> mk sid wid)
  where
    sid = ApiPoolId $ getApiT $ s ^. typed @(ApiT PoolId)
    wid = w ^. typed @(ApiT WalletId)

quitStakePool
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
quitStakePool w =
    endpoint @(Api.QuitStakePool Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

getDelegationFee
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getDelegationFee w =
    endpoint @Api.DelegationFee (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Network Information
--

getNetworkInfo
    :: (Method, Text)
getNetworkInfo =
    endpoint @Api.GetNetworkInformation id

getNetworkParams
    :: (Method, Text)
getNetworkParams =
    endpoint @Api.GetNetworkParameters id

getNetworkClock
    :: (Method, Text)
getNetworkClock =
    endpoint @Api.GetNetworkClock (False &)

getNetworkClock'
    :: Bool -- ^ When 'True', block and force NTP check
    -> (Method, Text)
getNetworkClock' forceNtpCheck =
    endpoint @Api.GetNetworkClock (forceNtpCheck &)


--
-- Proxy
--

postExternalTransaction
    :: (Method, Text)
postExternalTransaction =
    endpoint @Api.PostExternalTransaction id

--
-- Internals
--

-- | A safe endpoint creator. This extracts the endpoint from a given Api type
-- in the form of:
--
-- - A 'Text' string (the URL)
-- - An HTTP 'Method' for calling this endpoint (e.g. GET, POST, ...)
--
-- This function does not type-check if the given endpoint (via type
-- application) is not actually part of the wallet API.
--
-- Note that the 'MkLink endpoint Text' depends on the type of 'endpoint'.
--
-- - For simple endpoints with no path or query parameters we have:
--
--   @MkLink endpoint Text ~ Text@ and therefore, we can simply do
--
--   >>> endpoint @Api.ListWallets id
--   ( "GET", "v2/wallets" )
--
-- - For endpoints with parameters, 'MkLink endpoint Text' will be a function
--   taking as many arguments as there are path or query parameters (path
--   parameters going first).
--
--   >>> endpoint @Api.GetWallet (\(mk :: ApiT WalletId -> Text) -> mk wid)
--   ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
--   Or, simply:
--
--   >>> endpoint @Api.GetWallet (wid &)
--   ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
endpoint
    :: forall endpoint.
        ( HasLink endpoint
        , IsElem endpoint endpoint
        , HasVerb endpoint
        )
    => (MkLink endpoint Text -> Text)
    -> (Method, Text)
endpoint mk =
    ( method (Proxy @endpoint)
    , "v2/" <> mk (safeLink' toUrlPiece (Proxy @endpoint) (Proxy @endpoint))
    )

-- Returns first argument for Shelley style wallet, second argument otherwise.
class Discriminate (style :: WalletStyle) where
    discriminate :: a -> a -> a

instance Discriminate 'Shelley where
    discriminate a _ = a

instance Discriminate 'Byron where
    discriminate _ a = a

-- | Some endpoints are parameterized via a network discriminant in order to
-- correctly encode their end type (for example, 'CreateTransaction n'). Yet, in
-- the context of this module, the network discrimination doesn't matter for it
-- has no influence on the endpoint's value and/or path parameters.
--
-- To ease type signatures, we therefore arbitrarily fix the network to Mainnet.
type Net = 'Mainnet

-- | Extract the method from a given Api
class HasVerb api where
    method :: Proxy api -> Method

instance (ReflectMethod m) => HasVerb (NoContentVerb m) where
    method _ = reflectMethod (Proxy @m)

instance (ReflectMethod m) => HasVerb (Verb m s ct a) where
    method _ = reflectMethod (Proxy @m)

instance (ReflectMethod m) => HasVerb (Stream m s f ct a) where
    method _ = reflectMethod (Proxy @m)

instance HasVerb sub => HasVerb ((path :: Symbol) :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (Capture param t :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (ReqBody a b :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (QueryParam a b :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (QueryFlag sym :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (Header' opts name ty :> sub) where
    method _ = method (Proxy @sub)
