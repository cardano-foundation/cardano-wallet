{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiT
    , ApiTransaction
    , ApiWallet
    , PostTransactionData
    , PostTransactionFeeData
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.Types
    ( AddressState, WalletId )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Time.Clock
    ( UTCTime )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Media
    ( (//), (/:) )
import Servant.API
    ( (:<|>)
    , (:>)
    , Accept (..)
    , Capture
    , DeleteNoContent
    , FromHttpApiData (..)
    , Get
    , Header
    , JSON
    , NoContent
    , PostAccepted
    , Put
    , PutNoContent
    , QueryParam
    , ReqBody
    , ToHttpApiData (..)
    )

type Api t = Addresses t :<|> Wallets :<|> Transactions t

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses t =
    ListAddresses t

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses t = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddress t]

{-------------------------------------------------------------------------------
                                  Wallets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Wallets
-------------------------------------------------------------------------------}

type Wallets =
    DeleteWallet
    :<|> GetWallet
    :<|> ListWallets
    :<|> PostWallet
    :<|> PutWallet
    :<|> PutWalletPassphrase

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteWallet
type DeleteWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent '[Any] NoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [ApiWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] WalletPostData
    :> PostAccepted '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWallet
type PutWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWalletPassphrase
type PutWalletPassphrase = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] WalletPutPassphraseData
    :> PutNoContent '[Any] NoContent

{-------------------------------------------------------------------------------
                                  Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type Transactions t =
    CreateTransaction t
    :<|> ListTransactions t
    :<|> PostTransactionFee t

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransaction t = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionData t)
    :> PostAccepted '[JSON] (ApiTransaction t)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransactionFee
type PostTransactionFee t = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> "fees"
    :> ReqBody '[JSON] (PostTransactionFeeData t)
    :> PostAccepted '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listTransaction
type ListTransactions t = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Header "Range" (Iso8601Range "inserted-at")
    :> Get '[JSON] [ApiTransaction t]

{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}

-- | Any media type
data Any

instance Accept Any where
    contentTypes _ = ("*" // "*") :|
        -- We also 'conveniently' accept JSON format
        [ "application" // "json"
        , "application" // "json" /: ("charset", "utf-8")
        ]

-- | A range of dates in ISO-8601 UTC format without symbols. Meant to be
-- rendered as a HTTP 'Header', where the 'name' type-parameter renders as a
-- prefix, e.g.
--
-- name 20190227T160329Z-*
--
-- 'Nothing' ("*") can be used instead of upper and/or lower boundary.
--
-- - `20190227T160329Z-*`: means all transactions after
--    2019-02-27 T 16:03:29Z (including)
-- - `*-20190227T160329Z`: means all transactions before 2019-02-27 T 16:03:29Z
--    (including)
-- - `*-*`: means all transactions
-- - `20190227T000000Z-20200227T000000Z`: means all transaction between
--    2019-02-27 and 2020-02-27, in ascending order.
-- - `20200227T000000Z-20190227T000000Z`: means all transaction between
--  2020-02-27 and 2019-02-27, in descending order.
data Iso8601Range (name :: Symbol)
    = Iso8601Range (Maybe UTCTime) (Maybe UTCTime)
    deriving (Show)

instance FromHttpApiData (Iso8601Range (name :: Symbol)) where
    parseUrlPiece = error "FromHttpApiData Iso8601Range to be implemented"

instance ToHttpApiData (Iso8601Range (name :: Symbol)) where
    toUrlPiece = error "ToHttpApiData Iso8601Range to be implemented"
