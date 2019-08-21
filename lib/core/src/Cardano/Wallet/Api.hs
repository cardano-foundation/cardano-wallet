{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiT
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , Iso8601Time
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.Types
    ( AddressState, SortOrder, WalletId )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Network.HTTP.Media
    ( (//), (/:) )
import Servant.API
    ( (:<|>)
    , (:>)
    , Accept (..)
    , Capture
    , DeleteNoContent
    , Get
    , JSON
    , NoContent
    , PostAccepted
    , Put
    , PutNoContent
    , QueryParam
    , ReqBody
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
    :<|> GetUTxOsStatistics

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

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type Transactions t =
    CreateTransaction t
    :<|> ListTransactions t
    :<|> PostTransactionFee t
    :<|> PostExternalTransaction t

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
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransaction t]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction t = "externalTransactions"
    :> ReqBody '[JSON] PostExternalTransactionData
    :> PostAccepted '[JSON] (ApiTransaction t)

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
