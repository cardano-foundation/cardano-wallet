{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api where

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiT
    , ApiTransaction
    , ApiWallet
    , PostTransactionData
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Primitive.Types
    ( AddressState, WalletId )
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
    , QueryParam
    , ReqBody
    )

type Api = Addresses :<|> Wallets :<|> Transactions

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses =
    ListAddresses

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddress]

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
    :> Put '[Any] NoContent

{-------------------------------------------------------------------------------
                                  Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type Transactions =
    CreateTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] PostTransactionData
    :> PostAccepted '[JSON] ApiTransaction

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
