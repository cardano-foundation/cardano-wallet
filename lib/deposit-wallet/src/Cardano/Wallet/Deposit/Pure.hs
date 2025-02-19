{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Deposit.Pure
    ( -- * Types
      WalletState
    , DeltaWalletState
    , WalletPublicIdentity (..)

      -- * Creation
    , Credentials (..)
    , fromCredentialsAndGenesis

      -- * Operations

      -- ** Mapping between customers and addresses
    , Customer
    , listCustomers
    , addressToCustomer
    , deriveAddress
    , knownCustomer
    , knownCustomerAddress
    , isCustomerAddress
    , fromRawCustomer
    , customerAddress
    , trackedCustomers
    , walletXPub

      -- ** Reading from the blockchain
    , Word31
    , getWalletTip
    , availableBalance
    , availableUTxO
    , rollForwardMany
    , rollForwardOne
    , rollBackward
    , ValueTransfer (..)
    , getTxHistoryByCustomer
    , getTxHistoryByTime
    , getEraSlotOfBlock
    , getCustomerDeposits
    , getAllDeposits
    , networkTag

      -- ** Writing to the blockchain
    , ErrCreatePayment (..)
    , createPayment
    , resolveCurrentEraTx
    , CurrentEraResolvedTx
    , BIP32Path (..)
    , DerivationType (..)
    , ResolvedTx (..)
    , canSign
    , CanSign (..)
    , getBIP32PathsForOwnedInputs
    , Passphrase
    , signTx
    , addTxSubmission
    , listTxsInSubmission
    , inspectTx
    , InspectTx (..)
    ) where

import Cardano.Wallet.Address.BIP32
    ( BIP32Path (..)
    , DerivationType (..)
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( CanSign (..)
    , Credentials (..)
    , WalletPublicIdentity (..)
    , canSign
    , fromCredentialsAndGenesis
    )
import Cardano.Wallet.Deposit.Pure.State.Payment
    ( CurrentEraResolvedTx
    , ErrCreatePayment (..)
    , createPayment
    , resolveCurrentEraTx
    )
import Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( InspectTx (..)
    , inspectTx
    )
import Cardano.Wallet.Deposit.Pure.State.Rolling
    ( rollBackward
    , rollForwardMany
    , rollForwardOne
    )
import Cardano.Wallet.Deposit.Pure.State.Signing
    ( Passphrase
    , getBIP32PathsForOwnedInputs
    , signTx
    )
import Cardano.Wallet.Deposit.Pure.State.Submissions
    ( addTxSubmission
    , availableBalance
    , availableUTxO
    , listTxsInSubmission
    )
import Cardano.Wallet.Deposit.Pure.State.TxHistory
    ( getAllDeposits
    , getCustomerDeposits
    , getTxHistoryByCustomer
    , getTxHistoryByTime
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( Customer
    , DeltaWalletState
    , WalletState
    , addressToCustomer
    , customerAddress
    , deriveAddress
    , fromRawCustomer
    , getWalletTip
    , isCustomerAddress
    , knownCustomer
    , knownCustomerAddress
    , listCustomers
    , networkTag
    , trackedCustomers
    , walletXPub
    )
import Cardano.Wallet.Deposit.Pure.UTxO.Tx
    ( ResolvedTx (..)
    )
import Cardano.Wallet.Deposit.Pure.UTxO.ValueTransfer
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( getEraSlotOfBlock
    )
import Data.Word.Odd
    ( Word31
    )
