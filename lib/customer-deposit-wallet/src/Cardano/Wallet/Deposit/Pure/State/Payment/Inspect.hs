{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( inspectTx
    , CurrentEraResolvedTx
    , InspectTx (..)
    ) where

import Cardano.Read.Ledger.Tx.Fee
    ( Fee (..)
    , getEraFee
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Cardano.Wallet.Deposit.Pure.Address
    ( Customer
    , isChangeAddress
    )
import Cardano.Wallet.Deposit.Pure.State.Payment
    ( CurrentEraResolvedTx
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    , addressToCustomer
    )
import Cardano.Wallet.Deposit.Pure.UTxO.Tx
    ( ResolvedTx (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , TxId
    )
import Cardano.Wallet.Read
    ( Coin
    , Conway
    , TxIx
    , Value (..)
    , getCompactAddr
    , getInputs
    , getValue
    , mkEraTxOut
    , pattern TxIn
    )
import Data.Foldable
    ( Foldable (..)
    , fold
    )
import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data InspectTx = InspectTx
    { ourInputs :: [(TxId, TxIx, Coin)]
    , otherInputs :: [(TxId, TxIx)]
    , change :: [(Address, Coin)]
    , ourOutputs :: [(Address, Customer, Coin)]
    , otherOutputs :: [(Address, Coin)]
    , fee :: Coin
    }

inspectTx
    :: WalletState -> CurrentEraResolvedTx -> InspectTx
inspectTx ws (ResolvedTx tx ourUTxO) =
    let
        (ourInputs, otherInputs) = fold $ do
            in'@(TxIn txId txIx) <- Set.toList $ getInputs tx
            case Map.lookup in' ourUTxO of
                Just out -> do
                    let ValueC coins _ = getValue out
                    pure ([(txId, txIx, coins)], [])
                Nothing -> [([], [(txId, txIx)])]
        (change, ourOutputs, otherOutputs) = fold $ do
            out <-
                fmap (mkEraTxOut @Conway . Output)
                    $ toList
                    $ (\(Outputs outs) -> outs)
                    $ getEraOutputs tx
            let addr = getCompactAddr out
                ValueC coins _ = getValue out
                contrib = pure (addr, coins)
            if
                | isChangeAddress (addresses ws) addr -> [(contrib, [], [])]
                | otherwise ->
                    case addressToCustomer addr ws of
                        Just customer -> [([], [(addr, customer, coins)], [])]
                        Nothing -> [([], [], contrib)]
        Fee fee = getEraFee tx
    in
        InspectTx{..}
