{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( inspectTx
    , CurrentEraResolvedTx
    , InspectTx (..)
    , transactionBalance
    ) where

import Prelude

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
    ( Coin (..)
    , Conway
    , TxIx
    , Value (..)
    , getCompactAddr
    , getInputs
    , getValue
    , mkEraTxOut
    , pattern TxIn
    )
import Control.Lens
    ( Field2 (_2)
    , Field3 (_3)
    , to
    , (^.)
    )
import Data.Foldable
    ( Foldable (..)
    , fold
    )
import Data.Monoid
    ( Sum (..)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Inspect the inputs and outputs of a transaction.
data InspectTx = InspectTx
    { ourInputs :: [(TxId, TxIx, Coin)]
    -- ^ Our inputs.
    , otherInputs :: [(TxId, TxIx)]
    -- ^ Other inputs, there shouldn't be any.
    , change :: [(Address, Coin)]
    -- ^ Change outputs.
    , ourOutputs :: [(Address, Customer, Coin)]
    -- ^ Our outputs. The customer is the owner of the address. There could be
    -- reasons the user wants to move funds among customer addresses.
    , otherOutputs :: [(Address, Coin)]
    -- ^ Other outputs. This is regular money leaving the wallet.
    , fee :: Coin
    }
    deriving (Eq, Show)

-- | Calculate the output balance of a transaction, which is the sum of the
-- values of our inputs minus the sum of the values of the change outputs and
-- minus the outputs to our customers.
transactionBalance :: InspectTx -> Integer
transactionBalance InspectTx{..} = getSum $
    (ourInputs ^. traverse . _3 . mkSum)
        - (change ^. traverse . _2 . mkSum)
        - (ourOutputs ^. traverse . _3 . mkSum)
    where
        mkSum = to (Sum . unCoin)

-- | Inspect a transaction where inputs have been resolved to our UTxO.
inspectTx :: WalletState -> CurrentEraResolvedTx -> InspectTx
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
